# =============================================================================
# Consolidated R script for bootstrap analysis and visualization of daily SR data
# =============================================================================
# This script combines the separate code fragments into one structured workflow:
# 1) load packages and define shared helpers;
# 2) read and prepare input datasets;
# 3) run bootstrap-based resampling analyses;
# 4) calculate summary metrics (NSign, dSRmean, NRMSE, CV);
# 5) fit segmented trends for NRMSE relationships;
# 6) create figures for emission, temperature, and difference from daily mean.
#
# Notes:
# - Duplicated package imports and repeated preprocessing blocks were removed.
# - Legacy exploratory code was not copied unless it contributed to the main workflow.
# - Some objects/files referenced in the original scripts (for example, Ring_comb.xlsx,
#   pval_*.csv, Daily_bootstrap_result.xlsx) are assumed to exist in the working directory.
# - A few original fragments contained inconsistencies or incomplete lines; they are marked
#   with TODO comments so another analyst can validate them before running end-to-end.
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Packages
# -----------------------------------------------------------------------------
library(readxl)
library(writexl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)
library(hydroGOF)
library(segmented)
library(paletteer)
library(patchwork)

options(dplyr.summarise.inform = FALSE)

# -----------------------------------------------------------------------------
# 1. Shared helpers and labels
# -----------------------------------------------------------------------------

# Reorder hours so the daily cycle starts at 10 and ends at 9 of the next day.
hour_levels <- c(
  "10","11","12","13","14","15","16","17","18",
  "19","20","21","22","23","24","1","2","3","4",
  "5","6","7","8","9"
)

# Axis labels for TimeOrder values 1..24.
hour_axis_labels <- c(
  "1" = "10", "2" = "11", "3" = "12", "4" = "13", "5" = "14", "6" = "15",
  "7" = "16", "8" = "17", "9" = "18", "10" = "19", "11" = "20", "12" = "21",
  "13" = "22", "14" = "23", "15" = "24", "16" = "1", "17" = "2", "18" = "3",
  "19" = "4", "20" = "5", "21" = "6", "22" = "7", "23" = "8", "24" = "9"
)

# Labels for forest and grassland campaigns.
plot_time_labels <- c(
  "1" = "Forest1_Jun'14", "2" = "Forest1_Aug'14", "3" = "Forest1_Jun'15", "4" = "Forest1_Oct'15",
  "5" = "Forest2_Jul'18", "6" = "Forest2_Nov'18", "7" = "Forest3_Jul'18", "8" = "Forest3_Oct'18",
  "9" = "Grassland1_Jun'14", "10" = "Grassland1_Aug'14", "11" = "Grassland1_May'15", "12" = "Grassland1_Oct'15",
  "13" = "Grassland2_Jul'18", "14" = "Grassland2_Nov'18", "15" = "Grassland3_Jul'18", "16" = "Grassland3_Oct'18"
)

# Labels for PlTID values used in difference plots.
pltid_forest_labels <- c(
  "1_1" = "Forest1_Jun'14", "1_2" = "Forest1_Aug'14", "1_3" = "Forest1_Jun'15", "1_4" = "Forest1_Oct'15",
  "2_1" = "Forest2_Jul'18", "2_2" = "Forest2_Nov'18", "3_1" = "Forest3_Jul'18", "3_2" = "Forest3_Oct'18"
)

pltid_grass_labels <- c(
  "1_1" = "Grassland1_Jun'14", "1_2" = "Grassland1_Aug'14", "1_3" = "Grassland1_May'15", "1_4" = "Grassland1_Oct'15",
  "2_1" = "Grassland2_Jul'18", "2_2" = "Grassland2_Nov'18", "3_1" = "Grassland3_Jul'18", "3_2" = "Grassland3_Oct'18"
)

# Helper: calculate standard NRMSE in percent.
calc_nrmse <- function(obs, sim) {
  (sqrt(mean((obs - sim)^2, na.rm = TRUE)) / mean(obs, na.rm = TRUE)) * 100
}

# Helper: safe segmented fit with two breakpoints.
# If the segmented model fails, the function returns NULL instead of stopping the script.
fit_segmented_curve <- function(data, x_var, y_var, x_min, x_max, group_value = NA, group_name = NULL) {
  fit_linear <- lm(reformulate(x_var, y_var), data = data)
  fit_segmented <- tryCatch(segmented(fit_linear, npsi = 2), error = function(e) NULL)

  if (is.null(fit_segmented)) return(NULL)

  break_points <- fit_segmented$psi[, 2]
  new_x <- c(x_min, break_points, x_max)
  pred_df <- setNames(data.frame(new_x), x_var)
  new_y <- predict(fit_segmented, pred_df)

  out <- data.frame(new_x = new_x, new_y = new_y)
  names(out) <- c(x_var, y_var)

  if (!is.null(group_name)) {
    out[[group_name]] <- group_value
  }

  out
}

# -----------------------------------------------------------------------------
# 2. Main data import and preprocessing
# -----------------------------------------------------------------------------

# Read the main daily dataset once and reuse it across all downstream analyses.
data1 <- read_excel("Daily_total.xlsx", sheet = 1, col_names = TRUE)

# Convert categorical fields to factors so plotting and grouping are handled consistently.
data1 <- data1 %>%
  mutate(
    Plot       = factor(Plot),
    Biotop     = factor(Biotop),
    Tbtp       = factor(Tbtp),
    Biodiv     = factor(Biodiv),
    TimeYear   = factor(TimeYear),
    Tour       = factor(Tour),
    PlaceID    = factor(PlaceID),
    PlTID      = factor(PlTID),
    PlD_code   = factor(PlD_code),
    DayPlaceID = factor(DayPlaceID),
    TimeInt    = factor(TimeInt, levels = hour_levels),
    SR         = as.numeric(SR),
    RespgCperm2day = as.numeric(RespgCperm2day),
    Tsoil      = as.numeric(Tsoil),
    TAir       = as.numeric(TAir),
    Hum        = as.numeric(Hum)
  )

# Summarize mean and SD of environmental variables for each DayPlaceID.
datamean <- data1 %>%
  group_by(DayPlaceID) %>%
  summarise(
    ts_mean  = mean(Tsoil, na.rm = TRUE),
    ts_sd    = sd(Tsoil, na.rm = TRUE),
    ta_mean  = mean(TAir, na.rm = TRUE),
    ta_sd    = sd(TAir, na.rm = TRUE),
    hum_mean = mean(Hum, na.rm = TRUE),
    hum_sd   = sd(Hum, na.rm = TRUE)
  )

# -----------------------------------------------------------------------------
# 3. Bootstrap simulation: one random hour per iteration, variable number of rings
# -----------------------------------------------------------------------------

# This section reproduces the main bootstrap logic from the original script.
# For each selected day and each number of rings (1..10), the code:
# 1) samples one hour,
# 2) samples rings independently within each plot,
# 3) calculates mean SR for the sampled hour,
# 4) compares the sampled-hour mean against the observed daily mean,
# 5) stores p-value, daily mean, hourly mean, hour, nRing, and DayPlaceID.
run_daily_bootstrap <- function(data1, days_to_use = c(14, 15), n_iter = 100000) {
  pvaltot <- data.frame(
    NN = numeric(), pvalue = numeric(), DayMean = numeric(), HourMean = numeric(),
    Hour = numeric(), nRing = numeric(), DayPlaceID = numeric()
  )

  data_small <- data1 %>% select(TimeInt, DayPlaceID, RingTot, SR, Plot)
  sr_daily_mean <- data1 %>% group_by(DayPlaceID, Plot) %>% summarise(SR = mean(SR, na.rm = TRUE))

  for (day in days_to_use) {
    datacut <- filter(data_small, DayPlaceID == day)
    sr_meancut <- filter(sr_daily_mean, DayPlaceID == day) %>% mutate(Plot = factor(Plot))

    pval <- data.frame(
      NN = numeric(), pvalue = numeric(), DayMean = numeric(), HourMean = numeric(),
      Hour = numeric(), nRing = numeric(), DayPlaceID = numeric()
    )

    nn <- 0

    for (nring in 1:10) {
      print(c(day, nring))

      for (ncomb in 1:n_iter) {
        hour <- sample(1:24, 1)
        datacuth <- filter(datacut, TimeInt == as.character(hour))

        # Build a sampling frame of selected rings for all three plots.
        combhour <- data.frame(matrix(nrow = nring * 3, ncol = 3))
        colnames(combhour) <- c("Plot", "Ring", "RingTot")

        nnHsub <- 0
        for (plot in 1:3) {
          for (ring in 1:nring) {
            nnHsub <- nnHsub + 1
            combhour[nnHsub, 1] <- plot
            combhour[nnHsub, 2] <- ring
            combhour[nnHsub, 3] <- sample(1:30, 1)
          }
        }

        # Join sampled ring IDs with real SR observations for the selected hour.
        combHour <- left_join(combhour, datacuth, by = "RingTot") %>%
          select(Plot.x, Ring.x, SR) %>%
          rename(Plot = Plot.x, Ring = Ring.x)

        # Average SR within each plot for the simulated hourly sample.
        SRhMean <- combHour %>%
          group_by(Plot) %>%
          summarise(SR = mean(SR, na.rm = TRUE)) %>%
          mutate(DayPlaceID = factor(as.character(day)))

        # Compare the simulated hourly mean against the observed daily mean.
        SR4comp <- full_join(sr_meancut, SRhMean, by = c("DayPlaceID", "Plot", "SR"))

        nn <- nn + 1
        pv <- summary(aov(SR ~ DayPlaceID, SR4comp))[[1]][["Pr(>F)"]][1]

        pval[nn, "NN"] <- nn
        pval[nn, "pvalue"] <- pv
        pval[nn, "DayMean"] <- mean(sr_meancut$SR)
        pval[nn, "HourMean"] <- mean(SRhMean$SR)
        pval[nn, "Hour"] <- hour
        pval[nn, "nRing"] <- nring
        pval[nn, "DayPlaceID"] <- day
      }
    }

    pvaltot <- bind_rows(pvaltot, pval)
  }

  pvaltot
}

# Example run command.
# Uncomment this line to regenerate bootstrap results from raw daily observations.
# pvaltot <- run_daily_bootstrap(data1, days_to_use = c(14, 15), n_iter = 100000)
# write.csv(pvaltot, "pval_1415.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 4. Metrics from bootstrap output: NSign, dSRmean, NRMSE by day and by hour
# -----------------------------------------------------------------------------

# Read a previously generated bootstrap file.
pval <- read.csv("pval_22022023.csv", header = TRUE, sep = ",") %>% na.omit()

# Mark iterations where the hourly sample is not significantly different from the daily mean.
pval <- pval %>% mutate(
  sign1 = pvalue > 0.05,
  Hour = factor(Hour),
  nRing = factor(nRing)
)

# Summary over the full day for each number of rings.
pvald <- pval %>%
  group_by(nRing) %>%
  summarise(
    NSign = mean(sign1) * 100,
    dSRmean = mean(abs(DayMean - HourMean)),
    nrmse = calc_nrmse(DayMean, HourMean)
  )

# Summary by hour and number of rings.
pvalh <- pval %>%
  group_by(nRing, Hour) %>%
  summarise(
    NSign = mean(sign1) * 100,
    dSRmean = mean(abs(DayMean - HourMean)),
    nrmse = calc_nrmse(DayMean, HourMean)
  )

# -----------------------------------------------------------------------------
# 5. Alternative simulation based on a precomputed p-value table
# -----------------------------------------------------------------------------

# This block estimates how NRMSE changes when a day is reconstructed from a subset
# of hours. The source table is assumed to already contain HourMean and DayMean.
pv <- read.csv("pval_0607.csv", header = TRUE, sep = ",") %>% na.omit()

pvHour1 <- data.frame(
  DayPlaceID = numeric(), nRing = numeric(), nHour = numeric(), nrmse = numeric()
)

for (DPID in 6:7) {
  pv10ring <- filter(pv, nRing %in% c(1, 5, 10), DayPlaceID == DPID)

  for (nHour in 1:24) {
    for (nHourComb in 1:10) {
      # Randomly select nHour distinct hours from the 24-hour cycle.
      pv10ringSS <- filter(pv10ring, Hour %in% sample(1:24, nHour, replace = FALSE))

      # Count how many replicated combinations are available for each hour.
      results10 <- pv10ringSS %>% group_by(nRing, Hour) %>% summarise(count = n_distinct(NN))
      mincount <- min(results10$count)

      # Equalize the number of repetitions across selected hours, then reconstruct
      # comparable pseudo-days by grouping observations with the same ranked NN.
      pv10ringSS$NN <- as.numeric(pv10ringSS$NN)
      pv10ringSSed <- pv10ringSS %>%
        group_by(nRing, Hour) %>%
        sample_n(mincount) %>%
        arrange(NN) %>%
        mutate(rankgr = rank(NN)) %>%
        group_by(nRing, rankgr) %>%
        summarise(
          HourMean = mean(HourMean),
          DayMean = mean(DayMean)
        )

      nrmse_tmp <- pv10ringSSed %>%
        group_by(nRing) %>%
        summarise(nrmse = calc_nrmse(DayMean, HourMean)) %>%
        mutate(DayPlaceID = DPID, nHour = nHour)

      pvHour1 <- bind_rows(pvHour1, nrmse_tmp)
    }

    write.csv(pvHour1, "nrmseVSnHour_xx.csv", row.names = FALSE)
    print(nHour)
  }
}

# Average NRMSE over repeated random hour subsets.
pvHour <- pvHour1 %>%
  mutate(nHour = factor(nHour)) %>%
  group_by(DayPlaceID, nRing, nHour) %>%
  summarise(nrmse = mean(nrmse))

# Example exploratory fit for one day and one ring count.
# TODO: The original script used an object named 'ewq' in lm(); that object was not defined.
# The lines below keep only the part that can be interpreted unambiguously.
nlc <- nls.control(maxiter = 1000, minFactor = 1 / 1112097152)
pvHourc <- filter(pvHour, DayPlaceID == 6, nRing == 10)
model_nls <- nls(nrmse ~ (a + b * as.numeric(nHour)^c), control = nlc, data = pvHourc,
                 start = c(a = 5, b = 3, c = -1))
pvHourc$nrmsenls <- predict(model_nls)

# -----------------------------------------------------------------------------
# 6. Merge bootstrap summaries with metadata from Daily_bootstrap_result.xlsx
# -----------------------------------------------------------------------------

dplid <- read_excel("Daily_bootstrap_result.xlsx", sheet = 3, col_names = TRUE)
pvald_meta <- read_excel("Daily_bootstrap_result.xlsx", sheet = 1, col_names = TRUE)
pvalh_meta <- read_excel("Daily_bootstrap_result.xlsx", sheet = 2, col_names = TRUE)

pvald_meta <- left_join(pvald_meta, dplid, by = "DayPlaceID") %>% mutate(TempCondID = factor(TempCondID))
pvalh_meta <- left_join(pvalh_meta, dplid, by = "DayPlaceID") %>% mutate(TempCondID = factor(TempCondID))

# -----------------------------------------------------------------------------
# 7. Segmented NRMSE trend vs number of points per plot (nRing)
# -----------------------------------------------------------------------------

pvald_mean <- pvald_meta %>%
  group_by(BioTempID, nRing) %>%
  summarise(
    nrmsem = mean(nrmse),
    UCI = mean(nrmse, na.rm = TRUE) + sd(nrmse) / n(),
    LCI = mean(nrmse, na.rm = TRUE) - sd(nrmse) / n()
  ) %>%
  left_join(dplid, by = "BioTempID") %>%
  distinct(BioTempID, nRing, nrmsem, UCI, LCI, BiotypeID, TempCondID) %>%
  rename(nrmse = nrmsem)

pvald_fit <- data.frame()
for (n in 1:4) {
  pvaldcut <- filter(pvald_mean, BioTempID == n)
  fit_tmp <- fit_segmented_curve(
    data = pvaldcut,
    x_var = "nRing",
    y_var = "nrmse",
    x_min = 1,
    x_max = 10,
    group_value = n,
    group_name = "BioTempID"
  )

  if (!is.null(fit_tmp)) pvald_fit <- bind_rows(pvald_fit, fit_tmp)
}

pvald_fit <- pvald_fit %>%
  left_join(dplid, by = "BioTempID") %>%
  distinct(BioTempID, nRing, nrmse, BiotypeID, TempCondID)

# Keep only inner breakpoint positions so they can be marked on the plot.
pvald_fitcut <- filter(pvald_fit, nRing > 2 & nRing < 10)

plot_nrmse_vs_nring <- ggplot(pvald_mean, aes(x = nRing, y = nrmse, color = factor(TempCondID))) +
  geom_point(aes(shape = factor(BiotypeID)), size = 4) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0, alpha = 0.5, linewidth = 1.1) +
  geom_line(data = pvald_fit,
            aes(x = nRing, y = nrmse, color = factor(TempCondID), linetype = factor(BiotypeID)),
            size = 1.2) +
  geom_point(data = pvald_fitcut, aes(size = "Break points"), shape = 4, color = "khaki4", stroke = 2) +
  scale_color_manual("Soil temperature condition", values = c("red", "blue"), labels = c("Warm", "Cold"), guide = guide_legend(order = 1)) +
  scale_linetype_manual("Type of Biotop", labels = c("Forest", "Grassland"), values = c(1, 2), guide = guide_legend(order = 2)) +
  scale_shape_manual("Type of Biotop", labels = c("Forest", "Grassland"), values = c(16, 17), guide = guide_legend(order = 2)) +
  scale_size_manual("", values = rep(3), labels = c("Break points"),
                    guide = guide_legend(override.aes = list(colour = c("khaki4")), order = 3)) +
  labs(x = expression("Number of points per plot"), y = expression("NRMSE, %")) +
  scale_x_continuous(breaks = seq(1, 10, 1), limits = c(1, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  theme_light() +
  theme(legend.position = "bottom")

# -----------------------------------------------------------------------------
# 8. NRMSE trend over time of day
# -----------------------------------------------------------------------------

pal_nring <- paletteer_d("Redmonder::qMSOStd")
plot_nrmse_vs_hour_by_nring <- ggplot(pvalh_meta, aes(x = Hour, y = nrmse, color = factor(nRing))) +
  geom_smooth(se = FALSE) +
  scale_color_manual("Point per plot", values = pal_nring) +
  labs(x = expression("Time of day," ~ h), y = expression("NRMSE, %")) +
  facet_grid(TempCondID ~ Biotype,
             labeller = as_labeller(c("1" = "Warm", "2" = "Cold", "Forest" = "Forest", "Grassland" = "Grassland"))) +
  scale_x_continuous(breaks = seq(1, 24, 1), limits = c(1, 24)) +
  theme_light() +
  theme(strip.background = element_rect(fill = "white"), strip.text = element_text(color = "black"))

pal_day <- paletteer_d("ggsci::springfield_simpsons")
plot_nrmse_vs_hour_by_day <- ggplot(pvalh_meta, aes(x = Hour, y = nrmse, color = factor(DayPlaceID))) +
  geom_smooth(se = FALSE) +
  scale_color_manual(
    "Tour", values = pal_day,
    labels = c("Jun'14", "Aug'14", "Jun'15", "Oct'15", "Jun'18", "Nov'18", "Jun'18", "Oct'18",
               "Jun'14", "Aug'14", "May'15", "Oct'15", "Jun'18", "Nov'18", "Jun'18", "Oct'18")
  ) +
  labs(x = expression("Time of day," ~ h), y = expression("NRMSE, %")) +
  facet_grid(TempCondID ~ Biotype,
             labeller = as_labeller(c("1" = "Warm", "2" = "Cold", "Forest" = "Forest", "Grassland" = "Grassland"))) +
  scale_x_continuous(breaks = seq(1, 24, 1), limits = c(1, 24)) +
  theme_light() +
  theme(strip.background = element_rect(fill = "white"), strip.text = element_text(color = "black"))

# -----------------------------------------------------------------------------
# 9. Segmented NRMSE trend vs number of tours per day (nHour)
# -----------------------------------------------------------------------------

pvnHmean <- read.csv("nrmseVSnHour_meanDay.csv", header = TRUE, sep = ",")

pvHm_mean <- pvnHmean %>%
  group_by(BioTempID, nRing, nHour) %>%
  summarise(
    nrmsem = mean(nrmse),
    UCI = mean(nrmse, na.rm = TRUE) + sd(nrmse) / n(),
    LCI = mean(nrmse, na.rm = TRUE) - sd(nrmse) / n()
  ) %>%
  left_join(dplid, by = "BioTempID") %>%
  distinct(BioTempID, nRing, nHour, nrmsem, UCI, LCI, BiotypeID, TempCondID) %>%
  rename(nrmse = nrmsem)

# In the original script, segmented fitting was clearly intended for nRing == 10.
# The intermediate object 'pvHm.fit1' was referenced before creation, so here we build it explicitly.
pvHm_fit1 <- data.frame()
for (n in 1:4) {
  pvaldcut <- filter(pvHm_mean, BioTempID == n, nRing == 10)
  fit_tmp <- fit_segmented_curve(
    data = pvaldcut,
    x_var = "nHour",
    y_var = "nrmse",
    x_min = 1,
    x_max = 24,
    group_value = n,
    group_name = "BioTempID"
  )

  if (!is.null(fit_tmp)) {
    fit_tmp$nRing <- 10
    pvHm_fit1 <- bind_rows(pvHm_fit1, fit_tmp)
  }
}

write.csv(pvHm_fit1, "nrmseVSnHour_fit.csv", row.names = FALSE)

pvHm_fit1 <- pvHm_fit1 %>%
  left_join(dplid, by = "BioTempID") %>%
  distinct(BioTempID, nRing, nHour, nrmse, BiotypeID, TempCondID)

pvHm_fitcut <- filter(pvHm_fit1, nHour > 2 & nHour < 14)

nRing_labs <- c("1" = "One point per plot", "5" = "Five point per plot", "10" = "Ten point per plot")

# Panels for nRing = 1 and 5.
pvHm_mean1 <- filter(pvHm_mean, nRing %in% c("1", "5", 1, 5))
pvHm_fit11 <- filter(pvHm_fit1, nRing %in% c("1", "5", 1, 5))
pvHm_fitcut1 <- filter(pvHm_fitcut, nRing %in% c("1", "5", 1, 5))

p1 <- ggplot(pvHm_mean1, aes(x = nHour, y = nrmse, color = factor(TempCondID))) +
  geom_point(aes(shape = factor(BiotypeID)), size = 4) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0, alpha = 0.5, linewidth = 1.1) +
  geom_line(data = pvHm_fit11,
            aes(x = nHour, y = nrmse, color = factor(TempCondID), linetype = factor(BiotypeID)),
            size = 1.2) +
  geom_point(data = pvHm_fitcut1, aes(size = "Break points"), shape = 4, color = "khaki4", stroke = 2) +
  scale_color_manual("Soil temperature condition", values = c("red", "blue"), labels = c("Warm", "Cold"), guide = guide_legend(order = 1)) +
  scale_linetype_manual("Type of Biotop", labels = c("Forest", "Grassland"), values = c(1, 2), guide = guide_legend(order = 2)) +
  scale_shape_manual("Type of Biotop", labels = c("Forest", "Grassland"), values = c(16, 17), guide = guide_legend(order = 2)) +
  scale_size_manual("", values = rep(3), labels = c("Break points"),
                    guide = guide_legend(override.aes = list(colour = c("khaki4")), order = 3)) +
  labs(x = expression("Number of tours per day"), y = expression("NRMSE, %")) +
  scale_x_continuous(breaks = seq(1, 24, 1), limits = c(1, 24)) +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 25)) +
  xlab(NULL) +
  facet_wrap(~nRing, nrow = 1, labeller = labeller(nRing = nRing_labs)) +
  theme_light() +
  theme(legend.position = "none", strip.background = element_rect(fill = "white"), strip.text = element_text(color = "black"))

# Panel for nRing = 10.
pvHm_mean2 <- filter(pvHm_mean, nRing %in% c("10", 10))
pvHm_fit12 <- filter(pvHm_fit1, nRing %in% c("10", 10))
pvHm_fitcut2 <- filter(pvHm_fitcut, nRing %in% c("10", 10))

p2 <- ggplot(pvHm_mean2, aes(x = nHour, y = nrmse, color = factor(TempCondID))) +
  geom_point(aes(shape = factor(BiotypeID)), size = 4) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0, alpha = 0.5, linewidth = 1.1) +
  geom_line(data = pvHm_fit12,
            aes(x = nHour, y = nrmse, color = factor(TempCondID), linetype = factor(BiotypeID)),
            size = 1.2) +
  geom_point(data = pvHm_fitcut2, aes(size = "Break points"), shape = 4, color = "khaki4", stroke = 2) +
  scale_color_manual("Soil temperature condition", values = c("red", "blue"), labels = c("Warm", "Cold"), guide = guide_legend(order = 1)) +
  scale_linetype_manual("Type of Biotop", labels = c("Forest", "Grassland"), values = c(1, 2), guide = guide_legend(order = 2)) +
  scale_shape_manual("Type of Biotop", labels = c("Forest", "Grassland"), values = c(16, 17), guide = guide_legend(order = 2)) +
  scale_size_manual("", values = rep(3), labels = c("Break points"),
                    guide = guide_legend(override.aes = list(colour = c("khaki4")), order = 3)) +
  labs(x = expression("Number of tours per day"), y = expression("NRMSE, %")) +
  scale_x_continuous(breaks = seq(1, 24, 1), limits = c(1, 24)) +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 25)) +
  facet_wrap(~nRing, nrow = 1, labeller = labeller(nRing = nRing_labs)) +
  theme_light() +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "white"), strip.text = element_text(color = "black"))

plot_nrmse_vs_nhour <- p1 + p2 &
  plot_layout(design = "AAAAAAA
#######
#BBBBB#", heights = c(3, 1, 3))

# -----------------------------------------------------------------------------
# 10. Difference from daily mean by hour
# -----------------------------------------------------------------------------

diff_df <- read_excel("Difference_Total.xlsx")
nrmse_real <- read_excel("CVmean_total.xlsx", sheet = 1, col_names = TRUE) %>%
  filter(Cvtype == "nrmse")

diff_df <- left_join(diff_df, nrmse_real, by = c("Biotop", "PlTID")) %>%
  mutate(
    PlTID = factor(PlTID),
    Crit = factor(Crit, levels = c("0", "1", "2")),
    TimeInt = factor(TimeInt, levels = hour_levels)
  )

# The original script overwrote forest labels with grassland labels immediately afterwards.
# Here the correct label set is chosen according to the filtered dataset.
df_grass <- filter(diff_df, Biotop == "Grassland")

plot_difference_grassland <- ggplot(df_grass, aes(x = TimeInt, y = Difference, fill = factor(Crit))) +
  geom_col(color = "black") +
  geom_hline(aes(color = "linec", yintercept = 15), linetype = 2) +
  geom_hline(aes(color = "linec", yintercept = -15), linetype = 2) +
  geom_text(data = NULL, hjust = "left", x = 0.5, y = -45, label = expression("NRMSE= "), color = "black") +
  geom_text(hjust = "left", aes(x = 4.5, y = -45, label = round(CV, 2))) +
  scale_fill_manual(
    "Significant differences from daily mean",
    values = c("green", "yellow", "orange"),
    labels = c("No", "By contrast", "By contrast & Student's t-test"),
    guide = guide_legend(order = 1)
  ) +
  scale_color_manual("", values = c("red"), labels = c("Daily mean 15%"), guide = guide_legend(order = 2)) +
  scale_y_continuous(breaks = seq(-55, 55, 10), limits = c(-50, 60)) +
  labs(x = expression("Time of day," ~ h), y = expression("Difference from the daily mean, %")) +
  facet_wrap(~PlTID, ncol = 2, scales = "free_y", labeller = as_labeller(pltid_grass_labels)) +
  theme_light() +
  theme(strip.background = element_rect(fill = "white"), strip.text = element_text(color = "black"), legend.position = "bottom")

# -----------------------------------------------------------------------------
# 11. CV plot by biotope and temperature condition
# -----------------------------------------------------------------------------

cvtot <- read_excel("CVmean_total.xlsx", sheet = 1, col_names = TRUE) %>%
  filter(Cvtype != "nrmse") %>%
  left_join(dplid, by = "DayPlaceID")

plot_cv_box <- ggplot(cvtot, aes(x = factor(BioTempID), y = CV, color = Cvtype)) +
  geom_boxplot() +
  labs(x = expression("Biotop and Soil Temperature conditions"), y = expression("CV, %")) +
  scale_color_manual("CV type", values = c("firebrick1", "purple2"), labels = c("Spatial", "Temporal")) +
  scale_x_discrete(labels = c("1" = "Forest_Warm", "2" = "Forest_Cold", "3" = "Grassland_Warm", "4" = "Grassland_Cold")) +
  theme_light()

# -----------------------------------------------------------------------------
# 12. SR emission through the day for forest sites
# -----------------------------------------------------------------------------

df_forest <- filter(data1, Biotop == "Forest")

resp_mean <- df_forest[, c(11, 17)] %>%
  group_by(DayPlaceID) %>%
  summarise(
    mean_val = mean(SR, na.rm = TRUE),
    sdupp = mean(SR, na.rm = TRUE) + sd(SR, na.rm = TRUE),
    sddwn = mean(SR, na.rm = TRUE) - sd(SR, na.rm = TRUE)
  ) %>%
  mutate(DayPlaceID = as.factor(DayPlaceID))

plot_forest_emission <- ggplot(data = df_forest) +
  geom_hline(aes(yintercept = mean_val, size = "dmean"), resp_mean, color = "black") +
  geom_rect(aes(xmin = 0.5, xmax = 24.5, ymin = sddwn, ymax = sdupp, fill = "MeanSD"),
            resp_mean, color = FALSE, alpha = 0.3) +
  geom_point(aes(x = TimeOrder, y = SR, color = factor(Plot)), alpha = 0.5, size = 1.5) +
  geom_smooth(aes(x = as.numeric(TimeOrder), y = SR, linetype = "mean"), se = TRUE, color = "red", fill = "yellow") +
  scale_x_continuous(breaks = seq(1, 24, 1), limits = c(0.5, 24.5), labels = hour_axis_labels) +
  scale_color_manual(values = c("deepskyblue", "blue", "darkviolet"), labels = c("1", "2", "3"), name = "Plot", guide = guide_legend(order = 1)) +
  scale_fill_manual("", values = c("black"), labels = c("Daily mean standard deviation"), guide = guide_legend(order = 3)) +
  scale_linetype_manual("", values = c(1), labels = c("Hourly mean"), guide = guide_legend(order = 4)) +
  scale_size_manual("", values = c(1), labels = c("Daily mean"), guide = guide_legend(order = 2)) +
  labs(x = expression("Time of day," ~ h), y = expression("Emission," ~ g ~ C-(CO[2]) / m^{2} * day)) +
  facet_wrap(~as.factor(DayPlaceID), ncol = 2, scales = "free_y", labeller = as_labeller(plot_time_labels)) +
  theme_light() +
  theme(strip.background = element_rect(fill = "white"), strip.text = element_text(color = "black"), legend.position = "bottom")

# -----------------------------------------------------------------------------
# 13. Temperature curves through the day for forest sites
# -----------------------------------------------------------------------------

plot_forest_temperature <- ggplot(data = df_forest) +
  geom_smooth(aes(x = as.numeric(TimeOrder), y = Tsoil, color = "red"), se = FALSE, fill = "yellow") +
  geom_smooth(aes(x = as.numeric(TimeOrder), y = TAir, color = "blue"), se = FALSE) +
  scale_x_continuous(breaks = seq(1, 24, 1), limits = c(0.5, 24.5), labels = hour_axis_labels) +
  scale_color_manual(values = c("red", "blue"), labels = c("Soil", "Air"), name = "") +
  labs(x = expression("Time of day," ~ h), y = expression("Temperature," ^{o} ~ "C")) +
  facet_wrap(~as.factor(DayPlaceID), ncol = 2, scales = "free_y", labeller = as_labeller(plot_time_labels)) +
  theme_light() +
  theme(strip.background = element_rect(fill = "white"), strip.text = element_text(color = "black"), legend.position = "bottom")

# -----------------------------------------------------------------------------
# 14. Save figures
# -----------------------------------------------------------------------------

ggsave("nrmseVSnRing_TimeOfDay_Forest.png", plot = plot_forest_emission, width = 25, height = 23, units = "cm", dpi = 300)
ggsave("Temp_forest.png", plot = plot_forest_temperature, width = 25, height = 28, units = "cm", dpi = 300)
ggsave("Difference_Grassland.png", plot = plot_difference_grassland, width = 25, height = 25, units = "cm", dpi = 300)
ggsave("NRMSE_vs_nRing.png", plot = plot_nrmse_vs_nring, width = 24, height = 16, units = "cm", dpi = 300)
ggsave("NRMSE_vs_Hour_by_nRing.png", plot = plot_nrmse_vs_hour_by_nring, width = 24, height = 16, units = "cm", dpi = 300)
ggsave("NRMSE_vs_Hour_by_Day.png", plot = plot_nrmse_vs_hour_by_day, width = 24, height = 16, units = "cm", dpi = 300)
ggsave("CV_boxplot.png", plot = plot_cv_box, width = 20, height = 14, units = "cm", dpi = 300)

# Save the consolidated script itself so it can be reviewed or edited later.
writeLines(readLines(sys.frame(1)$ofile), "output/consolidated_script.R")
