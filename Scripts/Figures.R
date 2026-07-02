## Packages --------------------------------------------------------------------
library(psych)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)

## ---------------------------------------------------------------------------
## 1. Load main dataset and set variable types
## ---------------------------------------------------------------------------

# Read main daily dataset
data1 <- read_excel("Daily_total.xlsx", sheet = 1, col_names = TRUE)

# Convert ID / categorical variables to factors
data1$Plot      <- factor(data1$Plot)
data1$Biotop    <- factor(data1$Biotop)
data1$Tbtp      <- factor(data1$Tbtp)
data1$Biodiv    <- factor(data1$Biodiv)
data1$TimeYear  <- factor(data1$TimeYear)
data1$Tour      <- factor(data1$Tour)
data1$PlaceID   <- factor(data1$PlaceID)
data1$PlTID     <- factor(data1$PlTID)

# Convert numeric variables explicitly to numeric
data1$RespgCperm2day <- as.numeric(data1$RespgCperm2day)
data1$Tsoil          <- as.numeric(data1$Tsoil)
data1$TAir           <- as.numeric(data1$TAir)
data1$Hum            <- as.numeric(data1$Hum)

## ---------------------------------------------------------------------------
## 2. Daily mean and variability of Tsoil / TAir / Hum per DayPlaceID
## ---------------------------------------------------------------------------

datamean <- group_by(data1, DayPlaceID)
datamean <- summarise(
  datamean,
  tsmin  = mean(Tsoil, na.rm = TRUE),
  tsmax  = sd(Tsoil,  na.rm = TRUE),
  tamin  = mean(TAir, na.rm = TRUE),
  tamax  = sd(TAir,   na.rm = TRUE),
  hummin = mean(Hum,  na.rm = TRUE),
  hummax = sd(Hum,    na.rm = TRUE)
)

## ---------------------------------------------------------------------------
## 3. Forest-only subset and diurnal SR plot with daily bands
## ---------------------------------------------------------------------------

# Keep only forest data
df <- filter(data1, Biotop == "Forest")

# Compute mean SR and +/- 1 SD per day (DayPlaceID)
# NOTE: here SR is assumed to be column 17 and DayPlaceID column 11 in data1;
# using numeric indices is fragile; using names would be safer.
resp_mean <- df[, c(11, 17)]
resp_mean <- group_by(resp_mean, DayPlaceID) %>%
  summarise(
    mean_val = mean(SR, na.rm = TRUE),
    sdupp    = mean(SR, na.rm = TRUE) + sd(SR, na.rm = TRUE),
    sddwn    = mean(SR, na.rm = TRUE) - sd(SR, na.rm = TRUE)
  )
resp_mean$DayPlaceID <- as.factor(resp_mean$DayPlaceID)

# Labels for facets (first version: forest tours)
PlTIDnames <- c(
  "1" = "Forest1_Jun'14", "2" = "Forest1_Aug'14",
  "3" = "Forest1_Jun'15", "4" = "Forest1_Oct'15",
  "5" = "Forest2_Jul'18", "6" = "Forest2_Nov'18",
  "7" = "Forest3_Jul'18", "8" = "Forest3_Oct'18"
)

# Then PlTIDnames is overwritten by a grassland version
# (only one of these two will actually be used in facet labelling)
PlTIDnames <- c(
  "9"  = "Grassland1_Jun'14", "10" = "Grassland1_Aug'14",
  "11" = "Grassland1_May'15", "12" = "Grassland1_Oct'15",
  "13" = "Grassland2_Jul'18", "14" = "Grassland2_Nov'18",
  "15" = "Grassland3_Jul'18", "16" = "Grassland3_Oct'18"
)

# Diurnal SR plot by day with:
# - horizontal line: daily mean SR
# - semi-transparent band: mean ± SD
# - points: hourly SR by plot
# - red smooth line: hourly mean pattern
p <-
  ggplot(data = df) +
  geom_hline(
    aes(yintercept = mean_val, size = "dmean"),
    data = resp_mean,
    color = "black"
  ) +
  geom_rect(
    aes(xmin = 0.5, xmax = 24.5, ymin = sddwn, ymax = sdupp, fill = "MeanSD"),
    data = resp_mean,
    color = FALSE,
    alpha = 0.3
  ) +
  geom_point(
    aes(x = TimeOrder, y = SR, color = factor(Plot)),
    alpha = 0.5,
    size = 1.5
  ) +
  geom_smooth(
    aes(x = as.numeric(TimeOrder), y = SR, linetype = "mean"),
    se   = TRUE,
    color = "red",
    fill  = "yellow"
  ) +
  # Custom x-axis labels: convert TimeOrder (1–24) to local time labels 10–9
  scale_x_continuous(
    breaks = seq(1, 24, 1),
    limits = c(0.5, 24.5),
    labels = c(
      "1" = "10", "2" = "11", "3" = "12", "4" = "13", "5" = "14", "6" = "15",
      "7" = "16", "8" = "17", "9" = "18", "10" = "19", "11" = "20", "12" = "21",
      "13" = "22", "14" = "23", "15" = "24", "16" = "1", "17" = "2", "18" = "3",
      "19" = "4", "20" = "5", "21" = "6", "22" = "7", "23" = "8", "24" = "9"
    )
  ) +
  # Plot colours = plots (1–3)
  scale_color_manual(
    values = c("deepskyblue", "blue", "darkviolet"),
    labels = c("1", "2", "3"),
    name   = "Plot",
    guide  = guide_legend(order = 1)
  ) +
  # Fill legend for mean ± SD band
  scale_fill_manual(
    "",
    values = c("black"),
    labels = c("Daily mean standard deviation"),
    guide  = guide_legend(order = 3)
  ) +
  # Linetype legend for smoothed hourly mean
  scale_linetype_manual(
    "",
    values = c(1),
    labels = c("Hourly mean"),
    guide  = guide_legend(order = 4)
  ) +
  # Size legend for daily mean horizontal line
  scale_size_manual(
    "",
    values = c(1),
    labels = c("Daily mean"),
    guide  = guide_legend(order = 2)
  ) +
  labs(
    x = expression("Time of day," ~ h),
    y = expression("Emission," ~ g ~ C-(CO[2]) / m^{2} * day)
  ) +
  # One facet per DayPlaceID, free y-scale, with custom labels
  facet_wrap(
    ~as.factor(DayPlaceID),
    ncol    = 2,
    scales  = "free_y",
    labeller = as_labeller(PlTIDnames)
  ) +
  theme_light() +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text       = element_text(color = "black"),
    legend.position  = "bottom"
  )

# Save the plot as PNG
ggsave(
  "nrmseVSnRing&TimeofdaywithBrakeP.png",
  plot   = p,
  width  = 25,
  height = 23,
  units  = "cm",
  dpi    = 300
)

## ---------------------------------------------------------------------------
## 4. Differences from daily mean by hour (Difference_Total, CVmean_total)
## ---------------------------------------------------------------------------

# Load differences from daily mean and NRMSE for each site/biotope
df <- read_excel("Difference_Total.xlsx")
nrmse_real <- read_excel("CVmean_total.xlsx", sheet = 1, col_names = TRUE)
nrmse_real <- filter(nrmse_real, Cvtype == "nrmse")

# Attach NRMSE info to difference table
df <- left_join(df, nrmse_real, by = c("Biotop", "PlTID"))

df$PlTID <- factor(df$PlTID)
df$Crit  <- factor(df$Crit, levels = c("0", "1", "2"))

# Order TimeInt factor to represent real diurnal order from 10 to 9
df$TimeInt <- factor(
  df$TimeInt,
  levels = c(
    "10", "11", "12", "13", "14", "15", "16", "17", "18",
    "19", "20", "21", "22", "23", "24", "1", "2", "3", "4",
    "5", "6", "7", "8", "9"
  )
)

# Labels for Biotop x time combinations (first: forests, then overwritten by grasslands)
PlTIDnames <- c(
  "1_1" = "Forest1_Jun'14", "1_2" = "Forest1_Aug'14",
  "1_3" = "Forest1_Jun'15", "1_4" = "Forest1_Oct'15",
  "2_1" = "Forest2_Jul'18", "2_2" = "Forest2_Nov'18",
  "3_1" = "Forest3_Jul'18", "3_2" = "Forest3_Oct'18"
)
PlTIDnames <- c(
  "1_1" = "Grassland1_Jun'14", "1_2" = "Grassland1_Aug'14",
  "1_3" = "Grassland1_May'15", "1_4" = "Grassland1_Oct'15",
  "2_1" = "Grassland2_Jul'18", "2_2" = "Grassland2_Nov'18",
  "3_1" = "Grassland3_Jul'18", "3_2" = "Grassland3_Oct'18"
)

# Focus on grassland only for this plot
df1 <- filter(df, Biotop == "Grassland")

# Column plot: % difference from daily mean vs time of day,
# coloured by significance criterion, with ±15% reference lines
p <-
  ggplot(data = df1, aes(x = TimeInt, y = Difference, fill = factor(Crit))) +
  geom_col(color = "black") +
  scale_fill_manual(
    "Significant differences from daily mean",
    values = c("green", "yellow", "orange"),
    labels = c("No", "By contrast", "By contrast & Student's t-test"),
    guide  = guide_legend(order = 1)
  ) +
  scale_y_continuous(
    breaks = seq(-55, 55, 10),
    limits = c(-50, 60)
  ) +
  labs(
    x = expression("Time of day," ~ h),
    y = expression("Difference from the daily mean, %")
  ) +
  # ±15 % horizontal reference lines
  geom_hline(aes(color = "linec", yintercept = 15),  linetype = 2) +
  geom_hline(aes(color = "linec", yintercept = -15), linetype = 2) +
  scale_color_manual(
    "",
    values = c("red"),
    labels = c("Daily mean 15%"),
    guide  = guide_legend(order = 2)
  ) +
  facet_wrap(
    ~PlTID,
    ncol    = 2,
    scales  = "free_y",
    labeller = as_labeller(PlTIDnames)
  ) +
  # Add NRMSE text annotation in each facet
  geom_text(
    data  = NULL,
    hjust = "left",
    x     = 0.5,
    y     = -45,
    label = expression("NRMSE= "),
    color = "black"
  ) +
  geom_text(
    hjust = "left",
    aes(x = 4.5, y = -45, label = round(CV, 2))
  ) +
  theme_light() +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text       = element_text(color = "black"),
    legend.position  = "bottom"
  )

## ---------------------------------------------------------------------------
## 5. CV comparison: spatial vs temporal, by BioTempID
## ---------------------------------------------------------------------------

cvtot <- read_excel("CVmean_total.xlsx", sheet = 1, col_names = TRUE)
cvtot <- filter(cvtot, Cvtype != "nrmse")

# dplid is joined in later for BioTempID labels (loaded further below)
cvtot <- left_join(cvtot, dplid, by = "DayPlaceID")

ggplot(cvtot, aes(x = factor(BioTempID), y = CV, color = Cvtype)) +
  geom_boxplot() +
  labs(
    x = expression("Biotop and Soil Temperature conditions"),
    y = expression("CV, %")
  ) +
  scale_color_manual(
    "CV type",
    values = c("firebrick1", "purple2"),
    labels = c("Spatial", "Temporal")
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Forest_Warm", "2" = "Forest_Cold",
      "3" = "Grassland_Warm", "4" = "Grassland_Cold"
    )
  ) +
  theme_light()

## ---------------------------------------------------------------------------
## 6. Bootstrap results: NRMSE vs nRing (segmented regression)
## ---------------------------------------------------------------------------

dplid <- read_excel("Daily_bootstrap_result.xlsx", sheet = 3, col_names = TRUE)
pvald <- read_excel("Daily_bootstrap_result.xlsx", sheet = 1, col_names = TRUE)
pvalh <- read_excel("Daily_bootstrap_result.xlsx", sheet = 2, col_names = TRUE)

# Attach BiotypeID / TempCondID etc. from dplid
pvald <- left_join(pvald, dplid, by = "DayPlaceID")
pvalh <- left_join(pvalh, dplid, by = "DayPlaceID")

pvald$TempCondID <- as.factor(pvald$TempCondID)
pvalh$TempCondID <- as.factor(pvalh$TempCondID)

library(segmented)

# Mean NRMSE per BioTempID and number of rings per plot (nRing),
# plus simple uncertainty interval (mean ± sd/n)
pvald.mean <- pvald %>%
  group_by(BioTempID, nRing) %>%
  summarise(
    nrmsem = mean(nrmse),
    UCI    = mean(nrmse, na.rm = TRUE) + sd(nrmse) / n(),
    LCI    = mean(nrmse, na.rm = TRUE) - sd(nrmse) / n()
  )

pvald.mean <- left_join(pvald.mean, dplid, by = "BioTempID") %>%
  distinct(BioTempID, nRing, nrmsem, UCI, LCI, BiotypeID, TempCondID)

pvald.mean <- rename(pvald.mean, "nrmse" = "nrmsem")

# Fit piecewise linear (segmented) relationships NRMSE ~ nRing
# separately for each BioTempID (1–4) and extract breakpoints
pvald.fit <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(pvald.fit) <- c("BioTempID", "nRing", "nrmse")

for (n in 1:4) {
  pvaldcut <- filter(pvald.mean, BioTempID == n)
  fitL     <- lm(nrmse ~ nRing, pvaldcut)       # linear base model
  fitS     <- segmented(fitL, npsi = 2)         # segmented model with 2 breakpoints
  
  bpoints  <- fitS$psi[, 2]                     # x-locations of breakpoints
  newx     <- c(1, bpoints, 10)                 # evaluate model at 1, breakpoints, 10
  newy     <- predict(fitS, data.frame(nRing = newx))
  
  fit <- data.frame(nRing = newx, nrmse = newy)
  fit$BioTempID <- n
  pvald.fit <- full_join(pvald.fit, fit)
}

pvald.fit <- left_join(pvald.fit, dplid, by = "BioTempID") %>%
  distinct(BioTempID, nRing, nrmse, BiotypeID, TempCondID)

# Keep only "internal" breakpoints for plotting
pvald.fitcut <- filter(pvald.fit, nRing > 2 & nRing < 10)

# Plot NRMSE vs nRing, with segmented fits and breakpoint markers
p <-
  ggplot(pvald.mean, aes(x = nRing, y = nrmse, color = factor(TempCondID))) +
  geom_point(aes(shape = factor(BiotypeID)), size = 4) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0, alpha = 0.5, linewidth = 1.1) +
  scale_color_manual(
    "Soil temperature condition",
    values = c("red", "blue"),
    labels = c("Warm", "Cold"),
    guide  = guide_legend(order = 1)
  ) +
  geom_line(
    data = pvald.fit,
    aes(
      x        = nRing,
      y        = nrmse,
      color    = factor(TempCondID),
      linetype = factor(BiotypeID)
    ),
    size = 1.2
  ) +
  scale_linetype_manual(
    "Type of Biotop",
    labels = c("Forest", "Grassland"),
    values = c(1, 2),
    guide  = guide_legend(order = 2)
  ) +
  scale_shape_manual(
    "Type of Biotop",
    labels = c("Forest", "Grassland"),
    values = c(16, 17),
    guide  = guide_legend(order = 2)
  ) +
  geom_point(
    data  = pvald.fitcut,
    aes(size = "Break points"),
    shape  = 4,
    color  = "khaki4",
    stroke = 2
  ) +
  scale_size_manual(
    "",
    values = rep(3),
    guide  = guide_legend(
      override.aes = list(colour = c("khaki4")),
      order        = 3
    ),
    labels = c("Break points")
  ) +
  labs(
    x = expression("Number of points per plot"),
    y = expression("NRMSE, %")
  ) +
  scale_x_continuous(breaks = seq(1, 10, 1), limits = c(1, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  theme_light() +
  theme(legend.position = "bottom")

## ---------------------------------------------------------------------------
## 7. NRMSE vs Hour: different numbers of points & tours (smooth curves)
## ---------------------------------------------------------------------------

library(paletteer)

# Colour palette for nRing curves
palet <- paletteer_d("Redmonder::qMSOStd")

ggplot(pvalh, aes(x = Hour, y = nrmse, color = factor(nRing))) +
  geom_smooth(se = FALSE) +
  scale_color_manual("Point per plot", values = palet) +
  labs(
    x = expression("Time of day," ~ h),
    y = expression("NRMSE, %")
  ) +
  facet_grid(
    TempCondID ~ Biotype,
    labeller = as_labeller(
      c("1" = "Warm", "2" = "Cold", "Forest" = "Forest", "Grassland" = "Grassland")
    )
  ) +
  scale_x_continuous(breaks = seq(1, 24, 1), limits = c(1, 24)) +
  theme_light() +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text       = element_text(color = "black")
  )

# Another view: NRMSE vs Hour with colours by DayPlaceID (individual tours)
palet <- paletteer_d("ggsci::springfield_simpsons")

ggplot(pvalh, aes(x = Hour, y = nrmse, color = factor(DayPlaceID))) +
  geom_smooth(se = FALSE) +
  scale_color_manual(
    "Tour",
    values = palet,
    labels = c(
      "Jun'14", "Aug'14", "Jun'15", "Oct'15", "Jun'18", "Nov'18", "Jun'18", "Oct'18",
      "Jun'14", "Aug'14", "May'15", "Oct'15", "Jun'18", "Nov'18", "Jun'18", "Oct'18"
    )
  ) +
  labs(
    x = expression("Time of day," ~ h),
    y = expression("NRMSE, %")
  ) +
  facet_grid(
    TempCondID ~ Biotype,
    labeller = as_labeller(
      c("1" = "Warm", "2" = "Cold", "Forest" = "Forest", "Grassland" = "Grassland")
    )
  ) +
  scale_x_continuous(breaks = seq(1, 24, 1), limits = c(1, 24)) +
  theme_light() +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text       = element_text(color = "black")
  )

## ---------------------------------------------------------------------------
## 8. NRMSE vs Hour & nRing: piecewise fits for nRing = 10, patchwork layout
## ---------------------------------------------------------------------------

pvnHmean <- read.csv("nrmseVSnHour_meanDay.csv", header = TRUE, sep = ",")

library(segmented)
library(patchwork)

# Mean NRMSE per BioTempID, nRing, nHour
pvHm.mean <- pvnHmean %>%
  group_by(BioTempID, nRing, nHour) %>%
  summarise(
    nrmsem = mean(nrmse),
    UCI    = mean(nrmse, na.rm = TRUE) + sd(nrmse) / n(),
    LCI    = mean(nrmse, na.rm = TRUE) - sd(nrmse) / n()
  )

pvHm.mean <- left_join(pvHm.mean, dplid, by = "BioTempID") %>%
  distinct(BioTempID, nRing, nHour, nrmse, UCI, LCI, BiotypeID, TempCondID)

pvHm.mean <- rename(pvHm.mean, "nrmse" = "nrmsem")

# Segmented fit nrmse ~ nHour for nRing = 10, per BioTempID
pvHm.fit <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(pvHm.fit) <- c("BioTempID", "nRing", "nHour", "nrmse")

for (n in 1:4) {
  pvcut <- filter(pvHm.mean, BioTempID == n, nRing == 10)
  fitL  <- lm(nrmse ~ nHour, pvcut)
  fitS  <- segmented(fitL, npsi = 2)
  
  bpoints <- fitS$psi[, 2]
  newx    <- c(1, bpoints, 24)
  newy    <- predict(fitS, data.frame(nHour = newx))
  
  fit <- data.frame(nHour = newx, nrmse = newy)
  fit$BioTempID <- n
  fit$nRing     <- 10
  pvHm.fit      <- full_join(pvHm.fit, fit)
}

# pvHm.fit1 seems used as cumulative object for all runs
pvHm.fit1 <- full_join(pvHm.fit1, pvHm.fit)
write.csv(pvHm.fit1, "nrmseVSnHour_fit.csv")

pvHm.fit1 <- left_join(pvHm.fit1, dplid, by = "BioTempID") %>%
  distinct(BioTempID, nRing, nHour, nrmse, BiotypeID, TempCondID)

pvHm.fitcut <- filter(pvHm.fit1, nHour > 2 & nHour < 14)

pvHm.fit1 <- read.csv("nrmseVSnHour_fit.csv", header = TRUE, sep = ",")

# Facet labels for numbers of points per plot
nRing.labs <- c(
  "1"  = "One point per plot",
  "5"  = "Five point per plot",
  "10" = "Ten point per plot"
)

# Plots for nRing = 1 & 5 together (upper panel)
pvHm.mean1  <- filter(pvHm.mean,  nRing %in% c("1", "5"))
pvHm.fit11  <- filter(pvHm.fit1, nRing %in% c("1", "5"))
pvHm.fitcut1 <- filter(pvHm.fitcut, nRing %in% c("1", "5"))

p1 <-
  ggplot(pvHm.mean1, aes(x = nHour, y = nrmse, color = factor(TempCondID))) +
  geom_point(aes(shape = factor(BiotypeID)), size = 4) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0, alpha = 0.5, linewidth = 1.1) +
  scale_color_manual(
    "Soil temperature condition",
    values = c("red", "blue"),
    labels = c("Warm", "Cold"),
    guide  = guide_legend(order = 1)
  ) +
  geom_line(
    data = pvHm.fit11,
    aes(
      x        = nHour,
      y        = nrmse,
      color    = factor(TempCondID),
      linetype = factor(BiotypeID)
    ),
    size = 1.2
  ) +
  scale_linetype_manual(
    "Type of Biotop",
    labels = c("Forest", "Grassland"),
    values = c(1, 2),
    guide  = guide_legend(order = 2)
  ) +
  scale_shape_manual(
    "Type of Biotop",
    labels = c("Forest", "Grassland"),
    values = c(16, 17),
    guide  = guide_legend(order = 2)
  ) +
  geom_point(
    data  = pvHm.fitcut1,
    aes(size = "Break points"),
    shape  = 4,
    color  = "khaki4",
    stroke = 2
  ) +
  scale_size_manual(
    "",
    values = rep(3),
    guide  = guide_legend(
      override.aes = list(colour = c("khaki4")),
      order        = 3
    ),
    labels = c("Break points")
  ) +
  labs(
    x = expression("Number of tours per day"),
    y = expression("NRMSE, %")
  ) +
  scale_x_continuous(breaks = seq(1, 24, 1), limits = c(1, 24)) +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 25)) +
  theme_light() +
  theme(legend.position = "none") +
  xlab(NULL) +
  facet_wrap(~nRing, nrow = 1, labeller = labeller(nRing = nRing.labs)) +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text       = element_text(color = "black")
  )

# Plot for nRing = 10 (bottom panel)
pvHm.mean2   <- filter(pvHm.mean,  nRing %in% c("10"))
pvHm.fit12   <- filter(pvHm.fit1, nRing %in% c("10"))
pvHm.fitcut2 <- filter(pvHm.fitcut, nRing %in% c("10"))

p2 <-
  ggplot(pvHm.mean2, aes(x = nHour, y = nrmse, color = factor(TempCondID))) +
  geom_point(aes(shape = factor(BiotypeID)), size = 4) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0, alpha = 0.5, linewidth = 1.1) +
  scale_color_manual(
    "Soil temperature condition",
    values = c("red", "blue"),
    labels = c("Warm", "Cold"),
    guide  = guide_legend(order = 1)
  ) +
  geom_line(
    data = pvHm.fit12,
    aes(
      x        = nHour,
      y        = nrmse,
      color    = factor(TempCondID),
      linetype = factor(BiotypeID)
    ),
    size = 1.2
  ) +
  scale_linetype_manual(
    "Type of Biotop",
    labels = c("Forest", "Grassland"),
    values = c(1, 2),
    guide  = guide_legend(order = 2)
  ) +
  scale_shape_manual(
    "Type of Biotop",
    labels = c("Forest", "Grassland"),
    values = c(16, 17),
    guide  = guide_legend(order = 2)
  ) +
  geom_point(
    data  = pvHm.fitcut2,
    aes(size = "Break points"),
    shape  = 4,
    color  = "khaki4",
    stroke = 2
  ) +
  scale_size_manual(
    "",
    values = rep(3),
    guide  = guide_legend(
      override.aes = list(colour = c("khaki4")),
      order        = 3
    ),
    labels = c("Break points")
  ) +
  labs(
    x = expression("Number of tours per day"),
    y = expression("NRMSE, %")
  ) +
  scale_x_continuous(breaks = seq(1, 24, 1), limits = c(1, 24)) +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 25)) +
  theme_light() +
  theme(legend.position = "bottom") +
  facet_wrap(~nRing, nrow = 1, labeller = labeller(nRing = nRing.labs)) +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text       = element_text(color = "black")
  )

# Combine p1 and p2 in custom layout using patchwork
p1 + p2 &
  plot_layout(
    design  = "AAAAAAA
               #######
               #BBBBB#",
    heights = c(3, 1, 3)
  )

## ---------------------------------------------------------------------------
## 9. Diurnal soil vs air temperature for forest sites
## ---------------------------------------------------------------------------

# Re-define facet names (again: first forest, then grassland version overwrites it)
PlTIDnames <- c(
  "1" = "Forest1_Jun'14", "2" = "Forest1_Aug'14",
  "3" = "Forest1_Jun'15", "4" = "Forest1_Oct'15",
  "5" = "Forest2_Jul'18", "6" = "Forest2_Nov'18",
  "7" = "Forest3_Jul'18", "8" = "Forest3_Oct'18"
)
PlTIDnames <- c(
  "9"  = "Grassland1_Jun'14", "10" = "Grassland1_Aug'14",
  "11" = "Grassland1_May'15", "12" = "Grassland1_Oct'15",
  "13" = "Grassland2_Jul'18", "14" = "Grassland2_Nov'18",
  "15" = "Grassland3_Jul'18", "16" = "Grassland3_Oct'18"
)

# Forest subset
df <- filter(data1, Biotop == "Forest")

# Plot daily soil and air temperature curves for each DayPlaceID
p <-
  ggplot(data = df) +
  geom_smooth(
    aes(x = as.numeric(TimeOrder), y = Tsoil, color = "red"),
    se   = FALSE,
    fill = "yellow"
  ) +
  geom_smooth(
    aes(x = as.numeric(TimeOrder), y = TAir, color = "blue"),
    se = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(1, 24, 1),
    limits = c(0.5, 24.5),
    labels = c(
      "1" = "10", "2" = "11", "3" = "12", "4" = "13", "5" = "14", "6" = "15",
      "7" = "16", "8" = "17", "9" = "18", "10" = "19", "11" = "20", "12" = "21",
      "13" = "22", "14" = "23", "15" = "24", "16" = "1", "17" = "2", "18" = "3",
      "19" = "4", "20" = "5", "21" = "6", "22" = "7", "23" = "8", "24" = "9"
    )
  ) +
  scale_color_manual(
    values = c("red", "blue"),
    labels = c("Soil", "Air"),
    name   = ""
  ) +
  labs(
    x = expression("Time of day," ~ h),
    y = expression("Temperature,"^{o} ~ "C")
  ) +
  facet_wrap(
    ~as.factor(DayPlaceID),
    ncol    = 2,
    scales  = "free_y",
    labeller = as_labeller(PlTIDnames)
  ) +
  theme_light() +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text       = element_text(color = "black"),
    legend.position  = "bottom"
  )
