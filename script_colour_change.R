# Loading the packages ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(emmeans)
library(rstatix)
library(colorspace)
library(munsell)
library(performance)
library(patchwork)
library(caret)
library(MASS)

# Importing the dataset --------------------------------------------------------

# prawns

colour_change_data <-
  read_excel("colour_change_dataset.xlsx",
    sheet = 1,
    range = "A1:P563")  |> 
  mutate(
    type = factor(type),
    seaweed = factor(seaweed,
                     levels = c("d", "sl", "hw", "sm")),
    day = factor(day, levels = c("0", "5", "10", "15", "20", "25", "30")))

colour_change_data$id_new <-
  paste(colour_change_data$type,
        colour_change_data$seaweed,
        colour_change_data$id,
        sep = "_")

# seaweeds

seaweed_data <-
  read_excel("colour_change_dataset.xlsx",
    sheet = 2,
    range = "A1:K101")  |> 
  filter(rnl_x_mean > (-1))  |> 
  mutate(
    seaweed = factor(seaweed),
    day = factor(day))

# let's select a total of 20 replicates for each seaweed (because we have 30 replicates only for the brown wireweed and the pink harpoon weed

set.seed(42) # maintaining the same 20 replicates

seaweed_data_sm <-
  seaweed_data  |> 
  filter(seaweed == "sm")  |> 
  slice_sample(n = 20)

set.seed(42)

seaweed_data_hw <-
  seaweed_data |> 
  filter(seaweed == "hw")  |> 
  slice_sample(n = 20)

seaweed_data_sl <-
  seaweed_data  |> 
  filter(seaweed == "sl")

seaweed_data_d <-
  seaweed_data  |> 
  filter(seaweed == "d")

# creating a new dataset for the seaweeds

seaweed_data_edited <-
  bind_rows(
    seaweed_data_d, seaweed_data_hw,
    seaweed_data_sl, seaweed_data_sm)

# let's get some summary data for the seaweeds

summary_seaweed_luminance <-
  seaweed_data_edited  |> 
  group_by(seaweed) |> 
  summarise(across(c(lum_mean, rnl_x_mean, rnl_y_mean),
            list(mean = ~mean(.x, na.rm = TRUE),
                 sd = ~sd(.x, na.rm = TRUE)),
                 .names = "{.col}_{.fn}"))

# Testing the initial size of prawns among the experimental conditions ---------

# this test is important to show that prawns had the same mean size when allocated to the different experimental units

## green prawns at day 0

model_green_size <-
  lm(size ~ seaweed,
     colour_change_data |> 
     filter(type == "g", day == "0"))

check_model(model_green_size, check = c("qq", "normality", "homogeneity"))

anova(model_green_size) 

# green prawns had similar size between the experimental conditions

## red prawns at day 0

model_red_size <-
  lm(size ~ seaweed,
     colour_change_data  |> 
     filter(type == "r", day == "0"))

check_model(model_red_size, check = c("qq", "normality", "homogeneity"))

anova(model_red_size) 

# red prawns had similar size between the experimental conditions

# Overall colour change - RNL chromaticity space -------------------------------

## Green prawns - colour change plot -------------------------------------------

# Figure 4a – Colour change and camouflage of green chameleon prawns (Hippolyte varians) maintained for 30 days against colour mismatching native (the red dulse Palmaria palmata) and non-native (the pink harpoon weed Asparagopsis armata and the brown wireweed Sargassum muticum) seaweeds.

# extracting the LAB columns to create the colour palette of the points

data_lab_green <-
  colour_change_data |> 
  filter(type == "g", day %in% c(0, 30))  |> 
  dplyr::select(type, id, seaweed, day,
  L = "LAB_CIE L_Mean", A = "LAB_CIE A_Mean", B = "LAB_CIE B_Mean")

# data was filtered for days 0 and 30 for a better visualization

# transforming the LAB data into a LAB object

LABdata_green <-
  with(data_lab_green[, c(5:7)], LAB(L, A, B))

# creating the plot for green prawns

day_labs <- 
  c("Day 0", "Day 5", "Day 10", "Day 15", "Day 20", "Day 25", "Day 30")

names(day_labs) <- 
  c("0", "5", "10", "15", "20", "25", "30")

green_colour_space_lab_a <-
  ggplot() +
  geom_point(
    data = colour_change_data  |> 
           filter(type == "g", day %in% c(0, 30)),
    aes(x = rnl_x_mean, y = rnl_y_mean, shape = seaweed),
    fill = hex(LABdata_green, fix = TRUE), alpha = 0.8, size = 5) +
  stat_ellipse(
    data = seaweed_data_edited  |> 
           filter(seaweed %in% c("d", "hw", "sm")),
    geom = "polygon",
    aes(x = rnl_x_mean, y = rnl_y_mean, fill = seaweed),
    alpha = 0.5, show.legend = FALSE) +
  facet_grid(~day,
             labeller = labeller(day = day_labs))

green_colour_space_lab_b <-
  green_colour_space_lab_a +
  scale_x_continuous(
    name = "Green - Red [mw:lw]",
    breaks = seq(-0.5, 4.5, by = 1),
    labels = seq(-0.5, 4.5, by = 1),
    limits = c(-0.5, 4.5)) +
  scale_y_continuous(
    name = "Yellow - Blue [(lw+mw):sw]",
    breaks = seq(-25, 5, by = 5),
    labels = seq(-25, 5, by = 5),
    limits = c(-25, 5)) +
  scale_shape_manual(
    name = "Seaweed",
    labels = c("Dulse", "Harpoon", "Wireweed"),
    values = c(21, 22, 24)) +
  scale_fill_manual(values = c("#990000", "#FF9999", "#663300"))

green_colour_space_lab_c <-
  green_colour_space_lab_b +
  theme_bw() +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, hjust = 0.5),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                    t = 10, r = 10, b = 10, l = 10)),
    axis.title.y = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                    t = 10, r = 10, b = 10, l = 10)),
    legend.position = "inside",
    legend.position.inside = c(0.375, 0.83),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.spacing.y = unit(0.35, "cm"),
    legend.key.size = unit(0.25, "cm"),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.background = element_rect(colour = "black", linewidth = 0.5),
    panel.spacing.y = unit(0.25, "cm")) +
  guides(shape = guide_legend(
          byrow = TRUE,
          override.aes = list(size = 3)))

## Red prawns - colour change plot ---------------------------------------------

# Figure 4c – Colour change and camouflage of red chameleon prawns (Hippolyte varians) maintained for 30 days against colour mismatching native (the green sea lettuce Ulva lactuca) and non-native (the pink harpoon weed Asparagopsis armata and the brown wireweed Sargassum muticum) seaweeds.

# extracting the LAB columns to create the colour palette of the points

data_lab_red <-
  colour_change_data  |> 
  filter(type == "r", day %in% c(0, 30))  |> 
  dplyr::select(type, id, seaweed, day,
  L = "LAB_CIE L_Mean", A = "LAB_CIE A_Mean", B = "LAB_CIE B_Mean")

# data was filtered for days 0 and 30 for a better visualization

# transforming the LAB data into a LAB object

LABdata_red <-
  with(data_lab_red[, c(5:7)], LAB(L, A, B))

# creating the plot for red prawns

red_colour_space_lab_a <-
  ggplot() +
  geom_point(
    data = colour_change_data  |> 
           filter(type == "r", day %in% c(0, 30)),
    aes(x = rnl_x_mean, y = rnl_y_mean, shape = seaweed),
    fill = hex(LABdata_red, fix = TRUE), alpha = 0.8, size = 5) +
  stat_ellipse(
    data = seaweed_data_edited  |> 
           filter(seaweed %in% c("sl", "hw", "sm")),
    geom = "polygon",
    aes(x = rnl_x_mean, y = rnl_y_mean, fill = seaweed),
    alpha = 0.5, show.legend = FALSE) +
  facet_grid(~day,
             labeller = labeller(day = day_labs))

red_colour_space_lab_b <-
  red_colour_space_lab_a +
  scale_x_continuous(
    name = "Green - Red [mw:lw]",
    breaks = seq(-0.5, 4.5, by = 1),
    labels = seq(-0.5, 4.5, by = 1),
    limits = c(-0.5, 4.5)) +
  scale_y_continuous(
    name = "Yellow - Blue [(lw+mw):sw]",
    breaks = seq(-25, 5, by = 5),
    labels = seq(-25, 5, by = 5),
    limits = c(-25, 5)) +
  scale_shape_manual(
    name = "Seaweed",
    labels = c("Lettuce", "Harpoon", "Wireweed"),
    values = c(21, 22, 24)) +
  scale_fill_manual(values = c("#FF9999", "#66FF33", "#663300"))

red_colour_space_lab_c <-
  red_colour_space_lab_b +
  theme_bw() +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, hjust = 0.5),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                    t = 10, r = 10, b = 10, l = 10)),
    axis.title.y = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                    t = 10, r = 10, b = 10, l = 10)),
    legend.position = "inside",
    legend.position.inside = c(0.12, 0.275),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.spacing.y = unit(0.35, "cm"),
    legend.key.size = unit(0.25, "cm"),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.background = element_rect(colour = "black", linewidth = 0.5),
    panel.spacing.y = unit(0.25, "cm")) +
  guides(shape = guide_legend(
    byrow = TRUE,
    override.aes = list(size = 3)))

# Calculating the JND variation of prawns over time ----------------------------

## Green prawns - JND calculation ----------------------------------------------

# 1) green prawns against HW

colour_change_data_g_hw <-
  colour_change_data |>
  filter(type == "g", seaweed == "hw")

jnd_total_g_hw <- rep(NA, dim(colour_change_data_g_hw)[1])

for (i in 1:dim(colour_change_data_g_hw)[1])
{
  jnd_g_hw <- rep(NA, dim(seaweed_data_hw)[1])
  for (j in 1:dim(seaweed_data_hw)[1])
  {
    jnd_g_hw[j] <-
      sqrt(((colour_change_data_g_hw$rnl_x_mean[i] -
             seaweed_data_hw$rnl_x_mean[j])^2) +
             ((colour_change_data_g_hw$rnl_y_mean[i] - 
             seaweed_data_hw$rnl_y_mean[j])^2))
  }
  jnd_total_g_hw[i] <- mean(jnd_g_hw)
}

colour_change_data_g_hw$jnd <-
  jnd_total_g_hw

# 2) green prawns against SM

colour_change_data_g_sm <-
  colour_change_data  |> 
  filter(type == "g", seaweed == "sm")

jnd_total_g_sm <- rep(NA, dim(colour_change_data_g_sm)[1])

for (i in 1:dim(colour_change_data_g_sm)[1])
{
  jnd_g_sm <- rep(NA, dim(seaweed_data_sm)[1])
  for (j in 1:dim(seaweed_data_sm)[1])
  {
    jnd_g_sm[j] <-
      sqrt(((colour_change_data_g_sm$rnl_x_mean[i] - 
             seaweed_data_sm$rnl_x_mean[j])^2) +
             ((colour_change_data_g_sm$rnl_y_mean[i] - 
             seaweed_data_sm$rnl_y_mean[j])^2))
  }
  jnd_total_g_sm[i] <- mean(jnd_g_sm)
}

colour_change_data_g_sm$jnd <-
  jnd_total_g_sm

# 3) green prawns against D

colour_change_data_g_d <-
  colour_change_data  |> 
  filter(type == "g", seaweed == "d")

jnd_total_g_d <- rep(NA, dim(colour_change_data_g_d)[1])

for (i in 1:dim(colour_change_data_g_d)[1])
{
  jnd_g_d <- rep(NA, dim(seaweed_data_d)[1])
  for (j in 1:dim(seaweed_data_d)[1])
  {
    jnd_g_d[j] <-
      sqrt(((colour_change_data_g_d$rnl_x_mean[i] - 
             seaweed_data_d$rnl_x_mean[j])^2) +
           ((colour_change_data_g_d$rnl_y_mean[i] - 
             seaweed_data_d$rnl_y_mean[j])^2))
  }
  jnd_total_g_d[i] <- mean(jnd_g_d)
}

colour_change_data_g_d$jnd <-
  jnd_total_g_d

## Red prawns - JND calculation ------------------------------------------------

# 1) red prawns against HW

colour_change_data_r_hw <-
  colour_change_data  |> 
  filter(type == "r", seaweed == "hw")

jnd_total_r_hw <- rep(NA, dim(colour_change_data_r_hw)[1])

for (i in 1:dim(colour_change_data_r_hw)[1])
{
  jnd_r_hw <- rep(NA, dim(seaweed_data_hw)[1])
  for (j in 1:dim(seaweed_data_hw)[1])
  {
    jnd_r_hw[j] <-
      sqrt(((colour_change_data_r_hw$rnl_x_mean[i] - 
             seaweed_data_hw$rnl_x_mean[j])^2) +
           ((colour_change_data_r_hw$rnl_y_mean[i] - 
             seaweed_data_hw$rnl_y_mean[j])^2))
  }
  jnd_total_r_hw[i] <- mean(jnd_r_hw)
}

colour_change_data_r_hw$jnd <-
  jnd_total_r_hw

# 2) red prawns against SM

colour_change_data_r_sm <-
  colour_change_data  |> 
  filter(type == "r", seaweed == "sm")

jnd_total_r_sm <- rep(NA, dim(colour_change_data_r_sm)[1])

for (i in 1:dim(colour_change_data_r_sm)[1])
{
  jnd_r_sm <- rep(NA, dim(seaweed_data_sm)[1])
  for (j in 1:dim(seaweed_data_sm)[1])
  {
    jnd_r_sm[j] <-
      sqrt(((colour_change_data_r_sm$rnl_x_mean[i] - 
             seaweed_data_sm$rnl_x_mean[j])^2) +
           ((colour_change_data_r_sm$rnl_y_mean[i] - 
             seaweed_data_sm$rnl_y_mean[j])^2))
  }
  jnd_total_r_sm[i] <- mean(jnd_r_sm)
}

colour_change_data_r_sm$jnd <-
  jnd_total_r_sm

# 3) red prawns against SL

colour_change_data_r_sl <-
  colour_change_data  |> 
  filter(type == "r", seaweed == "sl")

jnd_total_r_sl <- rep(NA, dim(colour_change_data_r_sl)[1])

for (i in 1:dim(colour_change_data_r_sl)[1])
{
  jnd_r_sl <- rep(NA, dim(seaweed_data_sl)[1])
  for (j in 1:dim(seaweed_data_sl)[1])
  {
    jnd_r_sl[j] <-
      sqrt(((colour_change_data_r_sl$rnl_x_mean[i] - 
            seaweed_data_sl$rnl_x_mean[j])^2) +
          ((colour_change_data_r_sl$rnl_y_mean[i] - 
            seaweed_data_sl$rnl_y_mean[j])^2))
  }
  jnd_total_r_sl[i] <- mean(jnd_r_sl)
}

colour_change_data_r_sl$jnd <-
  jnd_total_r_sl

# Let's combine all data

colour_change_data_jnd <-
  bind_rows(
    colour_change_data_g_d, colour_change_data_g_hw,
    colour_change_data_g_sm, colour_change_data_r_sl,
    colour_change_data_r_hw, colour_change_data_r_sm)

# Creating a summary table with the JND values ---------------------------------

colour_change_data_jnd_summary <-
  colour_change_data_jnd  |> 
  group_by(type, seaweed, day)  |> 
  summarise(
    mean_jnd = mean(jnd),
    sd_jnd = sd(jnd),
    sample_size = n())  |> 
  mutate(se_jnd = sd_jnd / sqrt(sample_size))

# Camouflage change plot ----------------------------------------------------

## Green prawns - camouflage change plot ------------------------------------

# Figure 4b – Colour change and camouflage of green chameleon prawns (Hippolyte varians) maintained for 30 days against colour mismatching native (the red dulse Palmaria palmata) and non-native (the pink harpoon weed Asparagopsis armata and the brown wireweed Sargassum muticum) seaweeds.

graph_colour_jnd_green_a <-
  ggplot(
    data = colour_change_data_jnd_summary  |> 
           filter(type == "g"),
    aes(x = day, y = mean_jnd)) +
  geom_line(aes(colour = seaweed, group = seaweed)) +
  geom_errorbar(
    aes(ymin = mean_jnd - se_jnd,
        ymax = mean_jnd + se_jnd),
    width = 0.2,
    position = position_dodge(0)) +
  geom_point(aes(fill = seaweed, shape = seaweed), colour = "black", size = 5)

graph_colour_jnd_green_b <-
  graph_colour_jnd_green_a +
  scale_x_discrete(
    name = "Day",
    na.translate = FALSE,
    breaks = seq(0, 30, by = 5),
    labels = seq(0, 30, by = 5)) +
  scale_y_continuous(
    name = "Colour JND",
    breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16),
    labels = c(0, 2, 4, 6, 8, 10, 12, 14, 16),
    limits = c(0, 16.05)) +
  scale_fill_manual(
    name = "Seaweed",
    labels = c("Dulse", "Harpoon", "Wireweed"),
    values = c("#990000", "#FF9999", "#663300")) +
  scale_colour_manual(
    name = "Seaweed",
    labels = c("Dulse", "Harpoon", "Wireweed"),
    values = c("#990000", "#FF9999", "#663300")) +
  scale_shape_manual(
    name = "Seaweed",
    labels = c("Dulse", "Harpoon", "Wireweed"),
    values = c(21, 22, 24))

graph_colour_jnd_green_c <-
  graph_colour_jnd_green_b +
  theme_bw() +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, hjust = 0.5),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                     t = 10, r = 10, b = 10, l = 10)),
    axis.title.y = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                     t = 10, r = 10, b = 10, l = 10)),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 1)

## Red prawns - camouflage change plot -----------------------------------------

# Figure 4d – Colour change and camouflage of red chameleon prawns (Hippolyte varians) maintained for 30 days against colour mismatching native (the green sea lettuce Ulva lactuca) and non-native (the pink harpoon weed Asparagopsis armata and the brown wireweed Sargassum muticum) seaweeds.

graph_colour_jnd_red_a <-
  ggplot(
    data = colour_change_data_jnd_summary  |> 
           filter(type == "r"),
    aes(x = day, y = mean_jnd)) +
  geom_line(aes(colour = seaweed, group = seaweed)) +
  geom_errorbar(
    aes(ymin = mean_jnd - se_jnd,
        ymax = mean_jnd + se_jnd),
    width = 0.2,
    position = position_dodge(0)) +
  geom_point(aes(fill = seaweed, shape = seaweed), colour = "black", size = 5)

graph_colour_jnd_red_b <-
  graph_colour_jnd_red_a +
  scale_x_discrete(
    name = "Day",
    breaks = seq(0, 30, by = 5),
    labels = seq(0, 30, by = 5)) +
  scale_y_continuous(
    name = "Colour JND",
    breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16),
    labels = c(0, 2, 4, 6, 8, 10, 12, 14, 16),
    limits = c(0, 16.05)) +
  scale_fill_manual(
    name = "Seaweed",
    labels = c("Lettuce", "Harpoon", "Wireweed"),
    values = c("#66FF33", "#FF9999", "#663300")) +
  scale_colour_manual(
    name = "Seaweed",
    labels = c("Lettuce", "Harpoon", "Wireweed"),
    values = c("#66FF33", "#FF9999", "#663300")) +
  scale_shape_manual(
    name = "Seaweed",
    labels = c("Lettuce", "Harpoon", "Wireweed"),
    values = c(21, 22, 24))

graph_colour_jnd_red_c <-
  graph_colour_jnd_red_b +
  theme_bw() +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, hjust = 0.5),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                     t = 10, r = 10, b = 10, l = 10)),
    axis.title.y = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                     t = 10, r = 10, b = 10, l = 10)),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 1)

# Creating the final figure of colour and camouflage change --------------------

graph_colour_camouflage_change <-
  green_colour_space_lab_c +
  graph_colour_jnd_green_c +
  red_colour_space_lab_c +
  graph_colour_jnd_red_c +
  plot_layout(ncol = 2, heights = c(5, 5), widths = c(4.5, 5)) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

graph_colour_camouflage_change[[1]] <-
  graph_colour_camouflage_change[[1]] +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

graph_colour_camouflage_change[[2]] <-
  graph_colour_camouflage_change[[2]] +
  theme(plot.margin = margin(0, 0.5, 0, 0.5, "cm"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.tag.position = c(0.055, 0.95))

graph_colour_camouflage_change[[3]] <-
  graph_colour_camouflage_change[[3]] +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))

graph_colour_camouflage_change[[4]] <-
  graph_colour_camouflage_change[[4]] +
  theme(plot.margin = margin(0, 0.5, 0, 0.5, "cm"),
        plot.tag.position = c(0.055, 0.95))

ggsave("Figure 4.png",
       graph_colour_camouflage_change,
       height = 15, width = 30, units = "cm", dpi = 600)

# Statistical tests ---------------------------------------------------------

# Table 2 – Summary results of the analysis of variance applied to separate linear mixed-effects models testing differences in luminance, colour (expressed as x and y coordinates of the receptor-noise limited model - RNL) and camouflage (expressed as units of just noticeable differences – JNDs) change of green and red chameleon prawns (Hippolyte varians) maintained in different seaweed species over 30 days.

## Change in luminance ------------------------------------------------------

# green prawns

model_green_lum <-
  lmer(lum_mean ~ day * seaweed + size + (1 | id_new),
    na.action = na.omit,
    data = colour_change_data_jnd  |> 
           filter(type == "g"))

check_model(model_green_lum, check = c("qq", "normality", "homogeneity"))

check_heteroscedasticity(model_green_lum)

anova(model_green_lum)

pairs(emmeans(model_green_lum, "seaweed", by = "day"))

# red prawns

model_red_lum <-
  lmer(lum_mean ~ day * seaweed + size + (1 | id_new),
    na.action = na.omit,
    data = colour_change_data_jnd  |> 
           filter(type == "r"))

check_model(model_red_lum, check = c("qq", "normality", "homogeneity"))

check_heteroscedasticity(model_red_lum)

anova(model_red_lum)

pairs(emmeans(model_red_lum, "seaweed", by = "day"))

## Change in colour - X coordinate (mw:lw) green - red ----------------------

# green prawns

model_green_x <-
  lmer(rnl_x_mean ~ day * seaweed + size + (1 | id_new),
    na.action = na.omit,
    data = colour_change_data_jnd  |> 
           filter(type == "g"))

check_model(model_green_x, check = c("qq", "normality", "homogeneity"))

check_heteroscedasticity(model_green_x)

anova(model_green_x)

pairs(emmeans(model_green_x, "seaweed", by = "day"))

# red prawns

model_red_x <-
  lmer(rnl_x_mean ~ day * seaweed + size + (1 | id_new),
    na.action = na.omit,
    data = colour_change_data_jnd  |> 
           filter(type == "r"))

check_model(model_red_x, check = c("qq", "normality", "homogeneity"))

check_heteroscedasticity(model_red_x)

anova(model_red_x)

pairs(emmeans(model_red_x, "seaweed", by = "day"))

## Change in colour - Y coordinate (lw+mw):sw yellow - blue -----------------

# green prawns

model_green_y <-
  lmer(rnl_y_mean ~ day * seaweed + size + (1 | id_new),
    na.action = na.omit,
    data = colour_change_data_jnd  |> 
           filter(type == "g"))

check_model(model_green_y, check = c("qq", "normality", "homogeneity"))

check_heteroscedasticity(model_green_y)

anova(model_green_y) |> 
  as.data.frame()

pairs(emmeans(model_green_y, "seaweed", by = "day"))

# red prawns

model_red_y <-
  lmer(rnl_y_mean ~ day * seaweed + size + (1 | id_new),
    na.action = na.omit,
    data = colour_change_data_jnd  |> 
           filter(type == "r"))

check_model(model_red_y, check = c("qq", "normality", "homogeneity"))

check_heteroscedasticity(model_red_y)

anova(model_red_y)

pairs(emmeans(model_red_y, "seaweed", by = "day"))

## Change in saturation ------------------------------------------------------

# green prawns

model_green_saturation <-
  lmer(log(rnl_saturation_mean) ~ day * seaweed + size + (1 | id_new),
       na.action = na.omit,
       data = colour_change_data_jnd  |> 
              filter(type == "g"))

check_model(model_green_saturation, 
            check = c("qq", "normality", "homogeneity"))

check_heteroscedasticity(model_green_saturation)

anova(model_green_saturation)

pairs(emmeans(model_green_saturation, "seaweed", by = "day"))

# red prawns

model_red_saturation <-
  lmer(log(rnl_saturation_mean) ~ day * seaweed + size + (1 | id_new),
       na.action = na.omit,
       data = colour_change_data_jnd  |> 
              filter(type == "r"))

check_model(model_red_saturation, check = c("qq", "normality", "homogeneity"))

check_heteroscedasticity(model_red_saturation)

anova(model_red_saturation)

pairs(emmeans(model_red_saturation, "seaweed", by = "day"))

## Change in camouflage - JND -----------------------------------------------

# green prawns

model_green_jnd <-
  lmer(log(jnd) ~ day * seaweed + size + (1 | id_new),
    na.action = na.omit,
    data = colour_change_data_jnd  |> 
           filter(type == "g"))

check_model(model_green_jnd, check = c("qq", "normality", "homogeneity"))

check_heteroscedasticity(model_green_jnd)

anova(model_green_jnd)

pairs(emmeans(model_green_jnd, "seaweed", by = "day"))

# red prawns

model_red_jnd <-
  lmer(log(jnd) ~ day * seaweed + size + (1 | id_new),
    na.action = na.omit,
    data = colour_change_data_jnd  |> 
           filter(type == "r"))

check_model(model_red_jnd, check = c("qq", "normality", "homogeneity"))

check_heteroscedasticity(model_red_jnd)

anova(model_red_jnd)

pairs(emmeans(model_red_jnd, "seaweed", by = "day"))

# Creating a summary table with the luminance and colour values -------------

# luminance

colour_change_data_summary_lum_mean <-
  colour_change_data_jnd  |> 
  group_by(type, seaweed, day)  |> 
  summarise(
    mean_lum_mean = mean(lum_mean),
    sd_lum_mean = sd(lum_mean),
    sample_size = n()) |> 
  mutate(se_lum_mean = sd_lum_mean / sqrt(sample_size))

# colour - X (mw : lw)

colour_change_data_summary_x <-
  colour_change_data_jnd  |> 
  group_by(type, seaweed, day)  |> 
  summarise(
    mean_x = mean(rnl_x_mean),
    sd_x = sd(rnl_x_mean),
    sample_size = n())  |> 
  mutate(se_x = sd_x / sqrt(sample_size))

# colour - Y [sw : (lw + mw)]

colour_change_data_summary_y <-
  colour_change_data |> 
  group_by(type, seaweed, day)  |> 
  summarise(
    mean_y = mean(rnl_y_mean),
    sd_y = sd(rnl_y_mean),
    sample_size = n())  |> 
  mutate(se_y = sd_y / sqrt(sample_size))

# Luminance and colour change plot (supplementary) --------------------------

# Figure S2 – Mean luminance and colour (expressed as x and y coordinates of the receptor-noise limited model - RNL) change of green (left plots – a, c, e) and red (right plots – b, d, f) chameleon prawns (Hippolyte varians) maintained for 30 days against colour mismatching native (the red dulse Palmaria palmata for green prawns, and the green sea lettuce Ulva lactuca for red prawns) and non-native (the pink harpoon weed Asparagopsis armata and the brown wireweed Sargassum muticum) seaweeds.

## Luminance plot -----------------------------------------------------------

# green prawns

data_green_lum <-
  colour_change_data_summary_lum_mean  |> 
  filter(type == "g") |> 
  ungroup()  |> 
  add_row(
    type = "g",
    seaweed = "sl",
    day = NA, mean_lum_mean = NA, sd_lum_mean = NA,
    sample_size = NA, se_lum_mean = NA)  |> 
  mutate(
    type = factor(type),
    seaweed = factor(seaweed,
      levels = c("d", "sl", "hw", "sm")))

graph_colour_lum_mean_green_a <-
  ggplot(
    data_green_lum,
    aes(x = day, y = mean_lum_mean)) +
  geom_line(aes(colour = seaweed, group = seaweed)) +
  geom_errorbar(
    aes(
      ymin = mean_lum_mean - se_lum_mean,
      ymax = mean_lum_mean + se_lum_mean),
    width = 0.2,
    position = position_dodge(0)) +
  geom_point(aes(fill = seaweed), colour = "black", size = 4, shape = 21)

graph_colour_lum_mean_green_b <-
  graph_colour_lum_mean_green_a +
  scale_x_discrete(
    name = "Day",
    na.translate = FALSE,
    breaks = seq(0, 30, by = 5),
    labels = seq(0, 30, by = 5)) +
  scale_y_continuous(
    name = "Luminance",
    breaks = c(0.125, 0.2, 0.275, 0.35, 0.425, 0.5, 0.575),
    labels = c(0.125, 0.2, 0.275, 0.35, 0.425, 0.5, 0.575),
    limits = c(0.125, 0.575)) +
  scale_fill_manual(
    name = "Seaweed",
    labels = c("Dulse", "Lettuce", "Harpoon", "Wireweed"),
    values = c("#990000", "#66FF33", "#FF9999", "#663300")) +
  scale_colour_manual(
    name = "Seaweed",
    labels = c("Dulse", "Lettuce", "Harpoon", "Wireweed"),
    values = c("#990000", "#66FF33", "#FF9999", "#663300"))

graph_colour_lum_mean_green_c <-
  graph_colour_lum_mean_green_b +
  theme_bw() +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, hjust = 0.5),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                     t = 10, r = 10, b = 10, l = 10)),
    axis.title.y = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                     t = 10, r = 10, b = 10, l = 10)),
    legend.position = "inside",
    legend.position.inside = c(0.21, 0.865),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 8),
    legend.spacing.y = unit(0, "cm"),
    legend.key.size = unit(0.5, "cm"),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(
          byrow = TRUE, nrow = 2,
          override.aes = list(size = 3)))

# red prawns

graph_colour_lum_mean_red_a <-
  ggplot(
    colour_change_data_summary_lum_mean  |> 
    filter(type == "r"),
    aes(x = day, y = mean_lum_mean)) +
  geom_line(aes(colour = seaweed, group = seaweed)) +
  geom_errorbar(
    aes(
      ymin = mean_lum_mean - se_lum_mean,
      ymax = mean_lum_mean + se_lum_mean),
    width = 0.2,
    position = position_dodge(0)) +
  geom_point(aes(fill = seaweed), colour = "black", size = 4, shape = 21)

graph_colour_lum_mean_red_b <-
  graph_colour_lum_mean_red_a +
  scale_x_discrete(
    name = "Day",
    breaks = seq(0, 30, by = 5),
    labels = seq(0, 30, by = 5)) +
  scale_y_continuous(
    name = "Luminance",
    breaks = c(0.125, 0.2, 0.275, 0.35, 0.425, 0.5, 0.575),
    labels = c(0.125, 0.2, 0.275, 0.35, 0.425, 0.5, 0.575),
    limits = c(0.125, 0.575)) +
  scale_fill_manual(
    name = "Seaweed",
    labels = c("Lettuce", "Harpoon", "Wireweed"),
    values = c("#66FF33", "#FF9999", "#663300")) +
  scale_colour_manual(
    name = "Seaweed",
    labels = c("Lettuce", "Harpoon", "Wireweed"),
    values = c("#66FF33", "#FF9999", "#663300"))

graph_colour_lum_mean_red_c <-
  graph_colour_lum_mean_red_b +
  theme_bw() +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, hjust = 0.5),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                     t = 10, r = 10, b = 10, l = 10)),
    axis.title.y = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                     t = 10, r = 10, b = 10, l = 10)),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

## Colour plot - X coordinate mw:lw -----------------------------------------

# green prawns

graph_colour_x_green_a <-
  ggplot(
    data = colour_change_data_summary_x  |> 
    filter(type == "g"),
    aes(x = day, y = mean_x)) +
  geom_line(aes(colour = seaweed, group = seaweed)) +
  geom_errorbar(
    aes(
      ymin = mean_x - se_x,
      ymax = mean_x + se_x),
    width = 0.2,
    position = position_dodge(0)) +
  geom_point(aes(fill = seaweed), colour = "black", size = 4, shape = 21)

graph_colour_x_green_b <-
  graph_colour_x_green_a +
  scale_x_discrete(
    name = "Day",
    na.translate = FALSE,
    breaks = seq(0, 30, by = 5),
    labels = seq(0, 30, by = 5)) +
  scale_y_continuous(
    name = "Green - Red [mw:lw]",
    breaks = c(0.4, 0.8, 1.2, 1.6, 2, 2.4, 2.8),
    labels = c(0.4, 0.8, 1.2, 1.6, 2, 2.4, 2.8),
    limits = c(0.4, 2.8)) +
  scale_fill_manual(
    name = "Seaweed",
    labels = c("Dulse", "Harpoon", "Wireweed"),
    values = c("#990000", "#FF9999", "#663300")) +
  scale_colour_manual(
    name = "Seaweed",
    labels = c("Dulse", "Harpoon", "Wireweed"),
    values = c("#990000", "#FF9999", "#663300"))

graph_colour_x_green_c <-
  graph_colour_x_green_b +
  theme_bw() +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, hjust = 0.5),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                     t = 10, r = 10, b = 10, l = 10)),
    axis.title.y = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                     t = 10, r = 10, b = 10, l = 10)),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

# red prawns

graph_colour_x_red_a <-
  ggplot(
    colour_change_data_summary_x  |> 
    filter(type == "r"),
    aes(x = day, y = mean_x)) +
  geom_line(aes(colour = seaweed, group = seaweed)) +
  geom_errorbar(
    aes(
      ymin = mean_x - se_x,
      ymax = mean_x + se_x),
    width = 0.2,
    position = position_dodge(0)) +
  geom_point(aes(fill = seaweed), colour = "black", size = 4, shape = 21)

graph_colour_x_red_b <-
  graph_colour_x_red_a +
  scale_x_discrete(
    name = "Day",
    breaks = seq(0, 30, by = 5),
    labels = seq(0, 30, by = 5)) +
  scale_y_continuous(
    name = "Green - Red [mw:lw]",
    breaks = c(0.4, 0.8, 1.2, 1.6, 2, 2.4, 2.8),
    labels = c(0.4, 0.8, 1.2, 1.6, 2, 2.4, 2.8),
    limits = c(0.4, 2.8)) +
  scale_fill_manual(
    name = "Seaweed",
    labels = c("Lettuce", "Harpoon", "Wireweed"),
    values = c("#66FF33", "#FF9999", "#663300")) +
  scale_colour_manual(
    name = "Seaweed",
    labels = c("Lettuce", "Harpoon", "Wireweed"),
    values = c("#66FF33", "#FF9999", "#663300"))

graph_colour_x_red_c <-
  graph_colour_x_red_b +
  theme_bw() +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, hjust = 0.5),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                     t = 10, r = 10, b = 10, l = 10)),
    axis.title.y = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                     t = 10, r = 10, b = 10, l = 10)),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

## Colour plot - Y coordinate (lw+mw):sw ------------------------------------

# green prawns

graph_colour_y_green_a <-
  ggplot(
    colour_change_data_summary_y  |> 
    filter(type == "g"),
    aes(x = day, y = mean_y)) +
  geom_line(aes(colour = seaweed, group = seaweed)) +
  geom_errorbar(
    aes(
      ymin = mean_y - se_y,
      ymax = mean_y + se_y),
    width = 0.2,
    position = position_dodge(0)) +
  geom_point(aes(fill = seaweed), colour = "black", size = 4, shape = 21)

graph_colour_y_green_b <-
  graph_colour_y_green_a +
  scale_x_discrete(
    name = "Day",
    breaks = seq(0, 30, by = 5),
    labels = seq(0, 30, by = 5)) +
  scale_y_continuous(
    name = "Yellow - Blue [(lw+mw):sw]",
    breaks = c(-18, -15, -12, -9, -6, -3, 0),
    labels = c(-18, -15, -12, -9, -6, -3, 0),
    limits = c(-18, 0)) +
  scale_fill_manual(
    name = "Seaweed",
    labels = c("Dulse", "Harpoon", "Wireweed"),
    values = c("#990000", "#FF9999", "#663300")) +
  scale_colour_manual(
    name = "Seaweed",
    labels = c("Dulse", "Harpoon", "Wireweed"),
    values = c("#990000", "#FF9999", "#663300"))

graph_colour_y_green_c <-
  graph_colour_y_green_b +
  theme_bw() +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, hjust = 0.5),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                     t = 10, r = 10, b = 10, l = 10)),
    axis.title.y = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                     t = 10, r = 10, b = 10, l = 10)),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

# red prawns

graph_colour_y_red_a <-
  ggplot(
    colour_change_data_summary_y  |> 
    filter(type == "r"),
    aes(x = day, y = mean_y)) +
  geom_line(aes(colour = seaweed, group = seaweed)) +
  geom_errorbar(
    aes(
      ymin = mean_y - se_y,
      ymax = mean_y + se_y),
    width = 0.2,
    position = position_dodge(0)) +
  geom_point(aes(fill = seaweed), colour = "black", size = 4, shape = 21)

graph_colour_y_red_b <-
  graph_colour_y_red_a +
  scale_x_discrete(
    name = "Day",
    breaks = seq(0, 30, by = 5),
    labels = seq(0, 30, by = 5)) +
  scale_y_continuous(
    name = "Yellow - Blue [(lw+mw):sw]",
    breaks = c(-18, -15, -12, -9, -6, -3, 0),
    labels = c(-18, -15, -12, -9, -6, -3, 0),
    limits = c(-18, 0)) +
  scale_fill_manual(
    name = "Seaweed",
    labels = c("Lettuce", "Harpoon", "Wireweed"),
    values = c("#66FF33", "#FF9999", "#663300")) +
  scale_colour_manual(
    name = "Seaweed",
    labels = c("Lettuce", "Harpoon", "Wireweed"),
    values = c("#66FF33", "#FF9999", "#663300"))

graph_colour_y_red_c <-
  graph_colour_y_red_b +
  theme_bw() +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, hjust = 0.5),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                     t = 10, r = 10, b = 10, l = 10)),
    axis.title.y = element_text(
                    face = "bold", colour = "black",
                    size = 12, margin = margin(
                     t = 10, r = 10, b = 10, l = 10)),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

## Creating the final figure of luminance and colour change -----------------

graph_luminance_colour_prawns <-
  graph_colour_lum_mean_green_c +
  graph_colour_lum_mean_red_c +
  graph_colour_x_green_c +
  graph_colour_x_red_c +
  graph_colour_y_green_c +
  graph_colour_y_red_c +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

# remove x title and text for plot 1

graph_luminance_colour_prawns[[1]] <-
  graph_luminance_colour_prawns[[1]] +
  theme(
    plot.margin = margin(0.5, 0.25, 0.25, 0.5, "cm"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank())

# remove x and y title and text for plot 2

graph_luminance_colour_prawns[[2]] <-
  graph_luminance_colour_prawns[[2]] +
  theme(
    plot.margin = margin(0.5, 0.5, 0, 0, "cm"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank())

# remove x title and text for plot 3

graph_luminance_colour_prawns[[3]] <-
  graph_luminance_colour_prawns[[3]] +
  theme(
    plot.margin = margin(0, 0.25, 0.25, 0.5, "cm"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank())

# remove x and y title and text for plot 4

graph_luminance_colour_prawns[[4]] <-
  graph_luminance_colour_prawns[[4]] +
  theme(
    plot.margin = margin(0, 0.5, 0, 0, "cm"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank())

# change margins for plots 5

graph_luminance_colour_prawns[[5]] <-
  graph_luminance_colour_prawns[[5]] +
  theme(plot.margin = margin(0, 0.25, 0.25, 0.5, "cm"))

# remove y title and text for plot 6

graph_luminance_colour_prawns[[6]] <-
  graph_luminance_colour_prawns[[6]] +
  theme(
    plot.margin = margin(0, 0.5, 0.5, 0, "cm"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank())

ggsave("Figure S2.png",
  graph_luminance_colour_prawns,
  height = 25, width = 25, units = "cm", dpi = 600)

# Discriminant function analyses  --------------------------------------------

## Do green prawns form discrete groups in the end of 30 days of experiment?

# filtering and selecting the data

data_green_day_30 <-
  colour_change_data  |>
  filter(type == "g", day == 30) |>
  mutate(seaweed = factor(seaweed, levels = c("d", "hw", "sm"))) |> 
  dplyr::select(
         seaweed, lum_mean, rnl_x_mean, rnl_y_mean, rnl_saturation_mean)

# splitting the dataset between training and testing samples with a 60% cut-off

training_samples_green <-
  data_green_day_30$seaweed  |> 
  createDataPartition(p = 0.6, list = FALSE)

train_data_green <- data_green_day_30[training_samples_green, ]

test_data_green <- data_green_day_30[-training_samples_green, ]

# estimating pre-processing parameters

preproc_param_green <- 
  train_data_green  |>  
  preProcess(method = c("center", "scale"))

# transforming the data using the estimated parameters

train_transformed_green <- 
  preproc_param_green  |>  
  predict(train_data_green)

test_transformed_green <- 
  preproc_param_green  |>  
  predict(test_data_green)

# running the LDA

model_green <- lda(seaweed~., data = train_transformed_green)

# making predictions

predictions_green <- 
  model_green  |>  
  predict(test_transformed_green)

# model accuracy

mean(predictions_green$class == test_transformed_green$seaweed) #  our model correctly classified 53% of the observations

## Do red prawns form discrete groups after 30 days of experiment?

# filtering and selecting the data

data_red_day_30 <-
  colour_change_data  |>
  filter(type == "r", day == 30) |>
  mutate(seaweed = factor(seaweed, levels = c("hw", "sl", "sm"))) |> 
  dplyr::select(
    seaweed, lum_mean, rnl_x_mean, rnl_y_mean, rnl_saturation_mean)

# splitting the dataset between training and testing samples with a 60% cut-off

training_samples_red <-
  data_red_day_30$seaweed  |> 
  createDataPartition(p = 0.6, list = FALSE)

train_data_red <- 
  data_red_day_30[training_samples_red, ]

test_data_red <- 
  data_red_day_30[-training_samples_red, ]

# estimating pre-processing parameters

preproc_param_red <- 
  train_data_red  |>  
  preProcess(method = c("center", "scale"))

# transforming the data using the estimated parameters

train_transformed_red <- 
  preproc_param_red  |>  
  predict(train_data_red)

test_transformed_red <- 
  preproc_param_red  |>  
  predict(test_data_red)

# running the LDA

model_red <- lda(seaweed~., data = train_transformed_red)

# making predictions

predictions_red <- 
  model_red  |>  
  predict(test_transformed_red)

# model accuracy

mean(predictions_red$class == test_transformed_red$seaweed) #  our model correctly classified 100% of the observations