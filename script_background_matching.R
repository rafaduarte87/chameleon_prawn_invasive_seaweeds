# Loading the packages ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(emmeans)
library(rstatix)
library(colorspace)
library(munsell)
library(ggfortify)
library(patchwork)

# Importing data ---------------------------------------------------------------

background_matching_data <-
  read_excel("background_matching_dataset.xlsx",
    sheet = 1,
    range = "A1:Q295")  |> 
  mutate(
    seaweed = factor(seaweed),
    type = factor(type))

## extracting LAB values to fill the points

LABdata_prawns <-
  with(background_matching_data[, c(15:17)], LAB(L, A, B))

## creating the data for the hull polygons around prawn data for each seaweed

hull_data_prawns <-
  background_matching_data |>
  filter(type == "prawn") |> 
  group_by(seaweed)  |> 
  slice(chull(rnl_x_mean, rnl_y_mean))

# Creating the plot of prawn colour diversity ----------------------------------

# Figure 2 (top plot)– Background colour matching of chameleon prawns (Hippolyte varians) against non-native seaweed host species. Colour variation of prawns sampled from pink harpoon weed Asparagopsis armata and brown wireweed Sargassum muticum in the receptor noise-limited (RNL).

seaweed_labs <- c("Harpoon", "Wireweed")
names(seaweed_labs) <- c("hw", "sm")

graph_colour_space_a <-
  ggplot() +
  geom_point(
    data = background_matching_data  |> 
           filter(type == "prawn"),
           aes(x = rnl_x_mean, y = rnl_y_mean),
           fill = hex(LABdata_prawns[c(1:184, 215:264), ], fix = TRUE),
           shape = 21, alpha = 0.8, size = 5) +
  geom_point(
    data = background_matching_data  |> 
           filter(type == "seaweed"),
           aes(x = rnl_x_mean, y = rnl_y_mean),
           fill = hex(LABdata_prawns[c(185:214, 265:294), ], fix = TRUE),
           shape = 24, alpha = 0.8, size = 5) +
  geom_polygon(
    data = hull_data_prawns,
    aes(x = rnl_x_mean, y = rnl_y_mean,
        fill = seaweed),
    alpha = 0.4, colour = "black", linetype = "dashed", linewidth = 0.8,
    show.legend = FALSE) +
  facet_grid(~ seaweed,
             labeller = labeller(seaweed = seaweed_labs)) 
  
graph_colour_space_b <-
  graph_colour_space_a +
  scale_x_continuous(
    name = "Green - Red [mw:lw]",
    breaks = seq(-0.5, 4.5, by = 1),
    labels = seq(-0.5, 4.5, by = 1),
    limits = c(-0.5, 4.5)) +
  scale_y_continuous(
    name = "Yellow - Blue [(lw+mw):sw]",
    breaks = seq(-22.5, 2.5, by = 5),
    labels = seq(-22.5, 2.5, by = 5),
    limits = c(-22.5, 2.5)) +
  scale_fill_manual(values=c("#FF9999", "#663300"))
  
graph_colour_space_c <-
  graph_colour_space_b +
  theme_bw() +
  theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    axis.text.x = element_text(colour = "black", size = 12, hjust = 0.5),
    axis.text.y = element_text(colour = "black", size = 12),
    axis.title.x = element_text(
                   face = "bold", colour = "black",
                   size = 14, margin = margin(
                              t = 10, r = 20, b = 10, l = 20)),
    axis.title.y = element_text(
                   face = "bold", colour = "black",
                   size = 14, margin = margin(
                              t = 10, r = 10, b = 10, l = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.background = element_rect(colour = "black", linewidth = 0.5),
    panel.spacing = unit(0.25, "cm"))

# Looking at prawn luminance between seaweeds ----------------------------------

background_matching_data  |> 
  filter(type == "prawn") |> 
  group_by(seaweed) |> 
  summarise(mean_luminance = mean(lum_mean),
            sd_luminance = sd(lum_mean))

var.test(log(lum_mean) ~ seaweed, 
         data = background_matching_data  |> 
         filter(type == "prawn"))

t.test(log(lum_mean) ~ seaweed, 
       data = background_matching_data  |> 
              filter(type == "prawn"),
       var.equal = TRUE)

# Calculating prawn JNDs against invasive seaweeds -----------------------------

# creating separate tables for prawns and seaweeds 

sargassum_data <-
  background_matching_data  |> 
  filter(seaweed == "sm", type == "seaweed")

prawn_sargassum_data <-
  background_matching_data |> 
  filter(seaweed == "sm", type == "prawn")

harpoon_data <-
  background_matching_data  |> 
  filter(seaweed == "hw", type == "seaweed")

prawn_harpoon_data <-
  background_matching_data  |> 
  filter(seaweed == "hw", type == "prawn")

# calculating the contrasts between each prawn and 30 algal fronds, and finally getting the mean

## Colour JNDs -----------------------------------------------------------------

### Sargassum ------------------------------------------------------------------

jnd_total_sargassum_colour <- rep(NA, dim(prawn_sargassum_data)[1])

for (i in 1:dim(prawn_sargassum_data)[1])
{
  jnd_individual_sargassum_colour <- rep(NA, dim(sargassum_data)[1])
  for (j in 1:dim(sargassum_data)[1])
  {
    jnd_individual_sargassum_colour[j] <-
      sqrt(((prawn_sargassum_data$rnl_x_mean[i] - 
             sargassum_data$rnl_x_mean[j])^2) +
          ((prawn_sargassum_data$rnl_y_mean[i] - 
            sargassum_data$rnl_y_mean[j])^2))
  }
  jnd_total_sargassum_colour[i] <- mean(jnd_individual_sargassum_colour)
}

### Harpoon --------------------------------------------------------------------

jnd_total_harpoon_colour <- rep(NA, dim(prawn_harpoon_data)[1])

for (i in 1:dim(prawn_harpoon_data)[1])
{
  jnd_individual_harpoon_colour <- rep(NA, dim(harpoon_data)[1])
  for (j in 1:dim(harpoon_data)[1])
  {
    jnd_individual_harpoon_colour[j] <-
      sqrt(((prawn_harpoon_data$rnl_x_mean[i] - 
             harpoon_data$rnl_x_mean[j])^2) +
           ((prawn_harpoon_data$rnl_y_mean[i] - 
             harpoon_data$rnl_y_mean[j])^2))
  }
  jnd_total_harpoon_colour[i] <- mean(jnd_individual_harpoon_colour)
}

## Luminance JNDs --------------------------------------------------------------

### Sargassum ------------------------------------------------------------------

jnd_total_sargassum_luminance <- rep(NA, dim(prawn_sargassum_data)[1])

for (i in 1:dim(prawn_sargassum_data)[1])
{
  jnd_individual_sargassum_luminance <- rep(NA, dim(sargassum_data)[1])
  for (j in 1:dim(sargassum_data)[1])
  {
    jnd_individual_sargassum_luminance[j] <-
      abs(log((prawn_sargassum_data |> 
               select(lum_mean) |> 
               pull() |> 
               nth(i) /
               sargassum_data$lum_mean[j]))/0.05)
  }
  jnd_total_sargassum_luminance[i] <- mean(jnd_individual_sargassum_luminance)
}

### Harpoon --------------------------------------------------------------------

jnd_total_harpoon_luminance <- rep(NA, dim(prawn_harpoon_data)[1])

for (i in 1:dim(prawn_harpoon_data)[1])
{
  jnd_individual_harpoon_luminance <- rep(NA, dim(harpoon_data)[1])
  for (j in 1:dim(harpoon_data)[1])
  {
    jnd_individual_harpoon_luminance[j] <-
      abs(log((prawn_harpoon_data |> 
                 select(lum_mean) |> 
                 pull() |> 
                 nth(i) /
                 harpoon_data$lum_mean[j]))/0.05)
  }
  jnd_total_harpoon_luminance[i] <- mean(jnd_individual_harpoon_luminance)
}

## Combining data --------------------------------------------------------------

prawn_sargassum_data_b <-
  prawn_sargassum_data  |> 
  mutate(jnd_colour = jnd_total_sargassum_colour,
         jnd_luminance = jnd_total_sargassum_luminance)

prawn_harpoon_data_b <-
  prawn_harpoon_data  |> 
  mutate(jnd_colour = jnd_total_harpoon_colour,
         jnd_luminance = jnd_total_harpoon_luminance)

background_matching_data_final <-
  bind_rows(
    prawn_sargassum_data_b,
    prawn_harpoon_data_b)

# calculating the mean JNDs for prawns against each exotic seaweed

mean_jnd_sargassum <-
  prawn_sargassum_data_b  |> 
  dplyr::summarise(jnd_colour_mean = mean(jnd_colour),
                   jnd_luminance_mean = mean(jnd_luminance))

mean_jnd_harpoon <-
  prawn_harpoon_data_b  |> 
  dplyr::summarise(jnd_colour_mean = mean(jnd_colour),
                   jnd_luminance_mean = mean(jnd_luminance))

# Creating the plot of prawn JNDs -----------------------------------------

# Figure 2 (bottom plot)– Background colour matching of chameleon prawns (Hippolyte varians) against non-native seaweed host species. Frequency distribution of the chromatic just-noticeable differences (JNDs) calculated as the Euclidean distances between prawn and seaweeds (harpoon or wireweed). 

graph_jnd_colour_seaweeds <-
  ggplot(
    background_matching_data_final,
    aes(x = jnd_colour, fill = seaweed, alpha = seaweed)) +
  geom_histogram(binwidth = 1, alpha = 0.5) +
  scale_x_continuous(
    name = "Colour JND",
    breaks = c(0, 2, 4, 6, 8, 10, 12),
    limits = c(-0.5, 12.5))+
  scale_y_continuous(
    name = "Count",
    breaks = c(0, 10, 20, 30, 40, 50, 60),
    limits = c(0, 60),
    expand = expansion(mult = 0, add = 0)) +
  scale_fill_manual(
    name = "Seaweed",
    labels = c("Harpoon", "Wireweed"),
    values = c("#FF9999", "#663300")) +
  geom_vline(
    data = mean_jnd_sargassum,
    aes(xintercept = jnd_colour_mean), colour = "#663300",
    linewidth = 1, linetype = "dashed") +
  geom_vline(
    data = mean_jnd_harpoon,
    aes(xintercept = jnd_colour_mean), colour = "#FF9999",
    linewidth = 1, linetype = "dashed") +
  theme_bw() +
  theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    axis.text.x = element_text(colour = "black", size = 12, hjust = 0.5),
    axis.text.y = element_text(colour = "black", size = 12),
    axis.title.x = element_text(
                   face = "bold", colour = "black",
                   size = 14, margin = margin(
                              t = 10, r = 20, b = 10, l = 20)),
    axis.title.y = element_text(
                   face = "bold", colour = "black",
                   size = 14, margin = margin(
                              t = 10, r = 10, b = 10, l = 10)),
    legend.position = "inside",
    legend.position.inside = c(0.08, 0.84),
    legend.title = element_text(size = 10, face = "bold",
                                margin = margin(b = 8)),
    legend.text = element_text(size = 8),
    legend.spacing.y = unit(0, "cm"),
    legend.key.size = unit(0.5, "cm"),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(
    byrow = TRUE,
    override.aes = list(size = 4)))

# Combining the colour space and JND plots to compose Figure 2   ---------------

final_figure_bm_seaweeds <-
  graph_colour_space_c +
  graph_jnd_colour_seaweeds +
  plot_layout(ncol = 1)

ggsave("Figure 2.png",
  final_figure_bm_seaweeds,
  height = 22.5, width = 20, units = "cm", dpi = 600)

# Calculating mean and sd of prawn JNDs against invasive seaweeds --------------

background_matching_data_final  |> 
  filter(type == "prawn")  |> 
  group_by(seaweed) |> 
  summarise(
    mean_jnd_colour = mean(jnd_colour),
    sd_jnd_colour = sd(jnd_colour),
    mean_jnd_luminance = mean(jnd_luminance),
    sd_jnd_luminance = sd(jnd_luminance)) |>
  as.data.frame()