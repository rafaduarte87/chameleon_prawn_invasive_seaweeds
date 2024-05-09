# Loading the packages ----------------------------------------------------

library(tidyverse)
library(readxl)
library(ggfortify)
library(patchwork)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)

# Importing the dataset -------------------------------------------------------

data_prawn_choice_a <-
  read_excel("data_choice_experiment.xlsx",
    sheet = 1,
    range = "A1:H523", na = "NA")  |> 
  mutate(
    type = factor(type),
    comparison = factor(comparison))

# doing some data wrangling and cleaning

data_prawn_choice_b <-
  data_prawn_choice_a |> 
  pivot_longer(
    cols = c(
           "first_choice", "two_minutes", "four_minutes",
           "six_minutes", "eight_minutes", "ten_minutes"),
    names_to = "time",
    values_to = "choice")  |> 
  mutate(time = factor(time))

data_prawn_choice_b$time <-
  forcats::fct_recode(data_prawn_choice_b$time,
           first = "first_choice",
           two_min = "two_minutes",
           four_min = "four_minutes",
           six_min = "six_minutes",
           eight_min = "eight_minutes",
           ten_min = "ten_minutes")

data_prawn_choice_b <-
  data_prawn_choice_b  |> 
  mutate(time = factor(time,
    levels = c("first", "two_min", "four_min",
               "six_min", "eight_min", "ten_min")))

# Filtering for first choices and for red/green prawns ------------------------

summary_table_first_choice <-
  data_prawn_choice_b  |> 
  filter(time == "first", type %in% c("g", "r"))  |> 
  group_by(type, comparison, choice)  |> 
  summarise(n_choices = n())

# calculating the number of prawns of each colour type used in the trials

data_prawn_choice_b  |> 
  filter(time == "first", type %in% c("g", "r"))  |> 
  group_by(type) |> 
  summarise(sample_size = n())

# calculating the number of replicates for each seaweed combination

data_prawn_choice_b  |> 
  filter(time == "first", type %in% c("g", "r"))  |> 
  group_by(type, comparison)  |> 
  summarise(sample_size = n())

# Filtering for 10 min choices and red/green prawns ------------------------

summary_table_ten_min_choice <-
  data_prawn_choice_b  |> 
  filter(time == "ten_min", type %in% c("g", "r"))  |> 
  group_by(type, comparison, choice) |> 
  summarise(n_choices = n())

# Counting the missing values (no choices) --------------------------------

## First choices

summary_table_first_choice  |> 
  filter(is.na(choice)) |> 
  group_by(type)  |> 
  summarise(sum(n_choices)) # missing values

summary_table_first_choice  |> 
  filter(!is.na(choice)) |> 
  group_by(type) |> 
  summarise(sum(n_choices)) # non-missing values

# comparing the ratio of no-choices between green and red prawns

prop.test(x = c(21, 22), n = c(21 + 146, 22 + 148))

## 10 minutes choices

summary_table_ten_min_choice  |> 
  filter(is.na(choice)) |> 
  group_by(type) |> 
  summarise(sum(n_choices)) # missing values

summary_table_ten_min_choice  |> 
  filter(!is.na(choice)) |> 
  group_by(type) |> 
  summarise(sum(n_choices)) # non-missing values

# comparing the ratio of no-choices between green and red prawns

prop.test(x = c(31, 37), n = c(31 + 136, 37 + 133))

# Fitting different GLMs to test prawn seaweed choices ---------------------

# generating the final table for analyses

data_choices_reduced <-
  data_prawn_choice_b  |> 
  filter(
    time %in% c("first", "ten_min"),
    type %in% c("g", "r"))  |> 
  mutate(prawn_id = rep(1:337, each = 2))

summary_table_overall <-
  data_choices_reduced  |> 
  group_by(type, comparison, time, choice)  |> 
  summarise(n_choices = n())

# First comparison: colour-matching natives x non-natives -----------------

# recoding the variables

data_binomial_1 <-
  data_choices_reduced  |> 
  filter(type == "g" & comparison %in% c("sl_hw", "sl_sm") |
         type == "r" & comparison %in% c("d_hw", "d_sm"))  |> 
  mutate(type = factor(type),
         time = factor(time),
         choice = factor(choice,
                         levels = c("sl", "d", "hw", "sm"),
                         labels = c("native", "native", "invasive", "invasive")))

# coding the choices as native colour-matching = 1; invasive = 0

data_binomial_1$choice_bin <-
  ifelse(data_binomial_1$choice == "native", 1, 0)

# running models with no-choice data

# full model

model_binomial_1_a <-
  glmer(choice_bin ~ type * time + (1 | prawn_id),
        data = data_binomial_1, na.action = na.omit,
        family = "binomial")

model_binomial_1_b <-
  glmer(choice_bin ~ type + time + (1 | prawn_id),
        data = data_binomial_1, na.action = na.omit,
        family = "binomial")

model_binomial_1_c <-
  glmer(choice_bin ~ type + (1 | prawn_id),
        data = data_binomial_1, na.action = na.omit,
        family = "binomial")

anova(model_binomial_1_b, model_binomial_1_c, test = "Chi")

model_binomial_1_d <-
  glmer(choice_bin ~ time + (1 | prawn_id),
        data = data_binomial_1, na.action = na.omit,
        family = "binomial")

anova(model_binomial_1_b, model_binomial_1_d, test = "Chi")

# since time is not an important factor in the analysis (i.e. the choice of prawns did not change over time) I'll work only with first-choice data

model_binomial_1_final_a <-
  glm(choice_bin ~ type,
      na.action = na.omit, family = "binomial",
      data = data_binomial_1  |> 
             filter(time == "first"))

model_binomial_1_final_b <-
  glm(choice_bin ~ 1,
      na.action = na.omit, family = "binomial",
      data = data_binomial_1  |> 
             filter(time == "first"))

anova(model_binomial_1_final_a, model_binomial_1_final_b, test = "Chi")

summary(model_binomial_1_final_b)

plogis(coef(model_binomial_1_final_b)[1])

## FIRST CHOICES

data_binomial_1  |> 
  filter(time == "first") |> 
  group_by(time, type, choice)  |> 
  summarise(frequency = n())

binom.test(35, 35 + 41, p = 0.5) # for green prawns

binom.test(37, 37 + 34, p = 0.5) # for red prawns

data_binomial_1  |> 
  filter(time == "first") |> 
  group_by(choice) |> 
  summarise(n())

binom.test(72, 72 + 75, p = 0.5) # overall choice 

# Second comparison: colour-mismatching natives x non-natives ---------------

# recoding the variables

data_binomial_2 <-
  data_choices_reduced  |> 
  filter(type == "g" & comparison %in% c("d_hw", "d_sm") |
         type == "r" & comparison %in% c("sl_hw", "sl_sm"))  |> 
  mutate(type = factor(type),
         time = factor(time),
         choice = factor(choice,
                         levels = c("sl", "d", "hw", "sm"),
                         labels = c("native", "native", "invasive", "invasive")))

# coding the choices as native colour-mismatching = 1; invasive = 0

data_binomial_2$choice_bin <-
  ifelse(data_binomial_2$choice == "native", 1, 0)

# running models with no-choice data

# full model

model_binomial_2_a <-
  glmer(choice_bin ~ type * time + (1 | prawn_id),
        data = data_binomial_2, na.action = na.omit,
        family = "binomial")

model_binomial_2_b <-
  glmer(choice_bin ~ type + time + (1 | prawn_id),
        data = data_binomial_2, na.action = na.omit,
        family = "binomial")

anova(model_binomial_2_a, model_binomial_2_b, test = "Chi")

summary(model_binomial_2_a)

# although the best model contains the interaction, this is not-significant

pairs(emmeans(model_binomial_2_a, "time", by = "type"))

model_binomial_2_c <-
  glmer(choice_bin ~ type + (1 | prawn_id),
        data = data_binomial_2, na.action = na.omit,
        family = "binomial")

anova(model_binomial_2_b, model_binomial_2_c, test = "Chi")

model_binomial_2_d <-
  glmer(choice_bin ~ time + (1 | prawn_id),
        data = data_binomial_2, na.action = na.omit,
        family = "binomial")

anova(model_binomial_2_b, model_binomial_2_d, test = "Chi")

# since time is not an important factor in the analysis (i.e. the choice of prawns did not change over time) I'll work only with first-choice data

model_binomial_2_final_a <-
  glm(choice_bin ~ type,
      na.action = na.omit, family = "binomial",
      data = data_binomial_2  |> 
             filter(time == "first"))

model_binomial_2_final_b <-
  glm(choice_bin ~ 1,
      na.action = na.omit, family = "binomial",
      data = data_binomial_2  |> 
             filter(time == "first"))

anova(model_binomial_2_final_a, model_binomial_2_final_b, test = "Chi")

summary(model_binomial_2_final_b)

plogis(coef(model_binomial_2_final_b)[1])

# there is around 36% of probability of prawns selecting a colour-mismatching seaweed when gave them a choice between this seaweed (dulse for green prawns and sea lettuce for red prawns) and the two invasive seaweeds

## FIRST CHOICES

data_binomial_2 |> 
  filter(time == "first") |>
  group_by(type, choice)  |> 
  summarise(frequency = n())

binom.test(26, 26 + 44, p = 0.5) # for green prawns

binom.test(27, 27 + 50, p = 0.5) # for red prawns

data_binomial_2  |> 
  filter(time == "first") |> 
  group_by(choice) |> 
  summarise(n())

binom.test(53, 53 + 94, p = 0.5) # overall choice 

# Graphics ----------------------------------------------------------------

colour_labs <- c("Native colour-matching", "Native colour-mismatching")
names(colour_labs) <- c("matching", "non_matching")

graph_choice_bars_a  <- 
  ggplot(data_choice_graphic,
         aes(x = type, y = choice_prop * 100, group = seaweed)) +
  geom_bar(aes(fill = code),
    position = "dodge", stat = "identity", colour = "black",width = 0.7) +
  facet_grid(~colour,labeller = labeller(colour = colour_labs))

graph_choice_bars_b <-
  graph_choice_bars_a +
  scale_x_discrete(
    name = "Prawn colour type",
    labels = c("green", "red")) +
  scale_y_continuous(
    name = "Percentage of choice (%)",
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70),
    limits = c(0, 75),
    expand = expansion(mult = 0)) +
  scale_fill_manual(
    name = "Seaweed",
    labels = c("Native green sea-lettuce", "Native red dulse",
               "Non-native seaweeds"),
    values = c("#66FF33", "#990000", "#CCCCCC"))

graph_choice_bars_c <-
  graph_choice_bars_b +
  theme_bw() +
  theme(
        axis.text.x = element_text(colour = "black", size = 10, hjust = 0.5),
        axis.text.y = element_text(colour = "black", size = 10),
        axis.title.x = element_text(face = "bold", colour = "black",
                                    size = 12, margin = margin(
                                    t = 10, r = 20, b = 10, l = 20)),
        axis.title.y = element_text(face = "bold", colour = "black",
                                    size = 12, margin = margin(
                                    t = 10, r = 10, b = 10, l = 10)),
        legend.position = "inside",
        legend.position.inside = c(0.13, 0.89),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 8),
        legend.spacing.y = unit(0.2, "cm"),
        legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(
        linewidth = 0.5, linetype = "solid",
        colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.background = element_rect(colour = "black", linewidth = 0.5),
        panel.spacing = unit(0.25, "cm")) +
  geom_hline(yintercept = 50, linetype = "dashed") +
  guides(fill = guide_legend(byrow = TRUE))

text_stat_1 <-
  data.frame(label = c("ns", "ns"),
             colour = as.factor(c("matching", "matching")),
             seaweed = as.factor(c("native", "invasive")),
             x = c(1, 2), y = c(57, 57))

text_stat_2 <-
  data.frame(label = c("*", "*"),
             colour = as.factor(c("non_matching", "non_matching")),
             seaweed = as.factor(c("native", "invasive")),
             x = c(1, 2), y = c(67, 68))

graph_choice_bars_d <-
  graph_choice_bars_c +
  geom_text(data = text_stat_1,
            aes(x = x, y = y, label = label),
            fontface = "bold", size = 4) +
  geom_text(data = text_stat_2,
            aes(x = x, y = y, label = label),
            fontface = "bold", size = 5)

ggsave("Figure 3.png", graph_choice_bars_d,
       height = 15, width = 22.5, unit = "cm", dpi = 600)