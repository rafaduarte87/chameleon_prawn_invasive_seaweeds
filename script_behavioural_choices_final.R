# Loading the packages ----------------------------------------------------

library(tidyverse)
library(readxl)
library(ggfortify)
library(patchwork)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(DHARMa)
library(afex)

# Importing the dataset -------------------------------------------------------

data_prawn_choice_a <-
  read_excel("data_choice_experiment_final.xlsx",
    sheet = 1,
    range = "A1:F338", na = "NA")  |> 
  mutate(
    type = factor(type),
    non_native = factor(non_native),
    native_match = factor(native_match,
                          levels = c("1", "0"),
                          labels = c("yes", "no")))

# doing some data wrangling and cleaning - convert the time columns to a new factor named "time"

data_prawn_choice_b <-
  data_prawn_choice_a |> 
  pivot_longer(
    cols = c("first_choice", "ten_minutes"),
    names_to = "time",
    values_to = "choice")  |> 
  mutate(time = factor(time))

data_prawn_choice_b$time <-
  forcats::fct_recode(data_prawn_choice_b$time,
           first = "first_choice",
           ten_min = "ten_minutes")

data_prawn_choice_b <-
  data_prawn_choice_b  |> 
  mutate(time = factor(time,
    levels = c("first", "ten_min")))

# Exploring the dataset - overal proportions ----------------------------------

## Filtering for first choices -------------------------------------------------

summary_table_first_choice <-
  data_prawn_choice_b  |> 
  filter(time == "first")  |> 
  group_by(type, non_native, native_match, choice)  |> 
  summarise(n_choices = n())

# calculating the number of prawns of each colour type used in the trials

data_prawn_choice_b  |> 
  filter(time == "first")  |> 
  group_by(type) |> 
  summarise(sample_size = n())

# calculating the number of replicates for each seaweed combination

data_prawn_choice_b  |> 
  filter(time == "first")  |> 
  group_by(type, comparison)  |> 
  summarise(sample_size = n())

## Filtering for 10 min choices -------------------------------------------------

summary_table_ten_min_choice <-
  data_prawn_choice_b  |> 
  filter(time == "ten_min")  |> 
  group_by(type, non_native, native_match, choice) |> 
  summarise(n_choices = n())

# Counting the missing values (number  of no choices) --------------------------

## First choices

# considering prawn type

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

# considering the matching against native seaweeds

summary_table_first_choice  |> 
  filter(is.na(choice)) |> 
  group_by(native_match)  |> 
  summarise(sum(n_choices)) # missing values

# considering the identity of the non-native seaweed

summary_table_first_choice  |> 
  filter(is.na(choice)) |> 
  group_by(non_native)  |> 
  summarise(sum(n_choices)) # missing values

## 10 minutes choices

# considering prawn type

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

# considering the matching against native seaweeds

summary_table_ten_min_choice  |> 
  filter(is.na(choice)) |> 
  group_by(native_match)  |> 
  summarise(sum(n_choices)) # missing values

# considering the identity of the non-native seaweed

summary_table_ten_min_choice  |> 
  filter(is.na(choice)) |> 
  group_by(non_native)  |> 
  summarise(sum(n_choices)) # missing values

# Fitting generalized linear models to test prawn choices ----------------------

# Let's compare the choice between native and non-native seaweeds according to the type of non-native seaweed and the colour matching of the native seaweed

# First, let's add a new factor named prawn_id to control for the repeated measurement  at the first and the ten-minutes choice

data_choices_final <-
  data_prawn_choice_b  |> 
  mutate(prawn_id = rep(1:337, each = 2))

summary_table_overall <-
  data_choices_final  |> 
  group_by(type, non_native, native_match, time, choice)  |> 
  summarise(n_choices = n())

# recoding the variables - let's name sea lettuce (sl) and red dulse (d) as native seaweeds, and harpoon weed (hw) and brown wireweed (sm) as non-native seaweeds

data_binomial <-
  data_choices_final  |> 
  mutate(choice = factor(choice,
                         levels = c("sl", "d", "hw", "sm"),
                         labels = c("native", "native", 
                                    "non_native", "non_native")))

# coding the choices as native = 1; non-native = 0

data_binomial$choice_bin <-
  ifelse(data_binomial$choice == "native", 1, 0)

## Models that included NAs ----------------------------------------------------

### glmer using both first and 10 minutes choices ------------------------------

# running the model

model_binomial_1_a <-
  glmer(choice_bin ~ type * non_native * native_match + time + (1 | prawn_id),
        data = data_binomial, na.action = na.omit,
        family = "binomial")

model_binomial_1_b <-
  glmer(choice_bin ~ type * non_native + type * native_match + 
        non_native * native_match + time + (1 | prawn_id),
        data = data_binomial, na.action = na.omit,
        family = "binomial")

anova(model_binomial_1_a, model_binomial_1_b)

# the triple interaction is not important

model_binomial_1_c <-
  glmer(choice_bin ~ type + non_native * native_match + 
        time + (1 | prawn_id),
        data = data_binomial, na.action = na.omit,
        family = "binomial")

anova(model_binomial_1_b, model_binomial_1_c)

# the interactions with colour type are not important

model_binomial_1_d <-
  glmer(choice_bin ~ type + non_native + native_match + 
        time + (1 | prawn_id),
        data = data_binomial, na.action = na.omit,
        family = "binomial")

anova(model_binomial_1_c, model_binomial_1_d)

# the interaction between non-native and native_match is not important

# the best model is the additive model, without any interaction

# testing the assumptions

output_model_binomial_1_d <- 
  simulateResiduals(fittedModel = model_binomial_1_d)

plot(output_model_binomial_1_d, asFactor = TRUE)

testDispersion(output_model_binomial_1_d)

# let's test the effects of the main factors 

summary(model_binomial_1_d)

anova(model_binomial_1_d)

pairs(emmeans(model_binomial_1_d, "native_match"))

# the level of colour matching of the native seaweed is significant

# let's calculate the proportions

data_binomial |>
  group_by(type, native_match, choice) |> 
  summarise(n())

### glm using only data for first choices --------------------------------------

# running the model

model_binomial_1_e <-
  glm(choice_bin ~ type + non_native + native_match,
        data = data_binomial |> 
               filter(time == "first"), 
        na.action = na.omit,
        family = "binomial")

# testing the assumptions

output_model_binomial_1_e <- 
  simulateResiduals(fittedModel = model_binomial_1_e)

plot(output_model_binomial_1_e, asFactor = TRUE)

testDispersion(output_model_binomial_1_e)

# let's test the effects of the main factors 

summary(model_binomial_1_e)

anova(model_binomial_1_e)

pairs(emmeans(model_binomial_1_e, "native_match"))

# the level of colour matching of the native seaweed is significant

# let's calculate the proportions

data_binomial |>
  filter(time == "first") |> 
  group_by(type, native_match, choice) |> 
  summarise(n())

## Models that excluded NAs ----------------------------------------------------

### glmer using both first and 10 minutes choices ------------------------------

# running the model

model_binomial_1_f <-
  glmer(choice_bin ~ type + non_native + native_match + time + (1 | prawn_id),
        data = data_binomial |> 
               drop_na(), 
        family = "binomial")

# testing the assumptions

output_model_binomial_1_f <- 
  simulateResiduals(fittedModel = model_binomial_1_f)

plot(output_model_binomial_1_f, asFactor = TRUE)

testDispersion(output_model_binomial_1_f)

# let's test the effects of the main factors 

summary(model_binomial_1_f)

anova(model_binomial_1_f)

pairs(emmeans(model_binomial_1_f, "native_match"))

# the level of colour matching of the native seaweed is significant

# let's calculate the proportions

data_binomial |>
  drop_na() |> 
  group_by(type, native_match, choice) |> 
  summarise(n())

### glm using only data for first choices --------------------------------------

# running the model

model_binomial_1_g <-
  glm(choice_bin ~ type + non_native + native_match,
      data = data_binomial |> 
        filter(time == "first") |> 
        drop_na(),
      family = "binomial")

# testing the assumptions

output_model_binomial_1_g <- 
  simulateResiduals(fittedModel = model_binomial_1_g)

plot(output_model_binomial_1_g, asFactor = TRUE)

testDispersion(output_model_binomial_1_g)

# let's test the effects of the main factors 

summary(model_binomial_1_g)

anova(model_binomial_1_g)

pairs(emmeans(model_binomial_1_g, "native_match"))

# the level of colour matching of the native seaweed is significant

# let's calculate the proportions

data_binomial_summary <-
  data_binomial |>
  filter(time == "first") |> 
  drop_na() |> 
  group_by(type, native_match, choice) |> 
  summarise(n_choices = n()) |> 
  mutate(native_match = factor(native_match,
                               levels = c("yes", "no"),
                               labels = c("matching", "mismatching")))

data_binomial_summary$prop_choice <-
  c(35/(35+41), 41/(35+41), 26/(26+44), 44/(26+44), 
    37/(37+34), 34/(37+34), 27/(27+50), 50/(27+50))

data_binomial_summary$colour_code <-
  c("1", "3", "2", "3", "2", "3","1", "3")

# Graphics ---------------------------------------------------------------------

## model estimates -------------------------------------------------------------

afex_plot_behavioural_choice_glmer <-
  afex_plot(
    model_binomial_1_d, x = "native_match",
    error_arg = list(linewidth = 0.7, width = 0.1),
    point_arg = list(size = 6, shape = 21, stroke = 1,
                     fill = c("black", "white"),
                     colour = "black"),
    factor_levels = 
      list(native_match = c("yes","no")),
    mapping = "fill", data_alpha = 0.25,
    data_plot = FALSE) +
  labs(y = "Choice for the native seaweed (%)", 
       x = "Colour match to the native seaweed") +
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.2),
                     labels = seq(0, 80, by = 20),
                     limits = c(0, 0.8)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_text(label = "towards native", x = 2.35, y = 0.8, 
            size = 3, fontface = "bold", colour = "black") +
  geom_text(label = "towards non-native", x = 2.35, y = 0, 
            size = 3, fontface = "bold", colour = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 10, hjust = 0.5),
        axis.text.y = element_text(colour = "black", size = 10),
        axis.title.x = element_text(face = "bold", colour = "black", 
                                    size = 12, margin = margin(
                                    t = 10, r = 20, b = 10, l = 20)),
        axis.title.y = element_text(face = "bold", colour = "black", 
                                    size = 12, margin = margin(
                                    t = 10, r = 10, b = 10, l = 10)),
        legend.position = "none",
        panel.border = element_rect(linewidth = 1, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

afex_plot_behavioural_choice_glmer$data

ggsave("Figure 3.png", afex_plot_behavioural_choice_glmer,
       height = 17.5, width = 20, unit = "cm", dpi = 600)

afex_plot_behavioural_choice_glm <-
  afex_plot(
    model_binomial_1_e, x = "native_match",
    error_arg = list(linewidth = 0.7, width = 0.1),
    point_arg = list(size = 5, shape = 21, stroke = 1,
                     fill = c("black", "white"),
                     colour = "black"),
    factor_levels = 
      list(native_match = c("yes","no")),
    mapping = "fill", data_alpha = 0.25,
    data_plot = FALSE) +
  labs(y = "Choice for the native seaweed (%)", 
       x = "Colour match to the native seaweed") +
  scale_y_continuous(breaks = seq(0.2, 0.6, by = 0.1),
                     labels = seq(20, 60, by = 10),
                     limits = c(0.2, 0.6)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 10, hjust = 0.5),
        axis.text.y = element_text(colour = "black", size = 10),
        axis.title.x = element_text(face = "bold", colour = "black", 
                                    size = 12, margin = margin(
                                      t = 10, r = 20, b = 10, l = 20)),
        axis.title.y = element_text(face = "bold", colour = "black", 
                                    size = 12, margin = margin(
                                      t = 10, r = 10, b = 10, l = 10)),
        legend.position = "none",
        panel.border = element_rect(linewidth = 1, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

afex_plot_behavioural_choice_glm$data


## raw proportion -------------------------------------------------------------

colour_type_labs <- c("Green", "Red")
names(colour_type_labs) <- c("g", "r")

graph_choice_bars_a  <- 
  ggplot(data_binomial_summary,
         aes(x = native_match, y = prop_choice * 100, group = choice)) +
  geom_bar(aes(fill = colour_code),
    position = "dodge", stat = "identity", colour = "black",width = 0.7) +
  facet_grid(~ type,
             labeller = labeller(type = colour_type_labs))

graph_choice_bars_b <-
  graph_choice_bars_a +
  scale_x_discrete(
    name = "Colour match to the native seaweed",
    labels = c("yes", "no")) +
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
  data.frame(label = c("ns", "*"),
             type = as.factor(c("g", "g")),
             choice = as.factor(c("native", "non_native")),
             x = c(1, 2), y = c(57, 67))

text_stat_2 <-
  data.frame(label = c("ns", "*"),
             type = as.factor(c("r", "r")),
             choice = as.factor(c("native", "non_native")),
             x = c(1, 2), y = c(57, 68))

graph_choice_bars_d <-
  graph_choice_bars_c +
  geom_text(data = text_stat_1,
            aes(x = x, y = y, label = label),
            fontface = "bold", size = 4) +
  geom_text(data = text_stat_2,
            aes(x = x, y = y, label = label),
            fontface = "bold", size = 4)

ggsave("Figure 3_bars.png", graph_choice_bars_d,
       height = 15, width = 22.5, unit = "cm", dpi = 600)
