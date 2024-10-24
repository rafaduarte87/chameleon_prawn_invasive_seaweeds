# Loading the packages ----------------------------------------------------

library(tidyverse)
library(readxl)
library(patchwork)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(DHARMa)
library(afex)

# Importing the dataset -------------------------------------------------------

data_prawn_choice_a <-
  read_excel("behavioural_choices_dataset.xlsx",
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

## Filtering for 10 min choices ------------------------------------------------

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

## Models that excluded NAs ----------------------------------------------------

# running the model

model_binomial_1_e <-
  glmer(choice_bin ~ type + non_native + native_match + time + (1 | prawn_id),
        data = data_binomial |> 
               drop_na(choice_bin), 
        family = "binomial")

# testing the assumptions

output_model_binomial_1_e <- 
  simulateResiduals(fittedModel = model_binomial_1_f)

plot(output_model_binomial_1_e, asFactor = TRUE)

testDispersion(output_model_binomial_1_e)

# let's test the effects of the main factors 

# Table 1 – Summary results of the linear mixed-effects model testing the preference of chameleon prawns (Hippolyte varians) when given a choice between native and non-native seaweeds. Choice was set as a binary variable (coded as 0 when the prawn chose the non-native seaweed and 1 when it chose the native seaweed) and the comparisons were made between prawn colour types (green and red), the non-native seaweed species (pink harpoon weed Asparagopsis armata and brown wireweed Sargassum muticum), the colour matching to the native seaweed (no and yes), and the choice time (first choice and choice after 10 minutes). 

summary(model_binomial_1_e)

pairs(emmeans(model_binomial_1_e, "native_match"))

# the level of colour matching of the native seaweed is significant

# Graphics ---------------------------------------------------------------------

## model estimates 

afex_plot_behavioural_choice_glmer <-
  afex_plot(
    model_binomial_1_e, x = "native_match",
    mapping = c("fill"),
    data_geom = geom_jitter,
    data_alpha = 0.5,
    data_color = "#333333",
    data_arg = list(size = 2, shape = 1, height = 0.1, width = 0.05),
    point_arg = list(size = 6, shape = 21, stroke = 1,
                     fill = c("black", "lightgrey")),
    error_arg = list(linewidth = 0.7, width = 0.1, colour = "black"),
    factor_levels = 
      list(native_match = c("yes","no")))

## creating a table with the model estimates 

table_glmer_binomial <- 
  tibble(native_match = afex_plot_behavioural_choice_glmer$data[[1]],
         estimate = afex_plot_behavioural_choice_glmer$data[[2]],
         error = afex_plot_behavioural_choice_glmer$data[[3]],
         lower = afex_plot_behavioural_choice_glmer$data[[11]],
         upper = afex_plot_behavioural_choice_glmer$data[[12]])

# Figure 3 – Behavioural choice of green (n = 167) and red (n = 170) chameleon prawns (Hippolyte varians) between native (red dulse Palmaria palmata and green sea lettuce Ulva lactuca) and non-native seaweed species (pink harpoon weed Asparagopsis armata and brown wireweed Sargassum muticum). Comparisons were made when there was a match between the colour of prawns and the colour of the native seaweed during the trial (i.e. green prawns and green sea lettuce; red prawns and red dulse) or a colour mismatch (i.e. green prawns and red dulse; red prawns and green sea lettuce). 

graph_behavioural_choice <-
  ggplot() +
  geom_jitter(data = data_binomial  |> 
                     drop_na(choice_bin),
              aes(x = native_match, y = choice_bin, colour = native_match),
              size = 2, shape = 16, alpha = 0.5, 
              height = 0.1, width = 0.05) +
  geom_point(data = table_glmer_binomial,
             aes(x = native_match, y = estimate, fill = native_match),
             size = 5, shape = 22, stroke = 1) +
  geom_errorbar(data = table_glmer_binomial,
                aes(x = native_match, y = estimate, 
                    ymin = lower, ymax = upper),
                width = 0.1, linewidth = 0.7, 
                position = position_dodge(0)) +
  labs(y = "Choice for the native seaweed (%)", 
       x = "Colour match to the native seaweed") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2),
                     labels = seq(0, 100, by = 20),
                     limits = c(-0.1, 1.1)) +
  scale_fill_manual(values = c("black", "lightgrey")) +
  scale_colour_manual(values = c("black", "lightgrey")) +
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
        panel.grid.minor = element_blank()) +
  geom_text(data = table_glmer_binomial,
            aes(x = native_match),
           label = "towards native", x = 2.35, y = 1, 
            size = 3, fontface = "bold", colour = "black") +
  geom_text(data = table_glmer_binomial,
            aes(x = native_match),
            label = "towards non-native", x = 2.35, y = 0, 
            size = 3, fontface = "bold", colour = "black") 

ggsave("Figure 3.png", graph_behavioural_choice,
       height = 15, width = 20, unit = "cm", dpi = 600)