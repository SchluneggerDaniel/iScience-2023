# Data preprocessing Part 2 (generative values)

library(tidyverse)

# Load data
load("processed_data/allValues/allValuesAuditory_before_DPP_pt2_generative.Rda")
load("processed_data/allValues/allValuesVestibular_before_DPP_pt2_generative.Rda")
load("processed_data/allValues/allValuesVisual_before_DPP_pt2_generative.Rda")


### === AUDITORY === ###

# Convert intensity levels such they are meaningful to obtain psychometric curves
allValuesAuditory_generative <- allValuesAuditory_before_DPP_pt2_generative |> 
  mutate(intensity = case_when(intensity == -6 ~ -1,
                               intensity == -5 ~ -2,
                               intensity == -4 ~ -3,
                               intensity == -3 ~ -4,
                               intensity == -2 ~ -5,
                               intensity == -1 ~ -6,
                               intensity ==  1  ~ 6,
                               intensity ==  2  ~ 5,
                               intensity ==  3  ~ 4,
                               intensity ==  4  ~ 3,
                               intensity ==  5  ~ 2,
                               intensity ==  6  ~ 1),
         condition = case_when(condition == "1" ~ "Fully stochastic",
                               condition == "2" ~ "Frequency-biased",
                               condition == "3" ~ "Repetition-biased",
                               condition == "4" ~ "Alternation-biased"),
         condition = factor(condition, levels = c("Fully stochastic",
                                                  "Frequency-biased",
                                                  "Repetition-biased",
                                                  "Alternation-biased")))

# Create a new variale 'intensity_ord' which is needed for the ordered factor levels
allValuesAuditory_generative <- allValuesAuditory_generative |> 
  mutate(intensity_ord = abs(intensity)) |> 
  mutate(intensity_ord = factor(intensity_ord, 
                                ordered = TRUE, 
                                levels = c("1","2","3","4","5","6")))

# Relocate position within df
allValuesAuditory_generative <- allValuesAuditory_generative |> 
  relocate(intensity_ord, .after = intensity)

# Convert Auditory
allValuesAuditory_generative <- allValuesAuditory_generative |> 
  select(-(starts_with(c("predA")))) |> 
  mutate(stimulus = if_else(stimulus == 0, -0.5, 0.5),
         across(c(starts_with(c("predB"))), qlogis)) |> 
  drop_na()


### === VESTIBULAR === ###

allValuesVestibular_generative <- allValuesVestibular_before_DPP_pt2_generative |> 
  mutate(intensity_ord = abs(intensity)) |> 
  mutate(intensity_ord = factor(intensity_ord, 
                                ordered = TRUE, 
                                levels = c("1","2","3","4","5","6","7"))) |> 
    relocate(intensity_ord, .after = intensity)

# Convert Vestibular
allValuesVestibular_generative <- allValuesVestibular_generative |> 
  select(-(starts_with(c("predA")))) |> 
  mutate(stimulus = if_else(stimulus == 0, -0.5, 0.5),
         across(c(starts_with(c("predB"))), qlogis)) |> 
  drop_na()


### === VISUAL === ###

# Create "minus" - function
changeSign <- function(x) x*-1

# Add correct to dataframe
allValuesVisual_generative <- allValuesVisual_before_DPP_pt2_generative |> 
  mutate(correct = ifelse(test = stimulus == 0 & response == 0, yes = 1, 
                          ifelse(test = stimulus == 1 & response == 1, yes = 1, no = 0)),
         intensity = if_else(stimulus == 0, changeSign(intensity), intensity)) |> 
  mutate(intensity_ord = abs(intensity)) |>  
  mutate(intensity_ord = factor(intensity_ord, 
                                ordered = TRUE, 
                                levels = c("0","0.05","0.1","0.2","0.4","0.6"))) |> 
  relocate(any_of(c("intensity_ord", "correct")), .after = intensity)


# Convert Visual
allValuesVisual_generative <- allValuesVisual_generative |> 
  select(-(starts_with(c("predA")))) |> 
  mutate(stimulus = if_else(stimulus == 0, -0.5, 0.5),
         across(c(starts_with(c("predB"))), qlogis)) |> 
  drop_na()
  

# Save data
save(allValuesAuditory_generative, file = "processed_data/allValues/allValuesAuditory_generative.Rda")
save(allValuesVestibular_generative, file = "processed_data/allValues/allValuesVestibular_generative.Rda")
save(allValuesVisual_generative, file = "processed_data/allValues/allValuesVisual_generative.Rda")

