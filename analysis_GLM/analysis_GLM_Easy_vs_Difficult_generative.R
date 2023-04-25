# GLM Analysis Easy Vs. Difficult Generative

# Either TP, IF, or AF are tested as predictors for participants' responses
# Here, stimulus intensity is NOT included in an interaction
# Instead, the analysis is performed for easy and difficult trials separately

library(tidyverse)
library(ggtext)
library(patchwork)
library(DescTools)

# Source functions

# Source functions
source("functions/function_GLM.R")

source("functions/function_predictionAccuracy.R")
source("functions/function_permutationTestProbit.R")

# Load data
load("processed_data/allValues/allValuesAuditory_generative.Rda")
load("processed_data/allValues/allValuesVestibular_generative.Rda")
load("processed_data/allValues/allValuesVisual_generative.Rda")

### Check and Reallocate tibbles ###

auditory_generative <- allValuesAuditory_generative 
vestibular_generative <- allValuesVestibular_generative 
visual_generative <- allValuesVisual_generative

### Create Easy and Difficult Dataframes ###

auditory_easy_generative <- auditory_generative |> filter(intensity_ord %in% c("3", "4", "5", "6"))
auditory_difficult_generative <- auditory_generative |> filter(intensity_ord %in% c("1", "2"))

vestibular_easy_generative <- vestibular_generative |> filter(intensity_ord %in% c("4", "5", "6", "7"))
vestibular_difficult_generative <- vestibular_generative |> filter(intensity_ord %in% c("1", "2", "3"))

visual_easy_generative <- visual_generative |> filter(intensity_ord %in% c("0.1", "0.2", "0.4", "0.6"))
visual_difficult_generative <- visual_generative |> filter(intensity_ord %in% c("0.05")) # cannot include 0

# do not add 0%-coherence-motion trials in visual_difficult


### Auditory ###

# Create subdata frame for transition probability
auditory_sub_TP_easy_generative <- auditory_easy_generative |> 
  select(-contains(c("If", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_TP",
               values_to = "TP")

auditory_sub_TP_difficult_generative <- auditory_difficult_generative |> 
  select(-contains(c("If", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_TP",
               values_to = "TP")

# Item frequencey
auditory_sub_IF_easy_generative <- auditory_easy_generative |> 
  select(-contains(c("Tp", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_IF",
               values_to = "IF") |> 
  select(c("leak_IF", "IF"))

auditory_sub_IF_difficult_generative <- auditory_difficult_generative |> 
  select(-contains(c("Tp", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_IF",
               values_to = "IF") |> 
  select(c("leak_IF", "IF"))

# Alternation frequency 
auditory_sub_AF_easy_generative <- auditory_easy_generative |> 
  select(-contains(c("Tp", "If"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_AF",
               values_to = "AF") |> 
  select(c("leak_AF", "AF"))

auditory_sub_AF_difficult_generative <- auditory_difficult_generative |> 
  select(-contains(c("Tp", "If"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_AF",
               values_to = "AF") |> 
  select(c("leak_AF", "AF"))

# Combine data frames
auditorylong_easy_generative <- bind_cols(auditory_sub_TP_easy_generative, 
                                          auditory_sub_IF_easy_generative, 
                                          auditory_sub_AF_easy_generative)

auditorylong_difficult_generative <- bind_cols(auditory_sub_TP_difficult_generative, 
                                               auditory_sub_IF_difficult_generative, 
                                               auditory_sub_AF_difficult_generative)

### Predictor models: TP, IF, AF ### 

# TP - Easy
auditoryTP_easy_generative <- auditorylong_easy_generative |> 
  group_by(subject, leak_TP) |> 
  nest() |> 
  mutate(modelTP = map(data, modelTP)) |> 
  mutate(modelsummaryTP = map(modelTP, summary),
         AIC_TP = map(modelTP, AIC),
         BIC_TP = map(modelTP, BIC),
         Nagelkerke_TP = map(modelTP, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_TP = map(modelTP, function(x) PseudoR2(x, which = "McFaddenAdj")),
         predictionAccuracy_TP = map(modelTP, predictionAccuracy),
         coef_TP = map(modelTP, broom::tidy)) |> 
  unnest(cols = c(AIC_TP, BIC_TP, predictionAccuracy_TP, Nagelkerke_TP, McFaddenAdj_TP)) |> 
  group_by(subject) |> 
  slice_min(BIC_TP) |> 
  mutate(anovaOutput = map(modelTP,  function(x) anova(x, test = 'Chi'))) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# TP - Difficult
auditoryTP_difficult_generative <- auditorylong_difficult_generative |> 
  group_by(subject, leak_TP) |> 
  nest() |> 
  mutate(modelTP = map(data, modelTP)) |> 
  mutate(modelsummaryTP = map(modelTP, summary),
         AIC_TP = map(modelTP, AIC),
         BIC_TP = map(modelTP, BIC),
         Nagelkerke_TP = map(modelTP, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_TP = map(modelTP, function(x) PseudoR2(x, which = "McFaddenAdj")),
         predictionAccuracy_TP = map(modelTP, predictionAccuracy),
         coef_TP = map(modelTP, broom::tidy)) |> 
  unnest(cols = c(AIC_TP, BIC_TP, predictionAccuracy_TP, Nagelkerke_TP, McFaddenAdj_TP)) |> 
  group_by(subject) |> 
  slice_min(BIC_TP) |> 
  mutate(anovaOutput = map(modelTP,  function(x) anova(x, test = 'Chi'))) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF - easy
auditoryIF_easy_generative <- auditorylong_easy_generative |> 
  group_by(subject, leak_IF) |> 
  nest() |> 
  mutate(modelIF = map(data, modelIF)) |> 
  mutate(modelsummaryIF = map(modelIF, summary),
         AIC_IF = map(modelIF, AIC),
         BIC_IF = map(modelIF, BIC),
         Nagelkerke_IF = map(modelIF, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_IF = map(modelIF, function(x) PseudoR2(x, which = "McFaddenAdj")),
         predictionAccuracy_IF = map(modelIF, predictionAccuracy),
         coef_IF = map(modelIF, broom::tidy)) |> 
  unnest(cols = c(AIC_IF, BIC_IF, predictionAccuracy_IF, Nagelkerke_IF, McFaddenAdj_IF)) |> 
  group_by(subject) |> 
  slice_min(BIC_IF) |> 
  mutate(anovaOutput = map(modelIF,  function(x) anova(x, test = 'Chi'))) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF - difficult
auditoryIF_difficult_generative <- auditorylong_difficult_generative |> 
  group_by(subject, leak_IF) |> 
  nest() |> 
  mutate(modelIF = map(data, modelIF)) |> 
  mutate(modelsummaryIF = map(modelIF, summary),
         AIC_IF = map(modelIF, AIC),
         BIC_IF = map(modelIF, BIC),
         Nagelkerke_IF = map(modelIF, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_IF = map(modelIF, function(x) PseudoR2(x, which = "McFaddenAdj")),
         predictionAccuracy_IF = map(modelIF, predictionAccuracy),
         coef_IF = map(modelIF, broom::tidy)) |> 
  unnest(cols = c(AIC_IF, BIC_IF, predictionAccuracy_IF, Nagelkerke_IF, McFaddenAdj_IF)) |> 
  group_by(subject) |> 
  slice_min(BIC_IF) |> 
  mutate(anovaOutput = map(modelIF,  function(x) anova(x, test = 'Chi'))) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF - easy
auditoryAF_easy_generative <- auditorylong_easy_generative |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(modelAF = map(data, modelAF)) |> 
  mutate(modelsummaryAF = map(modelAF, summary),
         AIC_AF = map(modelAF, AIC),
         BIC_AF = map(modelAF, BIC),
         Nagelkerke_AF = map(modelAF, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_AF = map(modelAF, function(x) PseudoR2(x, which = "McFaddenAdj")),
         predictionAccuracy_AF = map(modelAF, predictionAccuracy),
         coef_AF = map(modelAF, broom::tidy)) |> 
  unnest(cols = c(AIC_AF, BIC_AF, predictionAccuracy_AF, Nagelkerke_AF, McFaddenAdj_AF)) |> 
  group_by(subject) |> 
  slice_min(BIC_AF) |> 
  mutate(anovaOutput = map(modelAF,  function(x) anova(x, test = 'Chi'))) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF - difficult
auditoryAF_difficult_generative <- auditorylong_difficult_generative |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(modelAF = map(data, modelAF)) |> 
  mutate(modelsummaryAF = map(modelAF, summary),
         AIC_AF = map(modelAF, AIC),
         BIC_AF = map(modelAF, BIC),
         Nagelkerke_AF = map(modelAF, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_AF = map(modelAF, function(x) PseudoR2(x, which = "McFaddenAdj")),
         predictionAccuracy_AF = map(modelAF, predictionAccuracy),
         coef_AF = map(modelAF, broom::tidy)) |> 
  unnest(cols = c(AIC_AF, BIC_AF, predictionAccuracy_AF, Nagelkerke_AF, McFaddenAdj_AF)) |> 
  group_by(subject) |> 
  slice_min(BIC_AF) |> 
  mutate(anovaOutput = map(modelAF,  function(x) anova(x, test = 'Chi'))) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))



### Vestibular ###

# Create subdata frame for transition probability
vestibular_sub_TP_easy_generative <- vestibular_easy_generative |> 
  select(-contains(c("If", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_TP",
               values_to = "TP")

vestibular_sub_TP_difficult_generative <- vestibular_difficult_generative |> 
  select(-contains(c("If", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_TP",
               values_to = "TP")

# Item frequencey
vestibular_sub_IF_easy_generative <- vestibular_easy_generative |> 
  select(-contains(c("Tp", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_IF",
               values_to = "IF") |> 
  select(c("leak_IF", "IF"))

vestibular_sub_IF_difficult_generative <- vestibular_difficult_generative |> 
  select(-contains(c("Tp", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_IF",
               values_to = "IF") |> 
  select(c("leak_IF", "IF"))

# Alternation frequency 
vestibular_sub_AF_easy_generative <- vestibular_easy_generative |> 
  select(-contains(c("Tp", "If"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_AF",
               values_to = "AF") |> 
  select(c("leak_AF", "AF"))

vestibular_sub_AF_difficult_generative <- vestibular_difficult_generative |> 
  select(-contains(c("Tp", "If"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_AF",
               values_to = "AF") |> 
  select(c("leak_AF", "AF"))

# Combine data frames
vestibularlong_easy_generative <- bind_cols(vestibular_sub_TP_easy_generative, 
                                            vestibular_sub_IF_easy_generative, 
                                            vestibular_sub_AF_easy_generative)

vestibularlong_difficult_generative <- bind_cols(vestibular_sub_TP_difficult_generative, 
                                                 vestibular_sub_IF_difficult_generative, 
                                                 vestibular_sub_AF_difficult_generative)

# TP - easy
vestibularTP_easy_generative <- vestibularlong_easy_generative |> 
  group_by(subject, leak_TP) |> 
  nest() |> 
  mutate(modelTP = map(data, modelTP)) |> 
  mutate(modelsummaryTP = map(modelTP, summary),
         AIC_TP = map(modelTP, AIC),
         BIC_TP = map(modelTP, BIC),
         predictionAccuracy_TP = map(modelTP, predictionAccuracy),
         Nagelkerke_TP = map(modelTP, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_TP = map(modelTP, function(x) PseudoR2(x, which = "McFaddenAdj")),
         predictionAccuracy_TP = map(modelTP, predictionAccuracy),
         coef_TP = map(modelTP, broom::tidy)) |> 
  unnest(cols = c(AIC_TP, BIC_TP, predictionAccuracy_TP, Nagelkerke_TP, McFaddenAdj_TP)) |> 
  group_by(subject) |> 
  slice_min(BIC_TP) |> 
  mutate(anovaOutput = map(modelTP,  function(x) anova(x, test = 'Chi'))) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# TP - difficult 
vestibularTP_difficult_generative <- vestibularlong_difficult_generative |> 
  group_by(subject, leak_TP) |> 
  nest() |> 
  mutate(modelTP = map(data, modelTP)) |> 
  mutate(modelsummaryTP = map(modelTP, summary),
         AIC_TP = map(modelTP, AIC),
         BIC_TP = map(modelTP, BIC),
         predictionAccuracy_TP = map(modelTP, predictionAccuracy),
         Nagelkerke_TP = map(modelTP, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_TP = map(modelTP, function(x) PseudoR2(x, which = "McFaddenAdj")),
         predictionAccuracy_TP = map(modelTP, predictionAccuracy),
         coef_TP = map(modelTP, broom::tidy)) |> 
  unnest(cols = c(AIC_TP, BIC_TP, predictionAccuracy_TP, Nagelkerke_TP, McFaddenAdj_TP)) |> 
  group_by(subject) |> 
  slice_min(BIC_TP) |> 
  mutate(anovaOutput = map(modelTP,  function(x) anova(x, test = 'Chi'))) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF - easy
vestibularIF_easy_generative <- vestibularlong_easy_generative |> 
  group_by(subject, leak_IF) |> 
  nest() |> 
  mutate(modelIF = map(data, modelIF)) |> 
  mutate(modelsummaryIF = map(modelIF, summary),
         AIC_IF = map(modelIF, AIC),
         BIC_IF = map(modelIF, BIC),
         Nagelkerke_IF = map(modelIF, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_IF = map(modelIF, function(x) PseudoR2(x, which = "McFaddenAdj")),
         predictionAccuracy_IF = map(modelIF, predictionAccuracy),
         coef_IF = map(modelIF, broom::tidy)) |> 
  unnest(cols = c(AIC_IF, BIC_IF, predictionAccuracy_IF, Nagelkerke_IF, McFaddenAdj_IF)) |> 
  group_by(subject) |> 
  slice_min(BIC_IF) |> 
  mutate(anovaOutput = map(modelIF,  function(x) anova(x, test = 'Chi'))) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF - difficult
vestibularIF_difficult_generative <- vestibularlong_difficult_generative |> 
  group_by(subject, leak_IF) |> 
  nest() |> 
  mutate(modelIF = map(data, modelIF)) |> 
  mutate(modelsummaryIF = map(modelIF, summary),
         AIC_IF = map(modelIF, AIC),
         BIC_IF = map(modelIF, BIC),
         Nagelkerke_IF = map(modelIF, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_IF = map(modelIF, function(x) PseudoR2(x, which = "McFaddenAdj")),
         predictionAccuracy_IF = map(modelIF, predictionAccuracy),
         coef_IF = map(modelIF, broom::tidy)) |> 
  unnest(cols = c(AIC_IF, BIC_IF, predictionAccuracy_IF, Nagelkerke_IF, McFaddenAdj_IF)) |> 
  group_by(subject) |> 
  slice_min(BIC_IF) |> 
  mutate(anovaOutput = map(modelIF,  function(x) anova(x, test = 'Chi'))) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF - easy
vestibularAF_easy_generative <- vestibularlong_easy_generative |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(modelAF = map(data, modelAF)) |> 
  mutate(modelsummaryAF = map(modelAF, summary),
         AIC_AF = map(modelAF, AIC),
         BIC_AF = map(modelAF, BIC),
         Nagelkerke_AF = map(modelAF, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_AF = map(modelAF, function(x) PseudoR2(x, which = "McFaddenAdj")),
         predictionAccuracy_AF = map(modelAF, predictionAccuracy),
         coef_AF = map(modelAF, broom::tidy)) |> 
  unnest(cols = c(AIC_AF, BIC_AF, predictionAccuracy_AF, Nagelkerke_AF, McFaddenAdj_AF)) |> 
  group_by(subject) |> 
  slice_min(BIC_AF) |> 
  mutate(anovaOutput = map(modelAF,  function(x) anova(x, test = 'Chi'))) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF - difficult
vestibularAF_difficult_generative <- vestibularlong_difficult_generative |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(modelAF = map(data, modelAF)) |> 
  mutate(modelsummaryAF = map(modelAF, summary),
         AIC_AF = map(modelAF, AIC),
         BIC_AF = map(modelAF, BIC),
         Nagelkerke_AF = map(modelAF, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_AF = map(modelAF, function(x) PseudoR2(x, which = "McFaddenAdj")),
         predictionAccuracy_AF = map(modelAF, predictionAccuracy),
         coef_AF = map(modelAF, broom::tidy)) |> 
  unnest(cols = c(AIC_AF, BIC_AF, predictionAccuracy_AF, Nagelkerke_AF, McFaddenAdj_AF)) |> 
  group_by(subject) |> 
  slice_min(BIC_AF) |> 
  mutate(anovaOutput = map(modelAF,  function(x) anova(x, test = 'Chi'))) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))


### Visual ###

# Create subdata frame for transition probability
visual_sub_TP_easy_generative <- visual_easy_generative |> 
  select(-contains(c("If", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_TP",
               values_to = "TP")

visual_sub_TP_difficult_generative <- visual_difficult_generative |> 
  select(-contains(c("If", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_TP",
               values_to = "TP")

# Item frequencey
visual_sub_IF_easy_generative <- visual_easy_generative |> 
  select(-contains(c("Tp", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_IF",
               values_to = "IF") |> 
  select(c("leak_IF", "IF"))

visual_sub_IF_difficult_generative <- visual_difficult_generative |> 
  select(-contains(c("Tp", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_IF",
               values_to = "IF") |> 
  select(c("leak_IF", "IF"))

# Alternation frequency 
visual_sub_AF_easy_generative <- visual_easy_generative |> 
  select(-contains(c("Tp", "If"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_AF",
               values_to = "AF") |> 
  select(c("leak_AF", "AF"))

visual_sub_AF_difficult_generative <- visual_difficult_generative |> 
  select(-contains(c("Tp", "If"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_AF",
               values_to = "AF") |> 
  select(c("leak_AF", "AF"))

# Combine data frames
visuallong_easy_generative <- bind_cols(visual_sub_TP_easy_generative, 
                                        visual_sub_IF_easy_generative, 
                                        visual_sub_AF_easy_generative)

visuallong_difficult_generative <- bind_cols(visual_sub_TP_difficult_generative, 
                                             visual_sub_IF_difficult_generative, 
                                             visual_sub_AF_difficult_generative)

# TP - easy
visualTP_easy_generative <- visuallong_easy_generative |> 
  group_by(subject, leak_TP) |> 
  nest() |> 
  mutate(modelTP = map(data, modelTP)) |> 
  mutate(modelsummaryTP = map(modelTP, summary),
         AIC_TP = map(modelTP, AIC),
         BIC_TP = map(modelTP, BIC),
         Nagelkerke_TP = map(modelTP, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_TP = map(modelTP, function(x) PseudoR2(x, which = "McFaddenAdj")),
         predictionAccuracy_TP = map(modelTP, predictionAccuracy),
         coef_TP = map(modelTP, broom::tidy)) |> 
  unnest(cols = c(AIC_TP, BIC_TP, predictionAccuracy_TP, Nagelkerke_TP, McFaddenAdj_TP)) |> 
  group_by(subject) |> 
  slice_min(BIC_TP) |> 
  mutate(anovaOutput = map(modelTP,  function(x) anova(x, test = 'Chi'))) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# TP - difficult
visualTP_difficult_generative <- visuallong_difficult_generative |> 
  group_by(subject, leak_TP) |> 
  nest() |> 
  mutate(modelTP = map(data, modelTP)) |> 
  mutate(modelsummaryTP = map(modelTP, summary),
         AIC_TP = map(modelTP, AIC),
         BIC_TP = map(modelTP, BIC),
         Nagelkerke_TP = map(modelTP, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_TP = map(modelTP, function(x) PseudoR2(x, which = "McFaddenAdj")),
         predictionAccuracy_TP = map(modelTP, predictionAccuracy),
         coef_TP = map(modelTP, broom::tidy)) |> 
  unnest(cols = c(AIC_TP, BIC_TP, predictionAccuracy_TP, Nagelkerke_TP, McFaddenAdj_TP)) |> 
  group_by(subject) |> 
  slice_min(BIC_TP) |> 
  mutate(anovaOutput = map(modelTP,  function(x) anova(x, test = 'Chi'))) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF - easy
visualIF_easy_generative <- visuallong_easy_generative |> 
  group_by(subject, leak_IF) |> 
  nest() |> 
  mutate(modelIF = map(data, modelIF)) |> 
  mutate(modelsummaryIF = map(modelIF, summary),
         AIC_IF = map(modelIF, AIC),
         BIC_IF = map(modelIF, BIC),
         Nagelkerke_IF = map(modelIF, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_IF = map(modelIF, function(x) PseudoR2(x, which = "McFaddenAdj")),
         predictionAccuracy_IF = map(modelIF, predictionAccuracy),
         coef_IF = map(modelIF, broom::tidy)) |> 
  unnest(cols = c(AIC_IF, BIC_IF, predictionAccuracy_IF, Nagelkerke_IF, McFaddenAdj_IF)) |> 
  group_by(subject) |> 
  slice_min(BIC_IF) |> 
  mutate(anovaOutput = map(modelIF,  function(x) anova(x, test = 'Chi'))) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF - difficult
visualIF_difficult_generative <- visuallong_difficult_generative |> 
  group_by(subject, leak_IF) |> 
  nest() |> 
  mutate(modelIF = map(data, modelIF)) |> 
  mutate(modelsummaryIF = map(modelIF, summary),
         AIC_IF = map(modelIF, AIC),
         BIC_IF = map(modelIF, BIC),
         Nagelkerke_IF = map(modelIF, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_IF = map(modelIF, function(x) PseudoR2(x, which = "McFaddenAdj")),
         predictionAccuracy_IF = map(modelIF, predictionAccuracy),
         coef_IF = map(modelIF, broom::tidy)) |> 
  unnest(cols = c(AIC_IF, BIC_IF, predictionAccuracy_IF, Nagelkerke_IF, McFaddenAdj_IF)) |> 
  group_by(subject) |> 
  slice_min(BIC_IF) |> 
  mutate(anovaOutput = map(modelIF,  function(x) anova(x, test = 'Chi'))) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))


# AF - easy
visualAF_easy_generative <- visuallong_easy_generative |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(modelAF = map(data, modelAF)) |> 
  mutate(modelsummaryAF = map(modelAF, summary),
         AIC_AF = map(modelAF, AIC),
         BIC_AF = map(modelAF, BIC),
         Nagelkerke_AF = map(modelAF, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_AF = map(modelAF, function(x) PseudoR2(x, which = "McFaddenAdj")),
         predictionAccuracy_AF = map(modelAF, predictionAccuracy),
         coef_AF = map(modelAF, broom::tidy)) |> 
  unnest(cols = c(AIC_AF, BIC_AF, predictionAccuracy_AF, Nagelkerke_AF, McFaddenAdj_AF)) |> 
  group_by(subject) |> 
  slice_min(BIC_AF) |> 
  mutate(anovaOutput = map(modelAF,  function(x) anova(x, test = 'Chi'))) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF - difficult
visualAF_difficult_generative <- visuallong_difficult_generative |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(modelAF = map(data, modelAF)) |> 
  mutate(modelsummaryAF = map(modelAF, summary),
         AIC_AF = map(modelAF, AIC),
         BIC_AF = map(modelAF, BIC),
         Nagelkerke_AF = map(modelAF, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_AF = map(modelAF, function(x) PseudoR2(x, which = "McFaddenAdj")),
         predictionAccuracy_AF = map(modelAF, predictionAccuracy),
         coef_AF = map(modelAF, broom::tidy)) |> 
  unnest(cols = c(AIC_AF, BIC_AF, predictionAccuracy_AF, Nagelkerke_AF, McFaddenAdj_AF)) |> 
  group_by(subject) |> 
  slice_min(BIC_AF) |> 
  mutate(anovaOutput = map(modelAF,  function(x) anova(x, test = 'Chi'))) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))




# Save data to processed data
save(auditoryTP_easy_generative, file = "processed_data/GLM_Easy_vs_Difficult_generative/auditoryTP_easy_generative.Rda")
save(auditoryIF_easy_generative, file = "processed_data/GLM_Easy_vs_Difficult_generative/auditoryIF_easy_generative.Rda")
save(auditoryAF_easy_generative, file = "processed_data/GLM_Easy_vs_Difficult_generative/auditoryAF_easy_generative.Rda")

save(auditoryTP_difficult_generative, file = "processed_data/GLM_Easy_vs_Difficult_generative/auditoryTP_difficult_generative.Rda")
save(auditoryIF_difficult_generative, file = "processed_data/GLM_Easy_vs_Difficult_generative/auditoryIF_difficult_generative.Rda")
save(auditoryAF_difficult_generative, file = "processed_data/GLM_Easy_vs_Difficult_generative/auditoryAF_difficult_generative.Rda")

save(vestibularIF_easy_generative, file = "processed_data/GLM_Easy_vs_Difficult_generative/vestibularIF_easy_generative.Rda")
save(vestibularTP_easy_generative, file = "processed_data/GLM_Easy_vs_Difficult_generative/vestibularTP_easy_generative.Rda")
save(vestibularAF_easy_generative, file = "processed_data/GLM_Easy_vs_Difficult_generative/vestibularAF_easy_generative.Rda")

save(vestibularTP_difficult_generative, file = "processed_data/GLM_Easy_vs_Difficult_generative/vestibularTP_difficult_generative.Rda")
save(vestibularIF_difficult_generative, file = "processed_data/GLM_Easy_vs_Difficult_generative/vestibularIF_difficult_generative.Rda")
save(vestibularAF_difficult_generative, file = "processed_data/GLM_Easy_vs_Difficult_generative/vestibularAF_difficult_generative.Rda")

save(visualTP_easy_generative, file = "processed_data/GLM_Easy_vs_Difficult_generative/visualTP_easy_generative.Rda")
save(visualIF_easy_generative, file = "processed_data/GLM_Easy_vs_Difficult_generative/visualIF_easy_generative.Rda")
save(visualAF_easy_generative, file = "processed_data/GLM_Easy_vs_Difficult_generative/visualAF_easy_generative.Rda")

save(visualTP_difficult_generative, file = "processed_data/GLM_Easy_vs_Difficult_generative/visualTP_difficult_generative.Rda")
save(visualIF_difficult_generative, file = "processed_data/GLM_Easy_vs_Difficult_generative/visualIF_difficult_generative.Rda")
save(visualAF_difficult_generative, file = "processed_data/GLM_Easy_vs_Difficult_generative/visualAF_difficult_generative.Rda")


