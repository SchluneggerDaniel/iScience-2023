# GLM Analysis Easy Vs. Difficult

# Either TP, IF, or AF are tested as predictors for participants' responses
# Here, stimulus intensity is NOT included in an interaction
# Instead, the analysis is performed for easy and difficult trials separately

library(tidyverse)
library(ggtext)
library(patchwork)
library(DescTools)

# Source functions
source("functions/function_GLM.R")

source("functions/function_predictionAccuracy.R")
source("functions/function_permutationTestProbit.R")

# Load data
load("processed_data/allValues/allValuesAuditory.Rda")
load("processed_data/allValues/allValuesVestibular.Rda")
load("processed_data/allValues/allValuesVisual.Rda")
### Check and Reallocate tibbles ###

auditory <- allValuesAuditory 
vestibular <- allValuesVestibular 
visual <- allValuesVisual

### Create Easy and Difficult Dataframes ###

auditory_easy <- auditory |> filter(intensity_ord %in% c("3", "4", "5", "6"))
auditory_difficult <- auditory |> filter(intensity_ord %in% c("1", "2"))

vestibular_easy <- vestibular |> filter(intensity_ord %in% c("4", "5", "6", "7"))
vestibular_difficult <- vestibular |> filter(intensity_ord %in% c("1", "2", "3"))

visual_easy <- visual |> filter(intensity_ord %in% c("0.1", "0.2", "0.4", "0.6"))
visual_difficult <- visual |> filter(intensity_ord %in% c("0.05")) # cannot include 0

# do not add 0%-coherence-motion trials in visual_difficult

### Auditory ###

# Create subdata frame for transition probability
auditory_sub_TP_easy <- auditory_easy |> 
  select(-contains(c("If", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_TP",
               values_to = "TP")

auditory_sub_TP_difficult <- auditory_difficult |> 
  select(-contains(c("If", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_TP",
               values_to = "TP")

# Item frequencey
auditory_sub_IF_easy <- auditory_easy |> 
  select(-contains(c("Tp", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_IF",
               values_to = "IF") |> 
  select(c("leak_IF", "IF"))

auditory_sub_IF_difficult <- auditory_difficult |> 
  select(-contains(c("Tp", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_IF",
               values_to = "IF") |> 
  select(c("leak_IF", "IF"))

# Alternation frequency 
auditory_sub_AF_easy <- auditory_easy |> 
  select(-contains(c("Tp", "If"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_AF",
               values_to = "AF") |> 
  select(c("leak_AF", "AF"))

auditory_sub_AF_difficult <- auditory_difficult |> 
  select(-contains(c("Tp", "If"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_AF",
               values_to = "AF") |> 
  select(c("leak_AF", "AF"))

# Combine data frames
auditorylong_easy <- bind_cols(auditory_sub_TP_easy, 
                               auditory_sub_IF_easy, 
                               auditory_sub_AF_easy)

auditorylong_difficult <- bind_cols(auditory_sub_TP_difficult, 
                                    auditory_sub_IF_difficult, 
                                    auditory_sub_AF_difficult)

### Predictor models: TP, IF, AF ### 

# TP - Easy
auditoryTP_easy <- auditorylong_easy |> 
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
auditoryTP_difficult <- auditorylong_difficult |> 
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
auditoryIF_easy <- auditorylong_easy |> 
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
auditoryIF_difficult <- auditorylong_difficult |> 
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
auditoryAF_easy <- auditorylong_easy |> 
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
auditoryAF_difficult <- auditorylong_difficult |> 
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
vestibular_sub_TP_easy <- vestibular_easy |> 
  select(-contains(c("If", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_TP",
               values_to = "TP")

vestibular_sub_TP_difficult <- vestibular_difficult |> 
  select(-contains(c("If", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_TP",
               values_to = "TP")

# Item frequencey
vestibular_sub_IF_easy <- vestibular_easy |> 
  select(-contains(c("Tp", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_IF",
               values_to = "IF") |> 
  select(c("leak_IF", "IF"))

vestibular_sub_IF_difficult <- vestibular_difficult |> 
  select(-contains(c("Tp", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_IF",
               values_to = "IF") |> 
  select(c("leak_IF", "IF"))

# Alternation frequency 
vestibular_sub_AF_easy <- vestibular_easy |> 
  select(-contains(c("Tp", "If"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_AF",
               values_to = "AF") |> 
  select(c("leak_AF", "AF"))

vestibular_sub_AF_difficult <- vestibular_difficult |> 
  select(-contains(c("Tp", "If"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_AF",
               values_to = "AF") |> 
  select(c("leak_AF", "AF"))

# Combine data frames
vestibularlong_easy <- bind_cols(vestibular_sub_TP_easy, 
                                 vestibular_sub_IF_easy, 
                                 vestibular_sub_AF_easy)

vestibularlong_difficult <- bind_cols(vestibular_sub_TP_difficult, 
                                      vestibular_sub_IF_difficult, 
                                      vestibular_sub_AF_difficult)

# TP - easy
vestibularTP_easy <- vestibularlong_easy |> 
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
vestibularTP_difficult <- vestibularlong_difficult |> 
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
vestibularIF_easy <- vestibularlong_easy |> 
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
vestibularIF_difficult <- vestibularlong_difficult |> 
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
vestibularAF_easy <- vestibularlong_easy |> 
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
vestibularAF_difficult <- vestibularlong_difficult |> 
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
visual_sub_TP_easy <- visual_easy |> 
  select(-contains(c("If", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_TP",
               values_to = "TP")

visual_sub_TP_difficult <- visual_difficult |> 
  select(-contains(c("If", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_TP",
               values_to = "TP")

# Item frequencey
visual_sub_IF_easy <- visual_easy |> 
  select(-contains(c("Tp", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_IF",
               values_to = "IF") |> 
  select(c("leak_IF", "IF"))

visual_sub_IF_difficult <- visual_difficult |> 
  select(-contains(c("Tp", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_IF",
               values_to = "IF") |> 
  select(c("leak_IF", "IF"))

# Alternation frequency 
visual_sub_AF_easy <- visual_easy |> 
  select(-contains(c("Tp", "If"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_AF",
               values_to = "AF") |> 
  select(c("leak_AF", "AF"))

visual_sub_AF_difficult <- visual_difficult |> 
  select(-contains(c("Tp", "If"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_AF",
               values_to = "AF") |> 
  select(c("leak_AF", "AF"))

# Combine data frames
visuallong_easy <- bind_cols(visual_sub_TP_easy, 
                             visual_sub_IF_easy, 
                             visual_sub_AF_easy)

visuallong_difficult <- bind_cols(visual_sub_TP_difficult, 
                                  visual_sub_IF_difficult, 
                                  visual_sub_AF_difficult)

# TP - easy
visualTP_easy <- visuallong_easy |> 
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
visualTP_difficult <- visuallong_difficult |> 
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
visualIF_easy <- visuallong_easy |> 
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
visualIF_difficult <- visuallong_difficult |> 
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
visualAF_easy <- visuallong_easy |> 
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
visualAF_difficult <- visuallong_difficult |> 
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
save(auditoryTP_easy, file = "processed_data/GLM_Easy_vs_Difficult/auditoryTP_easy.Rda")
save(auditoryIF_easy, file = "processed_data/GLM_Easy_vs_Difficult/auditoryIF_easy.Rda")
save(auditoryAF_easy, file = "processed_data/GLM_Easy_vs_Difficult/auditoryAF_easy.Rda")

save(auditoryTP_difficult, file = "processed_data/GLM_Easy_vs_Difficult/auditoryTP_difficult.Rda")
save(auditoryIF_difficult, file = "processed_data/GLM_Easy_vs_Difficult/auditoryIF_difficult.Rda")
save(auditoryAF_difficult, file = "processed_data/GLM_Easy_vs_Difficult/auditoryAF_difficult.Rda")

save(vestibularIF_easy, file = "processed_data/GLM_Easy_vs_Difficult/vestibularIF_easy.Rda")
save(vestibularTP_easy, file = "processed_data/GLM_Easy_vs_Difficult/vestibularTP_easy.Rda")
save(vestibularAF_easy, file = "processed_data/GLM_Easy_vs_Difficult/vestibularAF_easy.Rda")

save(vestibularTP_difficult, file = "processed_data/GLM_Easy_vs_Difficult/vestibularTP_difficult.Rda")
save(vestibularIF_difficult, file = "processed_data/GLM_Easy_vs_Difficult/vestibularIF_difficult.Rda")
save(vestibularAF_difficult, file = "processed_data/GLM_Easy_vs_Difficult/vestibularAF_difficult.Rda")

save(visualTP_easy, file = "processed_data/GLM_Easy_vs_Difficult/visualTP_easy.Rda")
save(visualIF_easy, file = "processed_data/GLM_Easy_vs_Difficult/visualIF_easy.Rda")
save(visualAF_easy, file = "processed_data/GLM_Easy_vs_Difficult/visualAF_easy.Rda")

save(visualTP_difficult, file = "processed_data/GLM_Easy_vs_Difficult/visualTP_difficult.Rda")
save(visualIF_difficult, file = "processed_data/GLM_Easy_vs_Difficult/visualIF_difficult.Rda")
save(visualAF_difficult, file = "processed_data/GLM_Easy_vs_Difficult/visualAF_difficult.Rda")



