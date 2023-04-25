# GLM Analysis (generative values were used as input for learning models)

# Either TP, IF, or AF are tested as predictors for participants' responses
# Stimulus intensity, as an ordered factor, is included in an interaction with
# the respective learning model

library(tidyverse)
library(ggtext)
library(patchwork)
library(DescTools)

# Source functions
source("functions/function_GLM_withInteraction.R")

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

### Auditory ###

# Create subdata frame for transition probability
auditory_sub_TP_generative <- auditory_generative |> 
  select(-contains(c("If", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_TP",
               values_to = "TP")

# Item frequencey
auditory_sub_IF_generative <- auditory_generative |> 
  select(-contains(c("Tp", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_IF",
               values_to = "IF") |> 
  select(c("leak_IF", "IF"))

# Alternation frequency 
auditory_sub_AF_generative <- auditory_generative |> 
  select(-contains(c("Tp", "If"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_AF",
               values_to = "AF") |> 
  select(c("leak_AF", "AF"))

# Combine data frames
auditorylong_generative <- bind_cols(auditory_sub_TP_generative, 
                                     auditory_sub_IF_generative, 
                                     auditory_sub_AF_generative)

### Predictor models: TP, IF, AF ### 

# TP
auditoryTP_generative <- auditorylong_generative |> 
  group_by(subject, leak_TP) |> 
  nest() |> 
  mutate(modelTP = map(data, fullModelTP)) |> 
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

# IF
auditoryIF_generative <- auditorylong_generative |> 
  group_by(subject, leak_IF) |> 
  nest() |> 
  mutate(modelIF = map(data, fullModelIF)) |> 
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

# AF
auditoryAF_generative <- auditorylong_generative |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(modelAF = map(data, fullModelAF)) |> 
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
vestibular_sub_TP_generative <- vestibular_generative |> 
  select(-contains(c("If", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_TP",
               values_to = "TP")

# Item frequencey
vestibular_sub_IF_generative <- vestibular_generative |> 
  select(-contains(c("Tp", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_IF",
               values_to = "IF") |> 
  select(c("leak_IF", "IF"))

# Alternation frequency 
vestibular_sub_AF_generative <- vestibular_generative |> 
  select(-contains(c("Tp", "If"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_AF",
               values_to = "AF") |> 
  select(c("leak_AF", "AF"))

# Combine data frames
vestibularlong_generative <- bind_cols(vestibular_sub_TP_generative, 
                                       vestibular_sub_IF_generative, 
                                       vestibular_sub_AF_generative)

# TP
vestibularTP_generative <- vestibularlong_generative |> 
  group_by(subject, leak_TP) |> 
  nest() |> 
  mutate(modelTP = map(data, fullModelTP)) |> 
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

# IF
vestibularIF_generative <- vestibularlong_generative |> 
  group_by(subject, leak_IF) |> 
  nest() |> 
  mutate(modelIF = map(data, fullModelIF)) |> 
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

# AF
vestibularAF_generative <- vestibularlong_generative |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(modelAF = map(data, fullModelAF)) |> 
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

# Remove zero coherence trials
# (important because stimulus is coded and therefore misleading!)

visual_generative <- filter(visual_generative, !(intensity == 0))

# Create subdata frame for transition probability
visual_sub_TP_generative <- visual_generative |> 
  select(-contains(c("If", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_TP",
               values_to = "TP")

# Item frequencey
visual_sub_IF_generative <- visual_generative |> 
  select(-contains(c("Tp", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_IF",
               values_to = "IF") |> 
  select(c("leak_IF", "IF"))

# Alternation frequency 
visual_sub_AF_generative <- visual_generative |> 
  select(-contains(c("Tp", "If"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_AF",
               values_to = "AF") |> 
  select(c("leak_AF", "AF"))

# Combine data frames
visuallong_generative <- bind_cols(visual_sub_TP_generative, 
                                   visual_sub_IF_generative, 
                                   visual_sub_AF_generative)

# TP
visualTP_generative <- visuallong_generative |> 
  group_by(subject, leak_TP) |> 
  nest() |> 
  mutate(modelTP = map(data, fullModelTP)) |> 
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

# IF
visualIF_generative <- visuallong_generative |> 
  group_by(subject, leak_IF) |> 
  nest() |> 
  mutate(modelIF = map(data, fullModelIF)) |> 
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

# AF
visualAF_generative <- visuallong_generative |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(modelAF = map(data, fullModelAF)) |> 
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
save(auditoryTP_generative, file = "processed_data/GLM_generative/auditoryTP_generative.Rda")
save(auditoryIF_generative, file = "processed_data/GLM_generative/auditoryIF_generative.Rda")
save(auditoryAF_generative, file = "processed_data/GLM_generative/auditoryAF_generative.Rda")

save(vestibularTP_generative, file = "processed_data/GLM_generative/vestibularTP_generative.Rda")
save(vestibularIF_generative, file = "processed_data/GLM_generative/vestibularIF_generative.Rda")
save(vestibularAF_generative, file = "processed_data/GLM_generative/vestibularAF_generative.Rda")

save(visualTP_generative, file = "processed_data/GLM_generative/visualTP_generative.Rda")
save(visualIF_generative, file = "processed_data/GLM_generative/visualIF_generative.Rda")
save(visualAF_generative, file = "processed_data/GLM_generative/visualAF_generative.Rda")



