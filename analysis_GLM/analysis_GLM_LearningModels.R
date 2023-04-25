# GLM Analysis (with fixed Omega)

# Either TP, IF, or AF are tested as predictors for participants' responses
# Stimulus intensity, as an ordered factor, is included in an interaction with
# the respective learning model

# Data preprocessing to plot Nagelkerke, when Omega is fixed across learning models

library(tidyverse)
library(ggtext)
library(patchwork)
library(DescTools)

# Source functions
source("functions/function_GLM_withInteraction.R")

source("functions/function_predictionAccuracy.R")
source("functions/function_permutationTestProbit.R")

# Load data
load("processed_data/allValues/allValuesAuditory.Rda")
load("processed_data/allValues/allValuesVestibular.Rda")
load("processed_data/allValues/allValuesVisual.Rda")

stderror <- function(x) sd(x)/sqrt(length(x))

### Check and Reallocate tibbles ###

auditory <- allValuesAuditory 
vestibular <- allValuesVestibular 
visual <- allValuesVisual

### Auditory ###

# Create subdata frame for transition probability
auditory_sub_TP <- auditory |> 
  select(-contains(c("If", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_TP",
               values_to = "TP")

# Item frequencey
auditory_sub_IF <- auditory |> 
  select(-contains(c("Tp", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_IF",
               values_to = "IF") |> 
  select(c("leak_IF", "IF"))

# Alternation frequency 
auditory_sub_AF <- auditory |> 
  select(-contains(c("Tp", "If"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_AF",
               values_to = "AF") |> 
  select(c("leak_AF", "AF"))

# Combine data frames
auditorylong <- bind_cols(auditory_sub_TP, auditory_sub_IF, auditory_sub_AF) |> 
  mutate(across(c("leak_TP", "leak_IF", "leak_AF"), forcats::as_factor))

auditorylong <- auditorylong |> 
  mutate(leak_TP = fct_relevel(leak_TP, "predBTpInf", after = Inf),
         leak_IF = fct_relevel(leak_IF, "predBIfInf", after = Inf),
         leak_AF = fct_relevel(leak_AF, "predBAfInf", after = Inf))

### Predictor models: TP, IF, AF ### 


# TP
auditory_LearningModels <- auditorylong |> 
  group_by(subject, leak_TP) |> # doesn't matter if you take TP, IF or AF
  nest() |> 
  mutate(modelTP = map(data, fullModelTP),
         modelIF = map(data, fullModelIF),
         modelAF = map(data, fullModelAF)) |> 
  mutate(BIC_TP = map(modelTP, BIC),
         BIC_IF = map(modelIF, BIC),
         BIC_AF = map(modelAF, BIC),
         Nagelkerke_TP = map(modelTP, function(x) PseudoR2(x, which = "Nagelkerke")),
         Nagelkerke_IF = map(modelIF, function(x) PseudoR2(x, which = "Nagelkerke")),
         Nagelkerke_AF = map(modelAF, function(x) PseudoR2(x, which = "Nagelkerke"))) |> 
  unnest(cols = c(BIC_TP, BIC_IF, BIC_AF, 
                  Nagelkerke_TP, Nagelkerke_IF, Nagelkerke_AF)) |> 
  group_by(leak_TP) |> # doesn't matter if you take TP, IF or AF
  summarise(TP = mean(BIC_TP),
            IF = mean(BIC_IF),
            AF = mean(BIC_AF),
            SEM_BIC_TP = stderror(BIC_TP),
            SEM_BIC_IF = stderror(BIC_IF),
            SEM_BIC_AF = stderror(BIC_AF),
            meanNagelkerke_TP = mean(Nagelkerke_TP),
            meanNagelkerke_IF = mean(Nagelkerke_IF),
            meanNagelkerke_AF = mean(Nagelkerke_AF),
            SEM_Nagelkerke_TP = stderror(Nagelkerke_TP),
            SEM_Nagelkerke_IF = stderror(Nagelkerke_IF),
            SEM_Nagelkerke_AF = stderror(Nagelkerke_AF))

  
auditory_LearningModels_sub1 <- auditory_LearningModels |> 
  pivot_longer(cols = c("TP", "IF", "AF"),
               names_to = "learningmodel",
               values_to = "BIC") |> 
  mutate(learningmodel = forcats::as_factor(learningmodel))

auditory_LearningModels_sub2 <- auditory_LearningModels |> 
  select(c("SEM_BIC_TP", "SEM_BIC_IF", "SEM_BIC_AF")) |>
  pivot_longer(cols = c("SEM_BIC_TP", "SEM_BIC_IF", "SEM_BIC_AF"),
               names_to = "SEM_BIC_Names",
               values_to = "SEM_BIC")

auditory_LearningModels_sub3 <- auditory_LearningModels |> 
  select(c("meanNagelkerke_TP", "meanNagelkerke_IF", "meanNagelkerke_AF")) |>
  pivot_longer(cols = c("meanNagelkerke_TP", "meanNagelkerke_IF", "meanNagelkerke_AF"),
               names_to = "meanNagelkerke",
               values_to = "Nagelkerke")

auditory_LearningModels_sub4 <- auditory_LearningModels |> 
  select(c("SEM_Nagelkerke_TP", "SEM_Nagelkerke_IF", "SEM_Nagelkerke_AF")) |>
  pivot_longer(cols = c("SEM_Nagelkerke_TP", "SEM_Nagelkerke_IF", "SEM_Nagelkerke_AF"),
               names_to = "SEM_Nagelkerke_Names",
               values_to = "SEM_Nagelkerke")


auditory_LearningModels <- bind_cols(auditory_LearningModels_sub1[, c("leak_TP", "learningmodel" ,"BIC")], 
                                     auditory_LearningModels_sub2[, "SEM_BIC"],
                                     auditory_LearningModels_sub3[, "Nagelkerke"],
                                     auditory_LearningModels_sub4[, "SEM_Nagelkerke"])


auditory_LearningModels <- auditory_LearningModels |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(c(leak_TP, leak_numbers))) |> 
  relocate(Omega, .after = learningmodel)




### Vestibular ###

# Create subdata frame for transition probability
vestibular_sub_TP <- vestibular |> 
  select(-contains(c("If", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_TP",
               values_to = "TP")

# Item frequencey
vestibular_sub_IF <- vestibular |> 
  select(-contains(c("Tp", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_IF",
               values_to = "IF") |> 
  select(c("leak_IF", "IF"))

# Alternation frequency 
vestibular_sub_AF <- vestibular |> 
  select(-contains(c("Tp", "If"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_AF",
               values_to = "AF") |> 
  select(c("leak_AF", "AF"))

# Combine data frames
vestibularlong <- bind_cols(vestibular_sub_TP, vestibular_sub_IF, vestibular_sub_AF) |> 
  mutate(across(c("leak_TP", "leak_IF", "leak_AF"), forcats::as_factor))

vestibularlong <- vestibularlong |> 
  mutate(leak_TP = fct_relevel(leak_TP, "predBTpInf", after = Inf),
         leak_IF = fct_relevel(leak_IF, "predBIfInf", after = Inf),
         leak_AF = fct_relevel(leak_AF, "predBAfInf", after = Inf))

### Predictor models: TP, IF, AF ### 


# TP
vestibular_LearningModels <- vestibularlong |> 
  group_by(subject, leak_TP) |> # doesn't matter if you take TP, IF or AF
  nest() |> 
  mutate(modelTP = map(data, fullModelTP),
         modelIF = map(data, fullModelIF),
         modelAF = map(data, fullModelAF)) |> 
  mutate(BIC_TP = map(modelTP, BIC),
         BIC_IF = map(modelIF, BIC),
         BIC_AF = map(modelAF, BIC),
         Nagelkerke_TP = map(modelTP, function(x) PseudoR2(x, which = "Nagelkerke")),
         Nagelkerke_IF = map(modelIF, function(x) PseudoR2(x, which = "Nagelkerke")),
         Nagelkerke_AF = map(modelAF, function(x) PseudoR2(x, which = "Nagelkerke"))) |> 
  unnest(cols = c(BIC_TP, BIC_IF, BIC_AF, 
                  Nagelkerke_TP, Nagelkerke_IF, Nagelkerke_AF)) |> 
  group_by(leak_TP) |> # doesn't matter if you take TP, IF or AF
  summarise(TP = mean(BIC_TP),
            IF = mean(BIC_IF),
            AF = mean(BIC_AF),
            SEM_BIC_TP = stderror(BIC_TP),
            SEM_BIC_IF = stderror(BIC_IF),
            SEM_BIC_AF = stderror(BIC_AF),
            meanNagelkerke_TP = mean(Nagelkerke_TP),
            meanNagelkerke_IF = mean(Nagelkerke_IF),
            meanNagelkerke_AF = mean(Nagelkerke_AF),
            SEM_Nagelkerke_TP = stderror(Nagelkerke_TP),
            SEM_Nagelkerke_IF = stderror(Nagelkerke_IF),
            SEM_Nagelkerke_AF = stderror(Nagelkerke_AF))


vestibular_LearningModels_sub1 <- vestibular_LearningModels |> 
  pivot_longer(cols = c("TP", "IF", "AF"),
               names_to = "learningmodel",
               values_to = "BIC") |> 
  mutate(learningmodel = forcats::as_factor(learningmodel))

vestibular_LearningModels_sub2 <- vestibular_LearningModels |> 
  select(c("SEM_BIC_TP", "SEM_BIC_IF", "SEM_BIC_AF")) |>
  pivot_longer(cols = c("SEM_BIC_TP", "SEM_BIC_IF", "SEM_BIC_AF"),
               names_to = "SEM_BIC_Names",
               values_to = "SEM_BIC")

vestibular_LearningModels_sub3 <- vestibular_LearningModels |> 
  select(c("meanNagelkerke_TP", "meanNagelkerke_IF", "meanNagelkerke_AF")) |>
  pivot_longer(cols = c("meanNagelkerke_TP", "meanNagelkerke_IF", "meanNagelkerke_AF"),
               names_to = "meanNagelkerke",
               values_to = "Nagelkerke")

vestibular_LearningModels_sub4 <- vestibular_LearningModels |> 
  select(c("SEM_Nagelkerke_TP", "SEM_Nagelkerke_IF", "SEM_Nagelkerke_AF")) |>
  pivot_longer(cols = c("SEM_Nagelkerke_TP", "SEM_Nagelkerke_IF", "SEM_Nagelkerke_AF"),
               names_to = "SEM_Nagelkerke_Names",
               values_to = "SEM_Nagelkerke")


vestibular_LearningModels <- bind_cols(vestibular_LearningModels_sub1[, c("leak_TP", "learningmodel", "BIC")], 
                                     vestibular_LearningModels_sub2[, "SEM_BIC"],
                                     vestibular_LearningModels_sub3[, "Nagelkerke"],
                                     vestibular_LearningModels_sub4[, "SEM_Nagelkerke"])


vestibular_LearningModels <- vestibular_LearningModels |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(c(leak_TP, leak_numbers))) |> 
  relocate(Omega, .after = learningmodel)






### Visual ###

# Create subdata frame for transition probability
visual_sub_TP <- visual |> 
  select(-contains(c("If", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_TP",
               values_to = "TP")

# Item frequencey
visual_sub_IF <- visual |> 
  select(-contains(c("Tp", "Af"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_IF",
               values_to = "IF") |> 
  select(c("leak_IF", "IF"))

# Alternation frequency 
visual_sub_AF <- visual |> 
  select(-contains(c("Tp", "If"))) |> 
  pivot_longer(cols = starts_with("predB"),
               names_to = "leak_AF",
               values_to = "AF") |> 
  select(c("leak_AF", "AF"))

# Combine data frames
visuallong <- bind_cols(visual_sub_TP, visual_sub_IF, visual_sub_AF) |> 
  mutate(across(c("leak_TP", "leak_IF", "leak_AF"), forcats::as_factor))

visuallong <- visuallong |> 
  mutate(leak_TP = fct_relevel(leak_TP, "predBTpInf", after = Inf),
         leak_IF = fct_relevel(leak_IF, "predBIfInf", after = Inf),
         leak_AF = fct_relevel(leak_AF, "predBAfInf", after = Inf))

### Predictor models: TP, IF, AF ### 


# TP
visual_LearningModels <- visuallong |> 
  group_by(subject, leak_TP) |> # doesn't matter if you take TP, IF or AF
  nest() |> 
  mutate(modelTP = map(data, fullModelTP),
         modelIF = map(data, fullModelIF),
         modelAF = map(data, fullModelAF)) |> 
  mutate(BIC_TP = map(modelTP, BIC),
         BIC_IF = map(modelIF, BIC),
         BIC_AF = map(modelAF, BIC),
         Nagelkerke_TP = map(modelTP, function(x) PseudoR2(x, which = "Nagelkerke")),
         Nagelkerke_IF = map(modelIF, function(x) PseudoR2(x, which = "Nagelkerke")),
         Nagelkerke_AF = map(modelAF, function(x) PseudoR2(x, which = "Nagelkerke"))) |> 
  unnest(cols = c(BIC_TP, BIC_IF, BIC_AF, 
                  Nagelkerke_TP, Nagelkerke_IF, Nagelkerke_AF)) |> 
  group_by(leak_TP) |> # doesn't matter if you take TP, IF or AF
  summarise(TP = mean(BIC_TP),
            IF = mean(BIC_IF),
            AF = mean(BIC_AF),
            SEM_BIC_TP = stderror(BIC_TP),
            SEM_BIC_IF = stderror(BIC_IF),
            SEM_BIC_AF = stderror(BIC_AF),
            meanNagelkerke_TP = mean(Nagelkerke_TP),
            meanNagelkerke_IF = mean(Nagelkerke_IF),
            meanNagelkerke_AF = mean(Nagelkerke_AF),
            SEM_Nagelkerke_TP = stderror(Nagelkerke_TP),
            SEM_Nagelkerke_IF = stderror(Nagelkerke_IF),
            SEM_Nagelkerke_AF = stderror(Nagelkerke_AF))


visual_LearningModels_sub1 <- visual_LearningModels |> 
  pivot_longer(cols = c("TP", "IF", "AF"),
               names_to = "learningmodel",
               values_to = "BIC") |> 
  mutate(learningmodel = forcats::as_factor(learningmodel))

visual_LearningModels_sub2 <- visual_LearningModels |> 
  select(c("SEM_BIC_TP", "SEM_BIC_IF", "SEM_BIC_AF")) |>
  pivot_longer(cols = c("SEM_BIC_TP", "SEM_BIC_IF", "SEM_BIC_AF"),
               names_to = "SEM_BIC_Names",
               values_to = "SEM_BIC")

visual_LearningModels_sub3 <- visual_LearningModels |> 
  select(c("meanNagelkerke_TP", "meanNagelkerke_IF", "meanNagelkerke_AF")) |>
  pivot_longer(cols = c("meanNagelkerke_TP", "meanNagelkerke_IF", "meanNagelkerke_AF"),
               names_to = "meanNagelkerke",
               values_to = "Nagelkerke")

visual_LearningModels_sub4 <- visual_LearningModels |> 
  select(c("SEM_Nagelkerke_TP", "SEM_Nagelkerke_IF", "SEM_Nagelkerke_AF")) |>
  pivot_longer(cols = c("SEM_Nagelkerke_TP", "SEM_Nagelkerke_IF", "SEM_Nagelkerke_AF"),
               names_to = "SEM_Nagelkerke_Names",
               values_to = "SEM_Nagelkerke")


visual_LearningModels <- bind_cols(visual_LearningModels_sub1[, c("leak_TP" , "learningmodel" ,"BIC")], 
                                       visual_LearningModels_sub2[, "SEM_BIC"],
                                       visual_LearningModels_sub3[, "Nagelkerke"],
                                       visual_LearningModels_sub4[, "SEM_Nagelkerke"])



visual_LearningModels <- visual_LearningModels |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(c(leak_TP, leak_numbers))) |> 
  relocate(Omega, .after = learningmodel)


# Save data
save(auditory_LearningModels, file = "processed_data/GLM_LearningModels/auditory_LearningModels.Rda")
save(vestibular_LearningModels, file = "processed_data/GLM_LearningModels/vestibular_LearningModels.Rda")
save(visual_LearningModels, file = "processed_data/GLM_LearningModels/visual_LearningModels.Rda")

