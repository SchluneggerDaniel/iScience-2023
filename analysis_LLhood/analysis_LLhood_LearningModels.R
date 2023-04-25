# Log-Likelihood Analysis for TP, IF and AF when Omega is fixed

# Data preprocessing to plot LLhood, when Omega is fixed across learning models

library(tidyverse)
library(ggtext)
library(patchwork)
library(DescTools)

# Source functions
source("functions/function_LogLikelihood.R")

# Load data
load("processed_data/allValues/allValuesAuditory.Rda")
load("processed_data/allValues/allValuesVestibular.Rda")
load("processed_data/allValues/allValuesVisual.Rda")

# Compute Standard Error of the Mean
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


# LLhood
auditory_LearningModels <- auditorylong |> 
  group_by(subject, leak_TP) |> # doesn't matter if you take TP, IF or AF
  nest() |> 
  mutate(LLhood_TP = map(data, logLike_TP),
         LLhood_IF = map(data, logLike_IF),
         LLhood_AF = map(data, logLike_AF),
         LLhood_Stim = map(data, logLike_Stim)) |> 
  unnest(cols = c(LLhood_TP, LLhood_IF, LLhood_AF, LLhood_Stim)) |> 
  group_by(leak_TP) |> # doesn't matter if you take TP, IF or AF
  summarise(TP = mean(LLhood_TP),
            IF = mean(LLhood_IF),
            AF = mean(LLhood_AF),
            Stim = mean(LLhood_Stim),
            SEM_TP = stderror(LLhood_TP),
            SEM_IF = stderror(LLhood_IF),
            SEM_AF = stderror(LLhood_AF),
            SEM_Stim = stderror(LLhood_Stim))
  
auditory_LearningModels_sub1 <- auditory_LearningModels |> 
  pivot_longer(cols = c("TP", "IF", "AF", "Stim"),
               names_to = "learningmodel",
               values_to = "LLhood") |> 
  mutate(learningmodel = forcats::as_factor(learningmodel))

auditory_LearningModels_sub2 <- auditory_LearningModels |> 
  select(c("SEM_TP", "SEM_IF", "SEM_AF", "SEM_Stim")) |>
  pivot_longer(cols = c("SEM_TP", "SEM_IF", "SEM_AF", "SEM_Stim"),
               names_to = "SEM_LLhood_Names",
               values_to = "SEM_LLhood")

auditory_LearningModels <- bind_cols(auditory_LearningModels_sub1[, c("leak_TP", "learningmodel" ,"LLhood")], 
                                     auditory_LearningModels_sub2[, "SEM_LLhood"])

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

# LLhood
vestibular_LearningModels <- vestibularlong |> 
  group_by(subject, leak_TP) |> # doesn't matter if you take TP, IF or AF
  nest() |> 
  mutate(LLhood_TP = map(data, logLike_TP),
         LLhood_IF = map(data, logLike_IF),
         LLhood_AF = map(data, logLike_AF),
         LLhood_Stim = map(data, logLike_Stim)) |> 
  unnest(cols = c(LLhood_TP, LLhood_IF, LLhood_AF, LLhood_Stim)) |> 
  group_by(leak_TP) |> # doesn't matter if you take TP, IF or AF
  summarise(TP = mean(LLhood_TP),
            IF = mean(LLhood_IF),
            AF = mean(LLhood_AF),
            Stim = mean(LLhood_Stim),
            SEM_TP = stderror(LLhood_TP),
            SEM_IF = stderror(LLhood_IF),
            SEM_AF = stderror(LLhood_AF),
            SEM_Stim = stderror(LLhood_Stim))

vestibular_LearningModels_sub1 <- vestibular_LearningModels |> 
  pivot_longer(cols = c("TP", "IF", "AF", "Stim"),
               names_to = "learningmodel",
               values_to = "LLhood") |> 
  mutate(learningmodel = forcats::as_factor(learningmodel))

vestibular_LearningModels_sub2 <- vestibular_LearningModels |> 
  select(c("SEM_TP", "SEM_IF", "SEM_AF", "SEM_Stim")) |>
  pivot_longer(cols = c("SEM_TP", "SEM_IF", "SEM_AF", "SEM_Stim"),
               names_to = "SEM_LLhood_Names",
               values_to = "SEM_LLhood")

vestibular_LearningModels <- bind_cols(vestibular_LearningModels_sub1[, c("leak_TP", "learningmodel" ,"LLhood")], 
                                     vestibular_LearningModels_sub2[, "SEM_LLhood"])

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


# LLhood
visual_LearningModels <- visuallong |> 
  group_by(subject, leak_TP) |> # doesn't matter if you take TP, IF or AF
  nest() |> 
  mutate(LLhood_TP = map(data, logLike_TP),
         LLhood_IF = map(data, logLike_IF),
         LLhood_AF = map(data, logLike_AF),
         LLhood_Stim = map(data, logLike_Stim)) |> 
  unnest(cols = c(LLhood_TP, LLhood_IF, LLhood_AF, LLhood_Stim)) |> 
  group_by(leak_TP) |> # doesn't matter if you take TP, IF or AF
  summarise(TP = mean(LLhood_TP),
            IF = mean(LLhood_IF),
            AF = mean(LLhood_AF),
            Stim = mean(LLhood_Stim),
            SEM_TP = stderror(LLhood_TP),
            SEM_IF = stderror(LLhood_IF),
            SEM_AF = stderror(LLhood_AF),
            SEM_Stim = stderror(LLhood_Stim))

visual_LearningModels_sub1 <- visual_LearningModels |> 
  pivot_longer(cols = c("TP", "IF", "AF", "Stim"),
               names_to = "learningmodel",
               values_to = "LLhood") |> 
  mutate(learningmodel = forcats::as_factor(learningmodel))

visual_LearningModels_sub2 <- visual_LearningModels |> 
  select(c("SEM_TP", "SEM_IF", "SEM_AF", "SEM_Stim")) |>
  pivot_longer(cols = c("SEM_TP", "SEM_IF", "SEM_AF", "SEM_Stim"),
               names_to = "SEM_LLhood_Names",
               values_to = "SEM_LLhood")

visual_LearningModels <- bind_cols(visual_LearningModels_sub1[, c("leak_TP", "learningmodel" ,"LLhood")], 
                                     visual_LearningModels_sub2[, "SEM_LLhood"])

visual_LearningModels <- visual_LearningModels |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(c(leak_TP, leak_numbers))) |> 
  relocate(Omega, .after = learningmodel)


### === Save data === ###

save(auditory_LearningModels, file = "processed_data/LLhood_LearningModels/auditory_LearningModels.Rda")
save(vestibular_LearningModels, file = "processed_data/LLhood_LearningModels/vestibular_LearningModels.Rda")
save(visual_LearningModels, file = "processed_data/LLhood_LearningModels/visual_LearningModels.Rda")

