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

## Create new variables 
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

# Compute Standard Error of the Mean
stderror <- function(x) sd(x)/sqrt(length(x))

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
                               auditory_sub_AF_easy) |> 
  mutate(across(c("leak_TP", "leak_IF", "leak_AF"), forcats::as_factor)) |> 
  mutate(leak_TP = fct_relevel(leak_TP, "predBTpInf", after = Inf),
         leak_IF = fct_relevel(leak_IF, "predBIfInf", after = Inf),
         leak_AF = fct_relevel(leak_AF, "predBAfInf", after = Inf))


auditorylong_difficult <- bind_cols(auditory_sub_TP_difficult, 
                                    auditory_sub_IF_difficult, 
                                    auditory_sub_AF_difficult) |> 
  mutate(across(c("leak_TP", "leak_IF", "leak_AF"), forcats::as_factor)) |> 
  mutate(leak_TP = fct_relevel(leak_TP, "predBTpInf", after = Inf),
         leak_IF = fct_relevel(leak_IF, "predBIfInf", after = Inf),
         leak_AF = fct_relevel(leak_AF, "predBAfInf", after = Inf))


# LLhood easy
auditory_LearningModels_easy <- auditorylong_easy |> 
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
  
auditory_LearningModels_sub1 <- auditory_LearningModels_easy |> 
  pivot_longer(cols = c("TP", "IF", "AF", "Stim"),
               names_to = "learningmodel",
               values_to = "LLhood") |> 
  mutate(learningmodel = forcats::as_factor(learningmodel))

auditory_LearningModels_sub2 <- auditory_LearningModels_easy |> 
  select(c("SEM_TP", "SEM_IF", "SEM_AF", "SEM_Stim")) |>
  pivot_longer(cols = c("SEM_TP", "SEM_IF", "SEM_AF", "SEM_Stim"),
               names_to = "SEM_LLhood_Names",
               values_to = "SEM_LLhood")

auditory_LearningModels_easy <- bind_cols(auditory_LearningModels_sub1[, c("leak_TP", "learningmodel" ,"LLhood")], 
                                     auditory_LearningModels_sub2[, "SEM_LLhood"])

auditory_LearningModels_easy <- auditory_LearningModels_easy |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(c(leak_TP, leak_numbers))) |> 
  relocate(Omega, .after = learningmodel)


# LLhood difficult
auditory_LearningModels_difficult <- auditorylong_difficult |> 
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

auditory_LearningModels_sub1 <- auditory_LearningModels_difficult |> 
  pivot_longer(cols = c("TP", "IF", "AF", "Stim"),
               names_to = "learningmodel",
               values_to = "LLhood") |> 
  mutate(learningmodel = forcats::as_factor(learningmodel))

auditory_LearningModels_sub2 <- auditory_LearningModels_difficult |> 
  select(c("SEM_TP", "SEM_IF", "SEM_AF", "SEM_Stim")) |>
  pivot_longer(cols = c("SEM_TP", "SEM_IF", "SEM_AF", "SEM_Stim"),
               names_to = "SEM_LLhood_Names",
               values_to = "SEM_LLhood")

auditory_LearningModels_difficult <- bind_cols(auditory_LearningModels_sub1[, c("leak_TP", "learningmodel" ,"LLhood")], 
                                          auditory_LearningModels_sub2[, "SEM_LLhood"])

auditory_LearningModels_difficult <- auditory_LearningModels_difficult |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(c(leak_TP, leak_numbers))) |> 
  relocate(Omega, .after = learningmodel)


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
                               vestibular_sub_AF_easy) |> 
  mutate(across(c("leak_TP", "leak_IF", "leak_AF"), forcats::as_factor)) |> 
  mutate(leak_TP = fct_relevel(leak_TP, "predBTpInf", after = Inf),
         leak_IF = fct_relevel(leak_IF, "predBIfInf", after = Inf),
         leak_AF = fct_relevel(leak_AF, "predBAfInf", after = Inf))


vestibularlong_difficult <- bind_cols(vestibular_sub_TP_difficult, 
                                    vestibular_sub_IF_difficult, 
                                    vestibular_sub_AF_difficult) |> 
  mutate(across(c("leak_TP", "leak_IF", "leak_AF"), forcats::as_factor)) |> 
  mutate(leak_TP = fct_relevel(leak_TP, "predBTpInf", after = Inf),
         leak_IF = fct_relevel(leak_IF, "predBIfInf", after = Inf),
         leak_AF = fct_relevel(leak_AF, "predBAfInf", after = Inf))


# LLhood easy
vestibular_LearningModels_easy <- vestibularlong_easy |> 
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

vestibular_LearningModels_sub1 <- vestibular_LearningModels_easy |> 
  pivot_longer(cols = c("TP", "IF", "AF", "Stim"),
               names_to = "learningmodel",
               values_to = "LLhood") |> 
  mutate(learningmodel = forcats::as_factor(learningmodel))

vestibular_LearningModels_sub2 <- vestibular_LearningModels_easy |> 
  select(c("SEM_TP", "SEM_IF", "SEM_AF", "SEM_Stim")) |>
  pivot_longer(cols = c("SEM_TP", "SEM_IF", "SEM_AF", "SEM_Stim"),
               names_to = "SEM_LLhood_Names",
               values_to = "SEM_LLhood")

vestibular_LearningModels_easy <- bind_cols(vestibular_LearningModels_sub1[, c("leak_TP", "learningmodel" ,"LLhood")], 
                                          vestibular_LearningModels_sub2[, "SEM_LLhood"])

vestibular_LearningModels_easy <- vestibular_LearningModels_easy |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(c(leak_TP, leak_numbers))) |> 
  relocate(Omega, .after = learningmodel)


# LLhood difficult
vestibular_LearningModels_difficult <- vestibularlong_difficult |> 
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

vestibular_LearningModels_sub1 <- vestibular_LearningModels_difficult |> 
  pivot_longer(cols = c("TP", "IF", "AF", "Stim"),
               names_to = "learningmodel",
               values_to = "LLhood") |> 
  mutate(learningmodel = forcats::as_factor(learningmodel))

vestibular_LearningModels_sub2 <- vestibular_LearningModels_difficult |> 
  select(c("SEM_TP", "SEM_IF", "SEM_AF", "SEM_Stim")) |>
  pivot_longer(cols = c("SEM_TP", "SEM_IF", "SEM_AF", "SEM_Stim"),
               names_to = "SEM_LLhood_Names",
               values_to = "SEM_LLhood")

vestibular_LearningModels_difficult <- bind_cols(vestibular_LearningModels_sub1[, c("leak_TP", "learningmodel" ,"LLhood")], 
                                               vestibular_LearningModels_sub2[, "SEM_LLhood"])

vestibular_LearningModels_difficult <- vestibular_LearningModels_difficult |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(c(leak_TP, leak_numbers))) |> 
  relocate(Omega, .after = learningmodel)


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
                               visual_sub_AF_easy) |> 
  mutate(across(c("leak_TP", "leak_IF", "leak_AF"), forcats::as_factor)) |> 
  mutate(leak_TP = fct_relevel(leak_TP, "predBTpInf", after = Inf),
         leak_IF = fct_relevel(leak_IF, "predBIfInf", after = Inf),
         leak_AF = fct_relevel(leak_AF, "predBAfInf", after = Inf))


visuallong_difficult <- bind_cols(visual_sub_TP_difficult, 
                                    visual_sub_IF_difficult, 
                                    visual_sub_AF_difficult) |> 
  mutate(across(c("leak_TP", "leak_IF", "leak_AF"), forcats::as_factor)) |> 
  mutate(leak_TP = fct_relevel(leak_TP, "predBTpInf", after = Inf),
         leak_IF = fct_relevel(leak_IF, "predBIfInf", after = Inf),
         leak_AF = fct_relevel(leak_AF, "predBAfInf", after = Inf))


# LLhood easy
visual_LearningModels_easy <- visuallong_easy |> 
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

visual_LearningModels_sub1 <- visual_LearningModels_easy |> 
  pivot_longer(cols = c("TP", "IF", "AF", "Stim"),
               names_to = "learningmodel",
               values_to = "LLhood") |> 
  mutate(learningmodel = forcats::as_factor(learningmodel))

visual_LearningModels_sub2 <- visual_LearningModels_easy |> 
  select(c("SEM_TP", "SEM_IF", "SEM_AF", "SEM_Stim")) |>
  pivot_longer(cols = c("SEM_TP", "SEM_IF", "SEM_AF", "SEM_Stim"),
               names_to = "SEM_LLhood_Names",
               values_to = "SEM_LLhood")

visual_LearningModels_easy <- bind_cols(visual_LearningModels_sub1[, c("leak_TP", "learningmodel" ,"LLhood")], 
                                          visual_LearningModels_sub2[, "SEM_LLhood"])

visual_LearningModels_easy <- visual_LearningModels_easy |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(c(leak_TP, leak_numbers))) |> 
  relocate(Omega, .after = learningmodel)


# LLhood difficult
visual_LearningModels_difficult <- visuallong_difficult |> 
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

visual_LearningModels_sub1 <- visual_LearningModels_difficult |> 
  pivot_longer(cols = c("TP", "IF", "AF", "Stim"),
               names_to = "learningmodel",
               values_to = "LLhood") |> 
  mutate(learningmodel = forcats::as_factor(learningmodel))

visual_LearningModels_sub2 <- visual_LearningModels_difficult |> 
  select(c("SEM_TP", "SEM_IF", "SEM_AF", "SEM_Stim")) |>
  pivot_longer(cols = c("SEM_TP", "SEM_IF", "SEM_AF", "SEM_Stim"),
               names_to = "SEM_LLhood_Names",
               values_to = "SEM_LLhood")

visual_LearningModels_difficult <- bind_cols(visual_LearningModels_sub1[, c("leak_TP", "learningmodel" ,"LLhood")], 
                                               visual_LearningModels_sub2[, "SEM_LLhood"])

visual_LearningModels_difficult <- visual_LearningModels_difficult |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(c(leak_TP, leak_numbers))) |> 
  relocate(Omega, .after = learningmodel)


### === Save data === ###
save(auditory_LearningModels_easy, file = "processed_data/LLhood_LearningModels/auditory_LearningModels_easy.Rda")
save(auditory_LearningModels_difficult, file = "processed_data/LLhood_LearningModels/auditory_LearningModels_difficult.Rda")

save(vestibular_LearningModels_easy, file = "processed_data/LLhood_LearningModels/vestibular_LearningModels_easy.Rda")
save(vestibular_LearningModels_difficult, file = "processed_data/LLhood_LearningModels/vestibular_LearningModels_difficult.Rda")

save(visual_LearningModels_easy, file = "processed_data/LLhood_LearningModels/visual_LearningModels_easy.Rda")
save(visual_LearningModels_difficult, file = "processed_data/LLhood_LearningModels/visual_LearningModels_difficult.Rda")

