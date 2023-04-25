# Loglikelihood approach (generative)

# What is done:
# --> loglik <- pbinom(1, prob = probs, q = y, log.p = TRUE) |> sum()

# In this script: 
# (1) Per condition (i.e. rep, alt, etc.) in auditory & visual

# NOTE: Data is backtransformed within the loglik-functions with plogis()
# Because for the GLM-Analyses, data was transformed using qlogis()

# perCondition means: separately for each condition

library(tidyverse)

# Load data
load("processed_data/allValues/allValuesAuditory_generative.Rda")
load("processed_data/allValues/allValuesVestibular_generative.Rda")
load("processed_data/allValues/allValuesVisual_generative.Rda")

# Source / Create Functions
source("functions/function_LogLikelihood.R")

## Create new variables 
auditory_generative <- allValuesAuditory_generative
vestibular_generative <- allValuesVestibular_generative 
visual_generative <- allValuesVisual_generative

### === Auditory === ###

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

# TP
auditoryTP_loglik_generative <- auditorylong_generative |> 
  group_by(subject, leak_TP) |> 
  nest() |> 
  mutate(loglikelihoodTP = map(data, logLike_TP)) |> 
  unnest(loglikelihoodTP) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodTP) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# TP per Condition
auditoryTP_perCondition_loglik_generative <- auditorylong_generative |> 
  group_by(subject, leak_TP, condition) |> 
  nest() |> 
  mutate(loglikelihoodTP = map(data, logLike_TP)) |> 
  unnest(loglikelihoodTP) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodTP) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF
auditoryIF_loglik_generative <- auditorylong_generative |> 
  group_by(subject, leak_IF) |> 
  nest() |> 
  mutate(loglikelihoodIF = map(data, logLike_IF)) |> 
  unnest(loglikelihoodIF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodIF) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF per Condition
auditoryIF_perCondition_loglik_generative <- auditorylong_generative |> 
  group_by(subject, leak_IF, condition) |> 
  nest() |> 
  mutate(loglikelihoodIF = map(data, logLike_IF)) |> 
  unnest(loglikelihoodIF) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodIF) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF
auditoryAF_loglik_generative <- auditorylong_generative |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodAF) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF per Condition
auditoryAF_perCondition_loglik_generative <- auditorylong_generative |> 
  group_by(subject, leak_AF, condition) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodAF) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))


### === Vestibular === ###

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
vestibularTP_loglik_generative <- vestibularlong_generative |> 
  group_by(subject, leak_TP) |> 
  nest() |> 
  mutate(loglikelihoodTP = map(data, logLike_TP)) |> 
  unnest(loglikelihoodTP) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodTP) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF
vestibularIF_loglik_generative <- vestibularlong_generative |> 
  group_by(subject, leak_IF) |> 
  nest() |> 
  mutate(loglikelihoodIF = map(data, logLike_IF)) |> 
  unnest(loglikelihoodIF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodIF) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF
vestibularAF_loglik_generative <- vestibularlong_generative |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodAF) |> 
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
visualTP_loglik_generative <- visuallong_generative |> 
  group_by(subject, leak_TP) |> 
  nest() |> 
  mutate(loglikelihoodTP = map(data, logLike_TP)) |> 
  unnest(loglikelihoodTP) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodTP) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# TP per Condition
visualTP_perCondition_loglik_generative <- visuallong_generative |> 
  group_by(subject, leak_TP, condition) |> 
  nest() |> 
  mutate(loglikelihoodTP = map(data, logLike_TP)) |> 
  unnest(loglikelihoodTP) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodTP) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF
visualIF_loglik_generative <- visuallong_generative |> 
  group_by(subject, leak_IF) |> 
  nest() |> 
  mutate(loglikelihoodIF = map(data, logLike_IF)) |> 
  unnest(loglikelihoodIF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodIF) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF per Condition
visualIF_perCondition_loglik_generative <- visuallong_generative |> 
  group_by(subject, leak_IF, condition) |> 
  nest() |> 
  mutate(loglikelihoodIF = map(data, logLike_IF)) |> 
  unnest(loglikelihoodIF) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodIF) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF
visualAF_loglik_generative <- visuallong_generative |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodAF) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF per Condition
visualAF_perCondition_loglik_generative <- visuallong_generative |> 
  group_by(subject, leak_AF, condition) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodAF) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))


# Save data to processed data

# Auditory
save(auditoryTP_loglik_generative, file = "processed_data/LLhood_generative/auditoryTP_loglik_generative.Rda")
save(auditoryIF_loglik_generative, file = "processed_data/LLhood_generative/auditoryIF_loglik_generative.Rda")
save(auditoryAF_loglik_generative, file = "processed_data/LLhood_generative/auditoryAF_loglik_generative.Rda")

save(auditoryTP_perCondition_loglik_generative, file = "processed_data/LLhood_perCondition_generative/auditoryTP_perCondition_loglik_generative.Rda")
save(auditoryIF_perCondition_loglik_generative, file = "processed_data/LLhood_perCondition_generative/auditoryIF_perCondition_loglik_generative.Rda")
save(auditoryAF_perCondition_loglik_generative, file = "processed_data/LLhood_perCondition_generative/auditoryAF_perCondition_loglik_generative.Rda")


# Vestibular
save(vestibularTP_loglik_generative, file = "processed_data/LLhood_generative/vestibularTP_loglik_generative.Rda")
save(vestibularIF_loglik_generative, file = "processed_data/LLhood_generative/vestibularIF_loglik_generative.Rda")
save(vestibularAF_loglik_generative, file = "processed_data/LLhood_generative/vestibularAF_loglik_generative.Rda")


# Visual
save(visualTP_loglik_generative, file = "processed_data/LLhood_generative/visualTP_loglik_generative.Rda")
save(visualIF_loglik_generative, file = "processed_data/LLhood_generative/visualIF_loglik_generative.Rda")
save(visualAF_loglik_generative, file = "processed_data/LLhood_generative/visualAF_loglik_generative.Rda")

save(visualTP_perCondition_loglik_generative, file = "processed_data/LLhood_perCondition_generative/visualTP_perCondition_loglik_generative.Rda")
save(visualIF_perCondition_loglik_generative, file = "processed_data/LLhood_perCondition_generative/visualIF_perCondition_loglik_generative.Rda")
save(visualAF_perCondition_loglik_generative, file = "processed_data/LLhood_perCondition_generative/visualAF_perCondition_loglik_generative.Rda")


























