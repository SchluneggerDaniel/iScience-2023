# Loglikelihood approach

# What is done:
# --> loglik <- pbinom(1, prob = probs, q = y, log.p = TRUE) |> sum()

# In this script: 
# (1) Stimulus-only models are computed too
# (2) Per condition (i.e. rep, alt, etc.) in auditory & visual

# NOTE: Data is backtransformed within the loglik-functions with plogis()
# Because for the GLM-Analyses, data was transformed using qlogis()

# perCondition means: separately for each condition

library(tidyverse)

# Load data
load("processed_data/allValues/allValuesAuditory.Rda")
load("processed_data/allValues/allValuesVestibular.Rda")
load("processed_data/allValues/allValuesVisual.Rda")

# Source / Create Functions
source("functions/function_LogLikelihood.R")

## Create new variables 
auditory <- allValuesAuditory
vestibular <- allValuesVestibular 
visual <- allValuesVisual

### === Auditory === ###

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
auditorylong <- bind_cols(auditory_sub_TP, auditory_sub_IF, auditory_sub_AF)

# TP
auditoryTP_loglik <- auditorylong |> 
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
auditoryTP_perCondition_loglik <- auditorylong |> 
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
auditoryIF_loglik <- auditorylong |> 
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
auditoryIF_perCondition_loglik <- auditorylong |> 
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
auditoryAF_loglik <- auditorylong |> 
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
auditoryAF_perCondition_loglik <- auditorylong |> 
  group_by(subject, leak_AF, condition) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodAF) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# Stimulus Loglikelihood 
auditoryStim_loglik <- auditory |> 
  select(-starts_with("pred")) |> 
  group_by(subject) |> 
  nest() |> 
  mutate(loglikelihoodStim = map(data, logLike_Stim)) |> 
  unnest(loglikelihoodStim) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodStim)


# Stimulus Loglikelihood per Condition
auditoryStim_perCondition_loglik <- auditory |> 
  select(-starts_with("pred")) |> 
  group_by(subject, condition) |> 
  nest() |> 
  mutate(loglikelihoodStim = map(data, logLike_Stim)) |> 
  unnest(loglikelihoodStim) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodStim)


### === Vestibular === ###

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
vestibularlong <- bind_cols(vestibular_sub_TP, vestibular_sub_IF, vestibular_sub_AF)

# TP
vestibularTP_loglik <- vestibularlong |> 
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
vestibularIF_loglik <- vestibularlong |> 
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
vestibularAF_loglik <- vestibularlong |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodAF) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# Stimulus Loglikelihood 
vestibularStim_loglik <- vestibular |> 
  select(-starts_with("pred")) |> 
  group_by(subject) |> 
  nest() |> 
  mutate(loglikelihoodStim = map(data, logLike_Stim)) |> 
  unnest(loglikelihoodStim) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodStim)



### Visual ###

# Remove zero coherence trials
# (important because stimulus is coded and therefore misleading!)

visual <- filter(visual, !(intensity == 0))

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
visuallong <- bind_cols(visual_sub_TP, visual_sub_IF, visual_sub_AF)

# TP
visualTP_loglik <- visuallong |> 
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
visualTP_perCondition_loglik <- visuallong |> 
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
visualIF_loglik <- visuallong |> 
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
visualIF_perCondition_loglik <- visuallong |> 
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
visualAF_loglik <- visuallong |> 
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
visualAF_perCondition_loglik <- visuallong |> 
  group_by(subject, leak_AF, condition) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodAF) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# Stimulus Loglikelihood 
visualStim_loglik <- visual |> 
  select(-starts_with("pred")) |> 
  group_by(subject) |> 
  nest() |> 
  mutate(loglikelihoodStim = map(data, logLike_Stim)) |> 
  unnest(loglikelihoodStim) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodStim)


# Stimulus Loglikelihood per Condition
visualStim_perCondition_loglik <- visual |> 
  select(-starts_with("pred")) |> 
  group_by(subject, condition) |> 
  nest() |> 
  mutate(loglikelihoodStim = map(data, logLike_Stim)) |> 
  unnest(loglikelihoodStim) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodStim)


# Save data to processed data

# Auditory
save(auditoryTP_loglik, file = "processed_data/LLhood/auditoryTP_loglik.Rda")
save(auditoryIF_loglik, file = "processed_data/LLhood/auditoryIF_loglik.Rda")
save(auditoryAF_loglik, file = "processed_data/LLhood/auditoryAF_loglik.Rda")
save(auditoryStim_loglik, file = "processed_data/LLhood/auditoryStim_loglik.Rda")

save(auditoryTP_perCondition_loglik, file = "processed_data/LLhood_perCondition/auditoryTP_perCondition_loglik.Rda")
save(auditoryIF_perCondition_loglik, file = "processed_data/LLhood_perCondition/auditoryIF_perCondition_loglik.Rda")
save(auditoryAF_perCondition_loglik, file = "processed_data/LLhood_perCondition/auditoryAF_perCondition_loglik.Rda")
save(auditoryStim_perCondition_loglik, file = "processed_data/LLhood_perCondition/auditoryStim_perCondition_loglik.Rda")

# Vestibular
save(vestibularTP_loglik, file = "processed_data/LLhood/vestibularTP_loglik.Rda")
save(vestibularIF_loglik, file = "processed_data/LLhood/vestibularIF_loglik.Rda")
save(vestibularAF_loglik, file = "processed_data/LLhood/vestibularAF_loglik.Rda")
save(vestibularStim_loglik, file = "processed_data/LLhood/vestibularStim_loglik.Rda")

# Visual
save(visualTP_loglik, file = "processed_data/LLhood/visualTP_loglik.Rda")
save(visualIF_loglik, file = "processed_data/LLhood/visualIF_loglik.Rda")
save(visualAF_loglik, file = "processed_data/LLhood/visualAF_loglik.Rda")
save(visualStim_loglik, file = "processed_data/LLhood/visualStim_loglik.Rda")

save(visualTP_perCondition_loglik, file = "processed_data/LLhood_perCondition/visualTP_perCondition_loglik.Rda")
save(visualIF_perCondition_loglik, file = "processed_data/LLhood_perCondition/visualIF_perCondition_loglik.Rda")
save(visualAF_perCondition_loglik, file = "processed_data/LLhood_perCondition/visualAF_perCondition_loglik.Rda")
save(visualStim_perCondition_loglik, file = "processed_data/LLhood_perCondition/visualStim_perCondition_loglik.Rda")

