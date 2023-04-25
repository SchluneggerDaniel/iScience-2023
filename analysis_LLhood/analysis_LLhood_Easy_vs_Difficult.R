# Loglikelihood approach easy vs. difficult

# What is done:
# --> loglik <- pbinom(1, prob = probs, q = y, log.p = TRUE) |> sum()

# In this script: Stimulus-Only Models are computed too

# NOTE: Data is backtransformed within the function with plogis()
# Because for the GLM-Analyses, data was transformed using qlogis()

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

### Log-Likelihood Models ###

# TP easy
auditoryTP_loglik_easy <- auditorylong_easy |> 
  group_by(subject, leak_TP) |> 
  nest() |> 
  mutate(loglikelihoodTP = map(data, logLike_TP)) |> 
  unnest(loglikelihoodTP) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodTP) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# TP difficult
auditoryTP_loglik_difficult <- auditorylong_difficult |> 
  group_by(subject, leak_TP) |> 
  nest() |> 
  mutate(loglikelihoodTP = map(data, logLike_TP)) |> 
  unnest(loglikelihoodTP) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodTP) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# TP per Condition easy
auditoryTP_perCondition_loglik_easy <- auditorylong_easy |> 
  group_by(subject, leak_TP, condition) |> 
  nest() |> 
  mutate(loglikelihoodTP = map(data, logLike_TP)) |> 
  unnest(loglikelihoodTP) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodTP) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# TP per Condition difficult
auditoryTP_perCondition_loglik_difficult <- auditorylong_difficult |> 
  group_by(subject, leak_TP, condition) |> 
  nest() |> 
  mutate(loglikelihoodTP = map(data, logLike_TP)) |> 
  unnest(loglikelihoodTP) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodTP) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF easy
auditoryIF_loglik_easy <- auditorylong_easy |> 
  group_by(subject, leak_IF) |> 
  nest() |> 
  mutate(loglikelihoodIF = map(data, logLike_IF)) |> 
  unnest(loglikelihoodIF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodIF) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF difficult
auditoryIF_loglik_difficult <- auditorylong_difficult |> 
  group_by(subject, leak_IF) |> 
  nest() |> 
  mutate(loglikelihoodIF = map(data, logLike_IF)) |> 
  unnest(loglikelihoodIF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodIF) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF per Condition easy
auditoryIF_perCondition_loglik_easy <- auditorylong_easy |> 
  group_by(subject, leak_IF, condition) |> 
  nest() |> 
  mutate(loglikelihoodIF = map(data, logLike_IF)) |> 
  unnest(loglikelihoodIF) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodIF) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF per Condition difficult
auditoryIF_perCondition_loglik_difficult <- auditorylong_difficult |> 
  group_by(subject, leak_IF, condition) |> 
  nest() |> 
  mutate(loglikelihoodIF = map(data, logLike_IF)) |> 
  unnest(loglikelihoodIF) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodIF) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF easy
auditoryAF_loglik_easy <- auditorylong_easy |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodAF) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF difficult
auditoryAF_loglik_difficult <- auditorylong_difficult |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodAF) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF per Condition easy
auditoryAF_perCondition_loglik_easy <- auditorylong_easy |> 
  group_by(subject, leak_AF, condition) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodAF) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF per Condition difficult
auditoryAF_perCondition_loglik_difficult <- auditorylong_difficult |> 
  group_by(subject, leak_AF, condition) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodAF) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# Stimulus Loglikelihood easy
auditoryStim_loglik_easy <- auditory_easy |> 
  select(-starts_with("pred")) |> 
  group_by(subject) |> 
  nest() |> 
  mutate(loglikelihoodStim = map(data, logLike_Stim)) |> 
  unnest(loglikelihoodStim) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodStim)

# Stimulus Loglikelihood difficult
auditoryStim_loglik_difficult <- auditory_difficult |> 
  select(-starts_with("pred")) |> 
  group_by(subject) |> 
  nest() |> 
  mutate(loglikelihoodStim = map(data, logLike_Stim)) |> 
  unnest(loglikelihoodStim) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodStim)

# Stimulus Loglikelihood per Condition easy
auditoryStim_perCondition_loglik_easy <- auditory_easy |> 
  select(-starts_with("pred")) |> 
  group_by(subject, condition) |> 
  nest() |> 
  mutate(loglikelihoodStim = map(data, logLike_Stim)) |> 
  unnest(loglikelihoodStim) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodStim)

# Stimulus Loglikelihood per Condition difficult
auditoryStim_perCondition_loglik_difficult <- auditory_difficult |> 
  select(-starts_with("pred")) |> 
  group_by(subject, condition) |> 
  nest() |> 
  mutate(loglikelihoodStim = map(data, logLike_Stim)) |> 
  unnest(loglikelihoodStim) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodStim)


### === Vestibular === ###

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

# TP easy
vestibularTP_loglik_easy <- vestibularlong_easy |> 
  group_by(subject, leak_TP) |> 
  nest() |> 
  mutate(loglikelihoodTP = map(data, logLike_TP)) |> 
  unnest(loglikelihoodTP) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodTP) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# TP difficult
vestibularTP_loglik_difficult <- vestibularlong_difficult |> 
  group_by(subject, leak_TP) |> 
  nest() |> 
  mutate(loglikelihoodTP = map(data, logLike_TP)) |> 
  unnest(loglikelihoodTP) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodTP) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF easy
vestibularIF_loglik_easy <- vestibularlong_easy |> 
  group_by(subject, leak_IF) |> 
  nest() |> 
  mutate(loglikelihoodIF = map(data, logLike_IF)) |> 
  unnest(loglikelihoodIF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodIF) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF difficult
vestibularIF_loglik_difficult <- vestibularlong_difficult |> 
  group_by(subject, leak_IF) |> 
  nest() |> 
  mutate(loglikelihoodIF = map(data, logLike_IF)) |> 
  unnest(loglikelihoodIF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodIF) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF easy
vestibularAF_loglik_easy <- vestibularlong_easy |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodAF) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF difficult
vestibularAF_loglik_difficult <- vestibularlong_difficult |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodAF) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# Stimulus Loglikelihood easy
vestibularStim_loglik_easy <- vestibular_easy |> 
  select(-starts_with("pred")) |> 
  group_by(subject) |> 
  nest() |> 
  mutate(loglikelihoodStim = map(data, logLike_Stim)) |> 
  unnest(loglikelihoodStim) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodStim)

# Stimulus Loglikelihood difficult
vestibularStim_loglik_difficult <- vestibular_difficult |> 
  select(-starts_with("pred")) |> 
  group_by(subject) |> 
  nest() |> 
  mutate(loglikelihoodStim = map(data, logLike_Stim)) |> 
  unnest(loglikelihoodStim) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodStim)

### === Visual === ###

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

# TP easy
visualTP_loglik_easy <- visuallong_easy |> 
  group_by(subject, leak_TP) |> 
  nest() |> 
  mutate(loglikelihoodTP = map(data, logLike_TP)) |> 
  unnest(loglikelihoodTP) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodTP) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# TP difficult
visualTP_loglik_difficult <- visuallong_difficult |> 
  group_by(subject, leak_TP) |> 
  nest() |> 
  mutate(loglikelihoodTP = map(data, logLike_TP)) |> 
  unnest(loglikelihoodTP) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodTP) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# TP per Condition easy
visualTP_perCondition_loglik_easy <- visuallong_easy |> 
  group_by(subject, leak_TP, condition) |> 
  nest() |> 
  mutate(loglikelihoodTP = map(data, logLike_TP)) |> 
  unnest(loglikelihoodTP) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodTP) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# TP per Condition difficult
visualTP_perCondition_loglik_difficult <- visuallong_difficult |> 
  group_by(subject, leak_TP, condition) |> 
  nest() |> 
  mutate(loglikelihoodTP = map(data, logLike_TP)) |> 
  unnest(loglikelihoodTP) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodTP) |> 
  mutate(leak_numbers = regmatches(leak_TP, gregexpr("[[:digit:]]+", leak_TP))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF easy
visualIF_loglik_easy <- visuallong_easy |> 
  group_by(subject, leak_IF) |> 
  nest() |> 
  mutate(loglikelihoodIF = map(data, logLike_IF)) |> 
  unnest(loglikelihoodIF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodIF) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF difficult
visualIF_loglik_difficult <- visuallong_difficult |> 
  group_by(subject, leak_IF) |> 
  nest() |> 
  mutate(loglikelihoodIF = map(data, logLike_IF)) |> 
  unnest(loglikelihoodIF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodIF) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF per Condition easy
visualIF_perCondition_loglik_easy <- visuallong_easy |> 
  group_by(subject, leak_IF, condition) |> 
  nest() |> 
  mutate(loglikelihoodIF = map(data, logLike_IF)) |> 
  unnest(loglikelihoodIF) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodIF) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# IF per Condition difficult
visualIF_perCondition_loglik_difficult <- visuallong_difficult |> 
  group_by(subject, leak_IF, condition) |> 
  nest() |> 
  mutate(loglikelihoodIF = map(data, logLike_IF)) |> 
  unnest(loglikelihoodIF) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodIF) |> 
  mutate(leak_numbers = regmatches(leak_IF, gregexpr("[[:digit:]]+", leak_IF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF easy
visualAF_loglik_easy <- visuallong_easy |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodAF) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF
visualAF_loglik_difficult <- visuallong_difficult |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodAF) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF per Condition easy
visualAF_perCondition_loglik_easy <- visuallong_easy |> 
  group_by(subject, leak_AF, condition) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodAF) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# AF per Condition difficult
visualAF_perCondition_loglik_difficult <- visuallong_difficult |> 
  group_by(subject, leak_AF, condition) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodAF) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))

# Stimulus Loglikelihood easy
visualStim_loglik_easy <- visual_easy |> 
  select(-starts_with("pred")) |> 
  group_by(subject) |> 
  nest() |> 
  mutate(loglikelihoodStim = map(data, logLike_Stim)) |> 
  unnest(loglikelihoodStim) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodStim)

# Stimulus Loglikelihood difficult
visualStim_loglik_difficult <- visual_difficult |> 
  select(-starts_with("pred")) |> 
  group_by(subject) |> 
  nest() |> 
  mutate(loglikelihoodStim = map(data, logLike_Stim)) |> 
  unnest(loglikelihoodStim) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodStim)

# Stimulus Loglikelihood per Condition easy
visualStim_perCondition_loglik_easy <- visual_easy |> 
  select(-starts_with("pred")) |> 
  group_by(subject, condition) |> 
  nest() |> 
  mutate(loglikelihoodStim = map(data, logLike_Stim)) |> 
  unnest(loglikelihoodStim) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodStim)

# Stimulus Loglikelihood per Condition difficult
visualStim_perCondition_loglik_difficult <- visual_difficult |> 
  select(-starts_with("pred")) |> 
  group_by(subject, condition) |> 
  nest() |> 
  mutate(loglikelihoodStim = map(data, logLike_Stim)) |> 
  unnest(loglikelihoodStim) |> 
  group_by(subject, condition) |> 
  slice_max(loglikelihoodStim)


# Save data to processed data

# Auditory
save(auditoryTP_loglik_easy, file = "processed_data/LLhood_Easy_vs_Difficult/auditoryTP_loglik_easy.Rda")
save(auditoryIF_loglik_easy, file = "processed_data/LLhood_Easy_vs_Difficult/auditoryIF_loglik_easy.Rda")
save(auditoryAF_loglik_easy, file = "processed_data/LLhood_Easy_vs_Difficult/auditoryAF_loglik_easy.Rda")
save(auditoryStim_loglik_easy, file = "processed_data/LLhood_Easy_vs_Difficult/auditoryStim_loglik_easy.Rda")

save(auditoryIF_loglik_difficult, file = "processed_data/LLhood_Easy_vs_Difficult/auditoryIF_loglik_difficult.Rda")
save(auditoryAF_loglik_difficult, file = "processed_data/LLhood_Easy_vs_Difficult/auditoryAF_loglik_difficult.Rda")
save(auditoryTP_loglik_difficult, file = "processed_data/LLhood_Easy_vs_Difficult/auditoryTP_loglik_difficult.Rda")
save(auditoryStim_loglik_difficult, file = "processed_data/LLhood_Easy_vs_Difficult/auditoryStim_loglik_difficult.Rda")

save(auditoryTP_perCondition_loglik_easy, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult/auditoryTP_perCondition_loglik_easy.Rda")
save(auditoryIF_perCondition_loglik_easy, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult/auditoryIF_perCondition_loglik_easy.Rda")
save(auditoryAF_perCondition_loglik_easy, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult/auditoryAF_perCondition_loglik_easy.Rda")
save(auditoryStim_perCondition_loglik_easy, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult/auditoryStim_perCondition_loglik_easy.Rda")

save(auditoryTP_perCondition_loglik_difficult, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult/auditoryTP_perCondition_loglik_difficult.Rda")
save(auditoryIF_perCondition_loglik_difficult, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult/auditoryIF_perCondition_loglik_difficult.Rda")
save(auditoryAF_perCondition_loglik_difficult, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult/auditoryAF_perCondition_loglik_difficult.Rda")
save(auditoryStim_perCondition_loglik_difficult, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult/auditoryStim_perCondition_loglik_difficult.Rda")

# Vestibular
save(vestibularTP_loglik_easy, file = "processed_data/LLhood_Easy_vs_Difficult/vestibularTP_loglik_easy.Rda")
save(vestibularIF_loglik_easy, file = "processed_data/LLhood_Easy_vs_Difficult/vestibularIF_loglik_easy.Rda")
save(vestibularAF_loglik_easy, file = "processed_data/LLhood_Easy_vs_Difficult/vestibularAF_loglik_easy.Rda")
save(vestibularStim_loglik_easy, file = "processed_data/LLhood_Easy_vs_Difficult/vestibularStim_loglik_easy.Rda")

save(vestibularTP_loglik_difficult, file = "processed_data/LLhood_Easy_vs_Difficult/vestibularTP_loglik_difficult.Rda")
save(vestibularIF_loglik_difficult, file = "processed_data/LLhood_Easy_vs_Difficult/vestibularIF_loglik_difficult.Rda")
save(vestibularAF_loglik_difficult, file = "processed_data/LLhood_Easy_vs_Difficult/vestibularAF_loglik_difficult.Rda")
save(vestibularStim_loglik_difficult, file = "processed_data/LLhood_Easy_vs_Difficult/vestibularStim_loglik_difficult.Rda")

# Visual
save(visualTP_loglik_easy, file = "processed_data/LLhood_Easy_vs_Difficult/visualTP_loglik_easy.Rda")
save(visualIF_loglik_easy, file = "processed_data/LLhood_Easy_vs_Difficult/visualIF_loglik_easy.Rda")
save(visualAF_loglik_easy, file = "processed_data/LLhood_Easy_vs_Difficult/visualAF_loglik_easy.Rda")
save(visualStim_loglik_easy, file = "processed_data/LLhood_Easy_vs_Difficult/visualStim_loglik_easy.Rda")

save(visualTP_loglik_difficult, file = "processed_data/LLhood_Easy_vs_Difficult/visualTP_loglik_difficult.Rda")
save(visualIF_loglik_difficult, file = "processed_data/LLhood_Easy_vs_Difficult/visualIF_loglik_difficult.Rda")
save(visualAF_loglik_difficult, file = "processed_data/LLhood_Easy_vs_Difficult/visualAF_loglik_difficult.Rda")
save(visualStim_loglik_difficult, file = "processed_data/LLhood_Easy_vs_Difficult/visualStim_loglik_difficult.Rda")

save(visualTP_perCondition_loglik_easy, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult/visualTP_perCondition_loglik_easy.Rda")
save(visualIF_perCondition_loglik_easy, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult/visualIF_perCondition_loglik_easy.Rda")
save(visualAF_perCondition_loglik_easy, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult/visualAF_perCondition_loglik_easy.Rda")
save(visualStim_perCondition_loglik_easy, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult/visualStim_perCondition_loglik_easy.Rda")

save(visualTP_perCondition_loglik_difficult, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult/visualTP_perCondition_loglik_difficult.Rda")
save(visualIF_perCondition_loglik_difficult, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult/visualIF_perCondition_loglik_difficult.Rda")
save(visualAF_perCondition_loglik_difficult, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult/visualAF_perCondition_loglik_difficult.Rda")
save(visualStim_perCondition_loglik_difficult, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult/visualStim_perCondition_loglik_difficult.Rda")

