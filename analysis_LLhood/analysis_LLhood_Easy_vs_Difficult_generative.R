# Loglikelihood approach easy vs. difficult (generative)

# What is done:
# --> loglik <- pbinom(1, prob = probs, q = y, log.p = TRUE) |> sum()

# NOTE: Data is backtransformed within the function with plogis()
# Because for the GLM-Analyses, data was transformed using qlogis()

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

### Log-Likelihood Models ###

# TP easy
auditoryTP_loglik_easy_generative <- auditorylong_easy_generative |> 
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
auditoryTP_loglik_difficult_generative <- auditorylong_difficult_generative |> 
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
auditoryTP_perCondition_loglik_easy_generative <- auditorylong_easy_generative |> 
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
auditoryTP_perCondition_loglik_difficult_generative <- auditorylong_difficult_generative |> 
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
auditoryIF_loglik_easy_generative <- auditorylong_easy_generative |> 
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
auditoryIF_loglik_difficult_generative <- auditorylong_difficult_generative |> 
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
auditoryIF_perCondition_loglik_easy_generative <- auditorylong_easy_generative |> 
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
auditoryIF_perCondition_loglik_difficult_generative <- auditorylong_difficult_generative |> 
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
auditoryAF_loglik_easy_generative <- auditorylong_easy_generative |> 
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
auditoryAF_loglik_difficult_generative <- auditorylong_difficult_generative |> 
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
auditoryAF_perCondition_loglik_easy_generative <- auditorylong_easy_generative |> 
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
auditoryAF_perCondition_loglik_difficult_generative <- auditorylong_difficult_generative |> 
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

# TP easy
vestibularTP_loglik_easy_generative <- vestibularlong_easy_generative |> 
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
vestibularTP_loglik_difficult_generative <- vestibularlong_difficult_generative |> 
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
vestibularIF_loglik_easy_generative <- vestibularlong_easy_generative |> 
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
vestibularIF_loglik_difficult_generative <- vestibularlong_difficult_generative |> 
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
vestibularAF_loglik_easy_generative <- vestibularlong_easy_generative |> 
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
vestibularAF_loglik_difficult_generative <- vestibularlong_difficult_generative |> 
  group_by(subject, leak_AF) |> 
  nest() |> 
  mutate(loglikelihoodAF = map(data, logLike_AF)) |> 
  unnest(loglikelihoodAF) |> 
  group_by(subject) |> 
  slice_max(loglikelihoodAF) |> 
  mutate(leak_numbers = regmatches(leak_AF, gregexpr("[[:digit:]]+", leak_AF))) |> 
  mutate(Omega = as.numeric(unlist(ifelse(leak_numbers == "character(0)", Inf, leak_numbers)))) |> 
  select(-(leak_numbers))


### === Visual === ###

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

# TP easy
visualTP_loglik_easy_generative <- visuallong_easy_generative |> 
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
visualTP_loglik_difficult_generative <- visuallong_difficult_generative |> 
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
visualTP_perCondition_loglik_easy_generative <- visuallong_easy_generative |> 
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
visualTP_perCondition_loglik_difficult_generative <- visuallong_difficult_generative |> 
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
visualIF_loglik_easy_generative <- visuallong_easy_generative |> 
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
visualIF_loglik_difficult_generative <- visuallong_difficult_generative |> 
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
visualIF_perCondition_loglik_easy_generative <- visuallong_easy_generative |> 
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
visualIF_perCondition_loglik_difficult_generative <- visuallong_difficult_generative |> 
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
visualAF_loglik_easy_generative <- visuallong_easy_generative |> 
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
visualAF_loglik_difficult_generative <- visuallong_difficult_generative |> 
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
visualAF_perCondition_loglik_easy_generative <- visuallong_easy_generative |> 
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
visualAF_perCondition_loglik_difficult_generative <- visuallong_difficult_generative |> 
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
save(auditoryTP_loglik_easy_generative, file = "processed_data/LLhood_Easy_vs_Difficult_generative/auditoryTP_loglik_easy_generative.Rda")
save(auditoryIF_loglik_easy_generative, file = "processed_data/LLhood_Easy_vs_Difficult_generative/auditoryIF_loglik_easy_generative.Rda")
save(auditoryAF_loglik_easy_generative, file = "processed_data/LLhood_Easy_vs_Difficult_generative/auditoryAF_loglik_easy_generative.Rda")

save(auditoryIF_loglik_difficult_generative, file = "processed_data/LLhood_Easy_vs_Difficult_generative/auditoryIF_loglik_difficult_generative.Rda")
save(auditoryAF_loglik_difficult_generative, file = "processed_data/LLhood_Easy_vs_Difficult_generative/auditoryAF_loglik_difficult_generative.Rda")
save(auditoryTP_loglik_difficult_generative, file = "processed_data/LLhood_Easy_vs_Difficult_generative/auditoryTP_loglik_difficult_generative.Rda")


save(auditoryTP_perCondition_loglik_easy_generative, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult_generative/auditoryTP_perCondition_loglik_easy_generative.Rda")
save(auditoryIF_perCondition_loglik_easy_generative, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult_generative/auditoryIF_perCondition_loglik_easy_generative.Rda")
save(auditoryAF_perCondition_loglik_easy_generative, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult_generative/auditoryAF_perCondition_loglik_easy_generative.Rda")

save(auditoryTP_perCondition_loglik_difficult_generative, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult_generative/auditoryTP_perCondition_loglik_difficult_generative.Rda")
save(auditoryIF_perCondition_loglik_difficult_generative, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult_generative/auditoryIF_perCondition_loglik_difficult_generative.Rda")
save(auditoryAF_perCondition_loglik_difficult_generative, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult_generative/auditoryAF_perCondition_loglik_difficult_generative.Rda")


# Vestibular
save(vestibularTP_loglik_easy_generative, file = "processed_data/LLhood_Easy_vs_Difficult_generative/vestibularTP_loglik_easy_generative.Rda")
save(vestibularIF_loglik_easy_generative, file = "processed_data/LLhood_Easy_vs_Difficult_generative/vestibularIF_loglik_easy_generative.Rda")
save(vestibularAF_loglik_easy_generative, file = "processed_data/LLhood_Easy_vs_Difficult_generative/vestibularAF_loglik_easy_generative.Rda")


save(vestibularTP_loglik_difficult_generative, file = "processed_data/LLhood_Easy_vs_Difficult_generative/vestibularTP_loglik_difficult_generative.Rda")
save(vestibularIF_loglik_difficult_generative, file = "processed_data/LLhood_Easy_vs_Difficult_generative/vestibularIF_loglik_difficult_generative.Rda")
save(vestibularAF_loglik_difficult_generative, file = "processed_data/LLhood_Easy_vs_Difficult_generative/vestibularAF_loglik_difficult_generative.Rda")


# Visual
save(visualTP_loglik_easy_generative, file = "processed_data/LLhood_Easy_vs_Difficult_generative/visualTP_loglik_easy_generative.Rda")
save(visualIF_loglik_easy_generative, file = "processed_data/LLhood_Easy_vs_Difficult_generative/visualIF_loglik_easy_generative.Rda")
save(visualAF_loglik_easy_generative, file = "processed_data/LLhood_Easy_vs_Difficult_generative/visualAF_loglik_easy_generative.Rda")

save(visualTP_loglik_difficult_generative, file = "processed_data/LLhood_Easy_vs_Difficult_generative/visualTP_loglik_difficult_generative.Rda")
save(visualIF_loglik_difficult_generative, file = "processed_data/LLhood_Easy_vs_Difficult_generative/visualIF_loglik_difficult_generative.Rda")
save(visualAF_loglik_difficult_generative, file = "processed_data/LLhood_Easy_vs_Difficult_generative/visualAF_loglik_difficult_generative.Rda")


save(visualTP_perCondition_loglik_easy_generative, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult_generative/visualTP_perCondition_loglik_easy_generative.Rda")
save(visualIF_perCondition_loglik_easy_generative, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult_generative/visualIF_perCondition_loglik_easy_generative.Rda")
save(visualAF_perCondition_loglik_easy_generative, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult_generative/visualAF_perCondition_loglik_easy_generative.Rda")

save(visualTP_perCondition_loglik_difficult_generative, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult_generative/visualTP_perCondition_loglik_difficult_generative.Rda")
save(visualIF_perCondition_loglik_difficult_generative, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult_generative/visualIF_perCondition_loglik_difficult_generative.Rda")
save(visualAF_perCondition_loglik_difficult_generative, file = "processed_data/LLhood_perCondition_Easy_vs_Difficult_generative/visualAF_perCondition_loglik_difficult_generative.Rda")

