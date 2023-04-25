# data preprocessing loglikelihood plots (delta LLhood)

# Purpose: Both, preparing for plot & significance testing

library(fs)
library(tidyverse)
library(patchwork)

### === LOAD DATA === ###

# Create file list (space is needed if you use path_filter()) 
file_list <- dir_ls("processed_data/LLhood_Easy_vs_Difficult/") 
file_list_generative <- dir_ls("processed_data/LLhood_Easy_vs_Difficult_generative/") 

# Load all data at once
lapply(file_list, load, .GlobalEnv)
lapply(file_list_generative, load, .GlobalEnv)


### === ADD GENERATIVE VALUE TO DF'S === ###

# Auditory easy
auditoryTP_loglik_easy$Omega_generative <- auditoryTP_loglik_easy_generative$Omega
auditoryTP_loglik_easy$loglikelihoodTP_generative <- auditoryTP_loglik_easy_generative$loglikelihoodTP

auditoryIF_loglik_easy$Omega_generative <- auditoryIF_loglik_easy_generative$Omega
auditoryIF_loglik_easy$loglikelihoodIF_generative <- auditoryIF_loglik_easy_generative$loglikelihoodIF

auditoryAF_loglik_easy$Omega_generative <- auditoryAF_loglik_easy_generative$Omega
auditoryAF_loglik_easy$loglikelihoodAF_generative <- auditoryAF_loglik_easy_generative$loglikelihoodAF


# Auditory difficult
auditoryTP_loglik_difficult$Omega_generative <- auditoryTP_loglik_difficult_generative$Omega
auditoryTP_loglik_difficult$loglikelihoodTP_generative <- auditoryTP_loglik_difficult_generative$loglikelihoodTP

auditoryIF_loglik_difficult$Omega_generative <- auditoryIF_loglik_difficult_generative$Omega
auditoryIF_loglik_difficult$loglikelihoodIF_generative <- auditoryIF_loglik_difficult_generative$loglikelihoodIF

auditoryAF_loglik_difficult$Omega_generative <- auditoryAF_loglik_difficult_generative$Omega
auditoryAF_loglik_difficult$loglikelihoodAF_generative <- auditoryAF_loglik_difficult_generative$loglikelihoodAF


# Vestibular easy
vestibularTP_loglik_easy$Omega_generative <- vestibularTP_loglik_easy_generative$Omega
vestibularTP_loglik_easy$loglikelihoodTP_generative <- vestibularTP_loglik_easy_generative$loglikelihoodTP

vestibularIF_loglik_easy$Omega_generative <- vestibularIF_loglik_easy_generative$Omega
vestibularIF_loglik_easy$loglikelihoodIF_generative <- vestibularIF_loglik_easy_generative$loglikelihoodIF

vestibularAF_loglik_easy$Omega_generative <- vestibularAF_loglik_easy_generative$Omega
vestibularAF_loglik_easy$loglikelihoodAF_generative <- vestibularAF_loglik_easy_generative$loglikelihoodAF

# Vestibular difficult
vestibularTP_loglik_difficult$Omega_generative <- vestibularTP_loglik_difficult_generative$Omega
vestibularTP_loglik_difficult$loglikelihoodTP_generative <- vestibularTP_loglik_difficult_generative$loglikelihoodTP

vestibularIF_loglik_difficult$Omega_generative <- vestibularIF_loglik_difficult_generative$Omega
vestibularIF_loglik_difficult$loglikelihoodIF_generative <- vestibularIF_loglik_difficult_generative$loglikelihoodIF

vestibularAF_loglik_difficult$Omega_generative <- vestibularAF_loglik_difficult_generative$Omega
vestibularAF_loglik_difficult$loglikelihoodAF_generative <- vestibularAF_loglik_difficult_generative$loglikelihoodAF

# Visual easy
visualTP_loglik_easy$Omega_generative <- visualTP_loglik_easy_generative$Omega
visualTP_loglik_easy$loglikelihoodTP_generative <- visualTP_loglik_easy_generative$loglikelihoodTP

visualIF_loglik_easy$Omega_generative <- visualIF_loglik_easy_generative$Omega
visualIF_loglik_easy$loglikelihoodIF_generative <- visualIF_loglik_easy_generative$loglikelihoodIF

visualAF_loglik_easy$Omega_generative <- visualAF_loglik_easy_generative$Omega
visualAF_loglik_easy$loglikelihoodAF_generative <- visualAF_loglik_easy_generative$loglikelihoodAF

# Visual difficult
visualTP_loglik_difficult$Omega_generative <- visualTP_loglik_difficult_generative$Omega
visualTP_loglik_difficult$loglikelihoodTP_generative <- visualTP_loglik_difficult_generative$loglikelihoodTP

visualIF_loglik_difficult$Omega_generative <- visualIF_loglik_difficult_generative$Omega
visualIF_loglik_difficult$loglikelihoodIF_generative <- visualIF_loglik_difficult_generative$loglikelihoodIF

visualAF_loglik_difficult$Omega_generative <- visualAF_loglik_difficult_generative$Omega
visualAF_loglik_difficult$loglikelihoodAF_generative <- visualAF_loglik_difficult_generative$loglikelihoodAF




# Create factors
modality <- rep(c("auditory", "vestibular", "visual"), each = 6)
model <- rep(c("TP", "IF", "AF"), each = 2, times = 3)
level <- rep(c("easy", "difficult"), times = 9) |> factor(levels = c("easy", "difficult"))

# Create pseudovariables
mean_difference <- 1:length(model) |> as.double()
mean_diff_to_stim_only <- 1:length(model) |> as.double()

SEM <- 1:length(model) |> as.double()
SEM_stim_only <- 1:length(model) |> as.double()

statistic <- 1:length(model) |> as.double()
statistic_stim_only <- 1:length(model) |> as.double()

p.value <- 1:length(model) |> as.double()
p.value_stim_only <- 1:length(model) |> as.double()


# Create tibble
delta_LLhood_EvD <- tibble(modality, 
                           model, 
                           level, 
                           mean_difference, 
                           mean_diff_to_stim_only, 
                           SEM, 
                           SEM_stim_only,
                           statistic, 
                           statistic_stim_only, 
                           p.value,
                           p.value_stim_only) |> 
  mutate(across(where(is.character), factor))

delta_LLhood_EvD


### === SIGNIFICANCE TESTS === ###

# In wilcox.test --> [[1]] == statistic | [[3]] == p.value

# Add function to compute Standarderror of the Mean (SEM)

stderror <- function(x) sd(x)/sqrt(length(x))

## == AUDITORY == ##

# Transition Probability 
delta_LLhood_EvD[modality == "auditory" & model == "TP" & level == "easy", "mean_difference"] <- 
  mean(auditoryTP_loglik_easy$loglikelihoodTP_generative - auditoryTP_loglik_easy$loglikelihoodTP)

delta_LLhood_EvD[modality == "auditory" & model == "TP" & level == "easy", "mean_diff_to_stim_only"] <- 
  mean(auditoryStim_loglik_easy$loglikelihoodStim - auditoryTP_loglik_easy$loglikelihoodTP)

delta_LLhood_EvD[modality == "auditory" & model == "TP" & level == "easy", "SEM"] <- 
  stderror(auditoryTP_loglik_easy$loglikelihoodTP_generative - auditoryTP_loglik_easy$loglikelihoodTP)

delta_LLhood_EvD[modality == "auditory" & model == "TP" & level == "easy", "SEM_stim_only"] <- 
  stderror(auditoryStim_loglik_easy$loglikelihoodStim - auditoryTP_loglik_easy$loglikelihoodTP)

delta_LLhood_EvD[modality == "auditory" & model == "TP" & level == "easy", "statistic"] <- 
  wilcox.test(auditoryTP_loglik_easy$loglikelihoodTP_generative - auditoryTP_loglik_easy$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "auditory" & model == "TP" & level == "easy", "statistic_stim_only"] <- 
  wilcox.test(auditoryStim_loglik_easy$loglikelihoodStim - auditoryTP_loglik_easy$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "auditory" & model == "TP" & level == "easy", "p.value"] <- 
  wilcox.test(auditoryTP_loglik_easy$loglikelihoodTP_generative - auditoryTP_loglik_easy$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

delta_LLhood_EvD[modality == "auditory" & model == "TP" & level == "easy", "p.value_stim_only"] <- 
  wilcox.test(auditoryStim_loglik_easy$loglikelihoodStim - auditoryTP_loglik_easy$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


delta_LLhood_EvD[modality == "auditory" & model == "TP" & level == "difficult", "mean_difference"] <- 
  mean(auditoryTP_loglik_difficult$loglikelihoodTP_generative - auditoryTP_loglik_difficult$loglikelihoodTP)

delta_LLhood_EvD[modality == "auditory" & model == "TP" & level == "difficult", "mean_diff_to_stim_only"] <- 
  mean(auditoryStim_loglik_difficult$loglikelihoodStim - auditoryTP_loglik_difficult$loglikelihoodTP)

delta_LLhood_EvD[modality == "auditory" & model == "TP" & level == "difficult", "SEM"] <- 
  stderror(auditoryTP_loglik_difficult$loglikelihoodTP_generative - auditoryTP_loglik_difficult$loglikelihoodTP)

delta_LLhood_EvD[modality == "auditory" & model == "TP" & level == "difficult", "SEM_stim_only"] <- 
  stderror(auditoryStim_loglik_difficult$loglikelihoodStim - auditoryTP_loglik_difficult$loglikelihoodTP)

delta_LLhood_EvD[modality == "auditory" & model == "TP" & level == "difficult", "statistic"] <- 
  wilcox.test(auditoryTP_loglik_difficult$loglikelihoodTP_generative - auditoryTP_loglik_difficult$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "auditory" & model == "TP" & level == "difficult", "statistic_stim_only"] <- 
  wilcox.test(auditoryStim_loglik_difficult$loglikelihoodStim - auditoryTP_loglik_difficult$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "auditory" & model == "TP" & level == "difficult", "p.value"] <- 
  wilcox.test(auditoryTP_loglik_difficult$loglikelihoodTP_generative - auditoryTP_loglik_difficult$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

delta_LLhood_EvD[modality == "auditory" & model == "TP" & level == "difficult", "p.value_stim_only"] <- 
  wilcox.test(auditoryStim_loglik_difficult$loglikelihoodStim - auditoryTP_loglik_difficult$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# Item Frequency
delta_LLhood_EvD[modality == "auditory" & model == "IF" & level == "easy", "mean_difference"] <- 
  mean(auditoryIF_loglik_easy$loglikelihoodIF_generative - auditoryIF_loglik_easy$loglikelihoodIF)

delta_LLhood_EvD[modality == "auditory" & model == "IF" & level == "easy", "mean_diff_to_stim_only"] <- 
  mean(auditoryStim_loglik_easy$loglikelihoodStim - auditoryIF_loglik_easy$loglikelihoodIF)

delta_LLhood_EvD[modality == "auditory" & model == "IF" & level == "easy", "SEM"] <- 
  stderror(auditoryIF_loglik_easy$loglikelihoodIF_generative - auditoryIF_loglik_easy$loglikelihoodIF)

delta_LLhood_EvD[modality == "auditory" & model == "IF" & level == "easy", "SEM_stim_only"] <- 
  stderror(auditoryStim_loglik_easy$loglikelihoodStim - auditoryIF_loglik_easy$loglikelihoodIF)

delta_LLhood_EvD[modality == "auditory" & model == "IF" & level == "easy", "statistic"] <- 
  wilcox.test(auditoryIF_loglik_easy$loglikelihoodIF_generative - auditoryIF_loglik_easy$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "auditory" & model == "IF" & level == "easy", "statistic_stim_only"] <- 
  wilcox.test(auditoryStim_loglik_easy$loglikelihoodStim - auditoryIF_loglik_easy$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "auditory" & model == "IF" & level == "easy", "p.value"] <- 
  wilcox.test(auditoryIF_loglik_easy$loglikelihoodIF_generative - auditoryIF_loglik_easy$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

delta_LLhood_EvD[modality == "auditory" & model == "IF" & level == "easy", "p.value_stim_only"] <- 
  wilcox.test(auditoryStim_loglik_easy$loglikelihoodStim - auditoryIF_loglik_easy$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


delta_LLhood_EvD[modality == "auditory" & model == "IF" & level == "difficult", "mean_difference"] <- 
  mean(auditoryIF_loglik_difficult$loglikelihoodIF_generative - auditoryIF_loglik_difficult$loglikelihoodIF)

delta_LLhood_EvD[modality == "auditory" & model == "IF" & level == "difficult", "mean_diff_to_stim_only"] <- 
  mean(auditoryStim_loglik_difficult$loglikelihoodStim - auditoryIF_loglik_difficult$loglikelihoodIF)

delta_LLhood_EvD[modality == "auditory" & model == "IF" & level == "difficult", "SEM"] <- 
  stderror(auditoryIF_loglik_difficult$loglikelihoodIF_generative - auditoryIF_loglik_difficult$loglikelihoodIF)

delta_LLhood_EvD[modality == "auditory" & model == "IF" & level == "difficult", "SEM_stim_only"] <- 
  stderror(auditoryStim_loglik_difficult$loglikelihoodStim - auditoryIF_loglik_difficult$loglikelihoodIF)

delta_LLhood_EvD[modality == "auditory" & model == "IF" & level == "difficult", "statistic"] <- 
  wilcox.test(auditoryIF_loglik_difficult$loglikelihoodIF_generative - auditoryIF_loglik_difficult$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "auditory" & model == "IF" & level == "difficult", "statistic_stim_only"] <- 
  wilcox.test(auditoryStim_loglik_difficult$loglikelihoodStim - auditoryIF_loglik_difficult$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "auditory" & model == "IF" & level == "difficult", "p.value"] <- 
  wilcox.test(auditoryIF_loglik_difficult$loglikelihoodIF_generative - auditoryIF_loglik_difficult$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

delta_LLhood_EvD[modality == "auditory" & model == "IF" & level == "difficult", "p.value_stim_only"] <- 
  wilcox.test(auditoryStim_loglik_difficult$loglikelihoodStim - auditoryIF_loglik_difficult$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# Alternation Frequency
delta_LLhood_EvD[modality == "auditory" & model == "AF" & level == "easy", "mean_difference"] <- 
  mean(auditoryAF_loglik_easy$loglikelihoodAF_generative - auditoryAF_loglik_easy$loglikelihoodAF)

delta_LLhood_EvD[modality == "auditory" & model == "AF" & level == "easy", "mean_diff_to_stim_only"] <- 
  mean(auditoryStim_loglik_easy$loglikelihoodStim - auditoryAF_loglik_easy$loglikelihoodAF)

delta_LLhood_EvD[modality == "auditory" & model == "AF" & level == "easy", "SEM"] <- 
  stderror(auditoryAF_loglik_easy$loglikelihoodAF_generative - auditoryAF_loglik_easy$loglikelihoodAF)

delta_LLhood_EvD[modality == "auditory" & model == "AF" & level == "easy", "SEM_stim_only"] <- 
  stderror(auditoryStim_loglik_easy$loglikelihoodStim - auditoryAF_loglik_easy$loglikelihoodAF)

delta_LLhood_EvD[modality == "auditory" & model == "AF" & level == "easy", "statistic"] <- 
  wilcox.test(auditoryAF_loglik_easy$loglikelihoodAF_generative - auditoryAF_loglik_easy$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "auditory" & model == "AF" & level == "easy", "statistic_stim_only"] <- 
  wilcox.test(auditoryStim_loglik_easy$loglikelihoodStim - auditoryAF_loglik_easy$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "auditory" & model == "AF" & level == "easy", "p.value"] <- 
  wilcox.test(auditoryAF_loglik_easy$loglikelihoodAF_generative - auditoryAF_loglik_easy$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

delta_LLhood_EvD[modality == "auditory" & model == "AF" & level == "easy", "p.value_stim_only"] <- 
  wilcox.test(auditoryStim_loglik_easy$loglikelihoodStim - auditoryAF_loglik_easy$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


delta_LLhood_EvD[modality == "auditory" & model == "AF" & level == "difficult", "mean_difference"] <- 
  mean(auditoryAF_loglik_difficult$loglikelihoodAF_generative - auditoryAF_loglik_difficult$loglikelihoodAF)

delta_LLhood_EvD[modality == "auditory" & model == "AF" & level == "difficult", "mean_diff_to_stim_only"] <- 
  mean(auditoryStim_loglik_difficult$loglikelihoodStim - auditoryAF_loglik_difficult$loglikelihoodAF)

delta_LLhood_EvD[modality == "auditory" & model == "AF" & level == "difficult", "SEM"] <- 
  stderror(auditoryAF_loglik_difficult$loglikelihoodAF_generative - auditoryAF_loglik_difficult$loglikelihoodAF)

delta_LLhood_EvD[modality == "auditory" & model == "AF" & level == "difficult", "SEM_stim_only"] <- 
  stderror(auditoryStim_loglik_difficult$loglikelihoodStim - auditoryAF_loglik_difficult$loglikelihoodAF)

delta_LLhood_EvD[modality == "auditory" & model == "AF" & level == "difficult", "statistic"] <- 
  wilcox.test(auditoryAF_loglik_difficult$loglikelihoodAF_generative - auditoryAF_loglik_difficult$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "auditory" & model == "AF" & level == "difficult", "statistic_stim_only"] <- 
  wilcox.test(auditoryStim_loglik_difficult$loglikelihoodStim - auditoryAF_loglik_difficult$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "auditory" & model == "AF" & level == "difficult", "p.value"] <- 
  wilcox.test(auditoryAF_loglik_difficult$loglikelihoodAF_generative - auditoryAF_loglik_difficult$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

delta_LLhood_EvD[modality == "auditory" & model == "AF" & level == "difficult", "p.value_stim_only"] <- 
  wilcox.test(auditoryStim_loglik_difficult$loglikelihoodStim - auditoryAF_loglik_difficult$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


## == VESTIBULAR == ##

# Transition Probability 
delta_LLhood_EvD[modality == "vestibular" & model == "TP" & level == "easy", "mean_difference"] <- 
  mean(vestibularTP_loglik_easy$loglikelihoodTP_generative - vestibularTP_loglik_easy$loglikelihoodTP)

delta_LLhood_EvD[modality == "vestibular" & model == "TP" & level == "easy", "mean_diff_to_stim_only"] <- 
  mean(vestibularStim_loglik_easy$loglikelihoodStim - vestibularTP_loglik_easy$loglikelihoodTP)

delta_LLhood_EvD[modality == "vestibular" & model == "TP" & level == "easy", "SEM"] <- 
  stderror(vestibularTP_loglik_easy$loglikelihoodTP_generative - vestibularTP_loglik_easy$loglikelihoodTP)

delta_LLhood_EvD[modality == "vestibular" & model == "TP" & level == "easy", "SEM_stim_only"] <- 
  stderror(vestibularStim_loglik_easy$loglikelihoodStim - vestibularTP_loglik_easy$loglikelihoodTP)

delta_LLhood_EvD[modality == "vestibular" & model == "TP" & level == "easy", "statistic"] <- 
  wilcox.test(vestibularTP_loglik_easy$loglikelihoodTP_generative - vestibularTP_loglik_easy$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "vestibular" & model == "TP" & level == "easy", "statistic_stim_only"] <- 
  wilcox.test(vestibularStim_loglik_easy$loglikelihoodStim - vestibularTP_loglik_easy$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "vestibular" & model == "TP" & level == "easy", "p.value"] <- 
  wilcox.test(vestibularTP_loglik_easy$loglikelihoodTP_generative - vestibularTP_loglik_easy$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

delta_LLhood_EvD[modality == "vestibular" & model == "TP" & level == "easy", "p.value_stim_only"] <- 
  wilcox.test(vestibularStim_loglik_easy$loglikelihoodStim - vestibularTP_loglik_easy$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


delta_LLhood_EvD[modality == "vestibular" & model == "TP" & level == "difficult", "mean_difference"] <- 
  mean(vestibularTP_loglik_difficult$loglikelihoodTP_generative - vestibularTP_loglik_difficult$loglikelihoodTP)

delta_LLhood_EvD[modality == "vestibular" & model == "TP" & level == "difficult", "mean_diff_to_stim_only"] <- 
  mean(vestibularStim_loglik_difficult$loglikelihoodStim - vestibularTP_loglik_difficult$loglikelihoodTP)

delta_LLhood_EvD[modality == "vestibular" & model == "TP" & level == "difficult", "SEM"] <- 
  stderror(vestibularTP_loglik_difficult$loglikelihoodTP_generative - vestibularTP_loglik_difficult$loglikelihoodTP)

delta_LLhood_EvD[modality == "vestibular" & model == "TP" & level == "difficult", "SEM_stim_only"] <- 
  stderror(vestibularStim_loglik_difficult$loglikelihoodStim - vestibularTP_loglik_difficult$loglikelihoodTP)

delta_LLhood_EvD[modality == "vestibular" & model == "TP" & level == "difficult", "statistic"] <- 
  wilcox.test(vestibularTP_loglik_difficult$loglikelihoodTP_generative - vestibularTP_loglik_difficult$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "vestibular" & model == "TP" & level == "difficult", "statistic_stim_only"] <- 
  wilcox.test(vestibularStim_loglik_difficult$loglikelihoodStim - vestibularTP_loglik_difficult$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "vestibular" & model == "TP" & level == "difficult", "p.value"] <- 
  wilcox.test(vestibularTP_loglik_difficult$loglikelihoodTP_generative - vestibularTP_loglik_difficult$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

delta_LLhood_EvD[modality == "vestibular" & model == "TP" & level == "difficult", "p.value_stim_only"] <- 
  wilcox.test(vestibularStim_loglik_difficult$loglikelihoodStim - vestibularTP_loglik_difficult$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# Item Frequency
delta_LLhood_EvD[modality == "vestibular" & model == "IF" & level == "easy", "mean_difference"] <- 
  mean(vestibularIF_loglik_easy$loglikelihoodIF_generative - vestibularIF_loglik_easy$loglikelihoodIF)

delta_LLhood_EvD[modality == "vestibular" & model == "IF" & level == "easy", "mean_diff_to_stim_only"] <- 
  mean(vestibularStim_loglik_easy$loglikelihoodStim - vestibularIF_loglik_easy$loglikelihoodIF)

delta_LLhood_EvD[modality == "vestibular" & model == "IF" & level == "easy", "SEM"] <- 
  stderror(vestibularIF_loglik_easy$loglikelihoodIF_generative - vestibularIF_loglik_easy$loglikelihoodIF)

delta_LLhood_EvD[modality == "vestibular" & model == "IF" & level == "easy", "SEM_stim_only"] <- 
  stderror(vestibularStim_loglik_easy$loglikelihoodStim - vestibularIF_loglik_easy$loglikelihoodIF)

delta_LLhood_EvD[modality == "vestibular" & model == "IF" & level == "easy", "statistic"] <- 
  wilcox.test(vestibularIF_loglik_easy$loglikelihoodIF_generative - vestibularIF_loglik_easy$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "vestibular" & model == "IF" & level == "easy", "statistic_stim_only"] <- 
  wilcox.test(vestibularStim_loglik_easy$loglikelihoodStim - vestibularIF_loglik_easy$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "vestibular" & model == "IF" & level == "easy", "p.value"] <- 
  wilcox.test(vestibularIF_loglik_easy$loglikelihoodIF_generative - vestibularIF_loglik_easy$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

delta_LLhood_EvD[modality == "vestibular" & model == "IF" & level == "easy", "p.value_stim_only"] <- 
  wilcox.test(vestibularStim_loglik_easy$loglikelihoodStim - vestibularIF_loglik_easy$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


delta_LLhood_EvD[modality == "vestibular" & model == "IF" & level == "difficult", "mean_difference"] <- 
  mean(vestibularIF_loglik_difficult$loglikelihoodIF_generative - vestibularIF_loglik_difficult$loglikelihoodIF)

delta_LLhood_EvD[modality == "vestibular" & model == "IF" & level == "difficult", "mean_diff_to_stim_only"] <- 
  mean(vestibularStim_loglik_difficult$loglikelihoodStim - vestibularIF_loglik_difficult$loglikelihoodIF)

delta_LLhood_EvD[modality == "vestibular" & model == "IF" & level == "difficult", "SEM"] <- 
  stderror(vestibularIF_loglik_difficult$loglikelihoodIF_generative - vestibularIF_loglik_difficult$loglikelihoodIF)

delta_LLhood_EvD[modality == "vestibular" & model == "IF" & level == "difficult", "SEM_stim_only"] <- 
  stderror(vestibularStim_loglik_difficult$loglikelihoodStim - vestibularIF_loglik_difficult$loglikelihoodIF)

delta_LLhood_EvD[modality == "vestibular" & model == "IF" & level == "difficult", "statistic"] <- 
  wilcox.test(vestibularIF_loglik_difficult$loglikelihoodIF_generative - vestibularIF_loglik_difficult$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "vestibular" & model == "IF" & level == "difficult", "statistic_stim_only"] <- 
  wilcox.test(vestibularStim_loglik_difficult$loglikelihoodStim - vestibularIF_loglik_difficult$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "vestibular" & model == "IF" & level == "difficult", "p.value"] <- 
  wilcox.test(vestibularIF_loglik_difficult$loglikelihoodIF_generative - vestibularIF_loglik_difficult$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

delta_LLhood_EvD[modality == "vestibular" & model == "IF" & level == "difficult", "p.value_stim_only"] <- 
  wilcox.test(vestibularStim_loglik_difficult$loglikelihoodStim - vestibularIF_loglik_difficult$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# Alternation Frequency
delta_LLhood_EvD[modality == "vestibular" & model == "AF" & level == "easy", "mean_difference"] <- 
  mean(vestibularAF_loglik_easy$loglikelihoodAF_generative - vestibularAF_loglik_easy$loglikelihoodAF)

delta_LLhood_EvD[modality == "vestibular" & model == "AF" & level == "easy", "mean_diff_to_stim_only"] <- 
  mean(vestibularStim_loglik_easy$loglikelihoodStim - vestibularAF_loglik_easy$loglikelihoodAF)

delta_LLhood_EvD[modality == "vestibular" & model == "AF" & level == "easy", "SEM"] <- 
  stderror(vestibularAF_loglik_easy$loglikelihoodAF_generative - vestibularAF_loglik_easy$loglikelihoodAF)

delta_LLhood_EvD[modality == "vestibular" & model == "AF" & level == "easy", "SEM_stim_only"] <- 
  stderror(vestibularStim_loglik_easy$loglikelihoodStim - vestibularAF_loglik_easy$loglikelihoodAF)

delta_LLhood_EvD[modality == "vestibular" & model == "AF" & level == "easy", "statistic"] <- 
  wilcox.test(vestibularAF_loglik_easy$loglikelihoodAF_generative - vestibularAF_loglik_easy$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "vestibular" & model == "AF" & level == "easy", "statistic_stim_only"] <- 
  wilcox.test(vestibularStim_loglik_easy$loglikelihoodStim - vestibularAF_loglik_easy$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "vestibular" & model == "AF" & level == "easy", "p.value"] <- 
  wilcox.test(vestibularAF_loglik_easy$loglikelihoodAF_generative - vestibularAF_loglik_easy$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

delta_LLhood_EvD[modality == "vestibular" & model == "AF" & level == "easy", "p.value_stim_only"] <- 
  wilcox.test(vestibularStim_loglik_easy$loglikelihoodStim - vestibularAF_loglik_easy$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


delta_LLhood_EvD[modality == "vestibular" & model == "AF" & level == "difficult", "mean_difference"] <- 
  mean(vestibularAF_loglik_difficult$loglikelihoodAF_generative - vestibularAF_loglik_difficult$loglikelihoodAF)

delta_LLhood_EvD[modality == "vestibular" & model == "AF" & level == "difficult", "mean_diff_to_stim_only"] <- 
  mean(vestibularStim_loglik_difficult$loglikelihoodStim - vestibularAF_loglik_difficult$loglikelihoodAF)

delta_LLhood_EvD[modality == "vestibular" & model == "AF" & level == "difficult", "SEM"] <- 
  stderror(vestibularAF_loglik_difficult$loglikelihoodAF_generative - vestibularAF_loglik_difficult$loglikelihoodAF)

delta_LLhood_EvD[modality == "vestibular" & model == "AF" & level == "difficult", "SEM_stim_only"] <- 
  stderror(vestibularStim_loglik_difficult$loglikelihoodStim - vestibularAF_loglik_difficult$loglikelihoodAF)

delta_LLhood_EvD[modality == "vestibular" & model == "AF" & level == "difficult", "statistic"] <- 
  wilcox.test(vestibularAF_loglik_difficult$loglikelihoodAF_generative - vestibularAF_loglik_difficult$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "vestibular" & model == "AF" & level == "difficult", "statistic_stim_only"] <- 
  wilcox.test(vestibularStim_loglik_difficult$loglikelihoodStim - vestibularAF_loglik_difficult$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "vestibular" & model == "AF" & level == "difficult", "p.value"] <- 
  wilcox.test(vestibularAF_loglik_difficult$loglikelihoodAF_generative - vestibularAF_loglik_difficult$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

delta_LLhood_EvD[modality == "vestibular" & model == "AF" & level == "difficult", "p.value_stim_only"] <- 
  wilcox.test(vestibularStim_loglik_difficult$loglikelihoodStim - vestibularAF_loglik_difficult$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)





## == VISUAL == ##

# Transition Probability 
delta_LLhood_EvD[modality == "visual" & model == "TP" & level == "easy", "mean_difference"] <- 
  mean(visualTP_loglik_easy$loglikelihoodTP_generative - visualTP_loglik_easy$loglikelihoodTP)

delta_LLhood_EvD[modality == "visual" & model == "TP" & level == "easy", "mean_diff_to_stim_only"] <- 
  mean(visualStim_loglik_easy$loglikelihoodStim - visualTP_loglik_easy$loglikelihoodTP)

delta_LLhood_EvD[modality == "visual" & model == "TP" & level == "easy", "SEM"] <- 
  stderror(visualTP_loglik_easy$loglikelihoodTP_generative - visualTP_loglik_easy$loglikelihoodTP)

delta_LLhood_EvD[modality == "visual" & model == "TP" & level == "easy", "SEM_stim_only"] <- 
  stderror(visualStim_loglik_easy$loglikelihoodStim - visualTP_loglik_easy$loglikelihoodTP)

delta_LLhood_EvD[modality == "visual" & model == "TP" & level == "easy", "statistic"] <- 
  wilcox.test(visualTP_loglik_easy$loglikelihoodTP_generative - visualTP_loglik_easy$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "visual" & model == "TP" & level == "easy", "statistic_stim_only"] <- 
  wilcox.test(visualStim_loglik_easy$loglikelihoodStim - visualTP_loglik_easy$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "visual" & model == "TP" & level == "easy", "p.value"] <- 
  wilcox.test(visualTP_loglik_easy$loglikelihoodTP_generative - visualTP_loglik_easy$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

delta_LLhood_EvD[modality == "visual" & model == "TP" & level == "easy", "p.value_stim_only"] <- 
  wilcox.test(visualStim_loglik_easy$loglikelihoodStim - visualTP_loglik_easy$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


delta_LLhood_EvD[modality == "visual" & model == "TP" & level == "difficult", "mean_difference"] <- 
  mean(visualTP_loglik_difficult$loglikelihoodTP_generative - visualTP_loglik_difficult$loglikelihoodTP)

delta_LLhood_EvD[modality == "visual" & model == "TP" & level == "difficult", "mean_diff_to_stim_only"] <- 
  mean(visualStim_loglik_difficult$loglikelihoodStim - visualTP_loglik_difficult$loglikelihoodTP)

delta_LLhood_EvD[modality == "visual" & model == "TP" & level == "difficult", "SEM"] <- 
  stderror(visualTP_loglik_difficult$loglikelihoodTP_generative - visualTP_loglik_difficult$loglikelihoodTP)

delta_LLhood_EvD[modality == "visual" & model == "TP" & level == "difficult", "SEM_stim_only"] <- 
  stderror(visualStim_loglik_difficult$loglikelihoodStim - visualTP_loglik_difficult$loglikelihoodTP)

delta_LLhood_EvD[modality == "visual" & model == "TP" & level == "difficult", "statistic"] <- 
  wilcox.test(visualTP_loglik_difficult$loglikelihoodTP_generative - visualTP_loglik_difficult$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "visual" & model == "TP" & level == "difficult", "statistic_stim_only"] <- 
  wilcox.test(visualStim_loglik_difficult$loglikelihoodStim - visualTP_loglik_difficult$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "visual" & model == "TP" & level == "difficult", "p.value"] <- 
  wilcox.test(visualTP_loglik_difficult$loglikelihoodTP_generative - visualTP_loglik_difficult$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

delta_LLhood_EvD[modality == "visual" & model == "TP" & level == "difficult", "p.value_stim_only"] <- 
  wilcox.test(visualStim_loglik_difficult$loglikelihoodStim - visualTP_loglik_difficult$loglikelihoodTP, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)




# Item Frequency
delta_LLhood_EvD[modality == "visual" & model == "IF" & level == "easy", "mean_difference"] <- 
  mean(visualIF_loglik_easy$loglikelihoodIF_generative - visualIF_loglik_easy$loglikelihoodIF)

delta_LLhood_EvD[modality == "visual" & model == "IF" & level == "easy", "mean_diff_to_stim_only"] <- 
  mean(visualStim_loglik_easy$loglikelihoodStim - visualIF_loglik_easy$loglikelihoodIF)

delta_LLhood_EvD[modality == "visual" & model == "IF" & level == "easy", "SEM"] <- 
  stderror(visualIF_loglik_easy$loglikelihoodIF_generative - visualIF_loglik_easy$loglikelihoodIF)

delta_LLhood_EvD[modality == "visual" & model == "IF" & level == "easy", "SEM_stim_only"] <- 
  stderror(visualStim_loglik_easy$loglikelihoodStim - visualIF_loglik_easy$loglikelihoodIF)

delta_LLhood_EvD[modality == "visual" & model == "IF" & level == "easy", "statistic"] <- 
  wilcox.test(visualIF_loglik_easy$loglikelihoodIF_generative - visualIF_loglik_easy$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "visual" & model == "IF" & level == "easy", "statistic_stim_only"] <- 
  wilcox.test(visualStim_loglik_easy$loglikelihoodStim - visualIF_loglik_easy$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "visual" & model == "IF" & level == "easy", "p.value"] <- 
  wilcox.test(visualIF_loglik_easy$loglikelihoodIF_generative - visualIF_loglik_easy$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

delta_LLhood_EvD[modality == "visual" & model == "IF" & level == "easy", "p.value_stim_only"] <- 
  wilcox.test(visualStim_loglik_easy$loglikelihoodStim - visualIF_loglik_easy$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


delta_LLhood_EvD[modality == "visual" & model == "IF" & level == "difficult", "mean_difference"] <- 
  mean(visualIF_loglik_difficult$loglikelihoodIF_generative - visualIF_loglik_difficult$loglikelihoodIF)

delta_LLhood_EvD[modality == "visual" & model == "IF" & level == "difficult", "mean_diff_to_stim_only"] <- 
  mean(visualStim_loglik_difficult$loglikelihoodStim - visualIF_loglik_difficult$loglikelihoodIF)

delta_LLhood_EvD[modality == "visual" & model == "IF" & level == "difficult", "SEM"] <- 
  stderror(visualIF_loglik_difficult$loglikelihoodIF_generative - visualIF_loglik_difficult$loglikelihoodIF)

delta_LLhood_EvD[modality == "visual" & model == "IF" & level == "difficult", "SEM_stim_only"] <- 
  stderror(visualStim_loglik_difficult$loglikelihoodStim - visualIF_loglik_difficult$loglikelihoodIF)

delta_LLhood_EvD[modality == "visual" & model == "IF" & level == "difficult", "statistic"] <- 
  wilcox.test(visualIF_loglik_difficult$loglikelihoodIF_generative - visualIF_loglik_difficult$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "visual" & model == "IF" & level == "difficult", "statistic_stim_only"] <- 
  wilcox.test(visualStim_loglik_difficult$loglikelihoodStim - visualIF_loglik_difficult$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "visual" & model == "IF" & level == "difficult", "p.value"] <- 
  wilcox.test(visualIF_loglik_difficult$loglikelihoodIF_generative - visualIF_loglik_difficult$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

delta_LLhood_EvD[modality == "visual" & model == "IF" & level == "difficult", "p.value_stim_only"] <- 
  wilcox.test(visualStim_loglik_difficult$loglikelihoodStim - visualIF_loglik_difficult$loglikelihoodIF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# Alternation Frequency
delta_LLhood_EvD[modality == "visual" & model == "AF" & level == "easy", "mean_difference"] <- 
  mean(visualAF_loglik_easy$loglikelihoodAF_generative - visualAF_loglik_easy$loglikelihoodAF)

delta_LLhood_EvD[modality == "visual" & model == "AF" & level == "easy", "mean_diff_to_stim_only"] <- 
  mean(visualStim_loglik_easy$loglikelihoodStim - visualAF_loglik_easy$loglikelihoodAF)

delta_LLhood_EvD[modality == "visual" & model == "AF" & level == "easy", "SEM"] <- 
  stderror(visualAF_loglik_easy$loglikelihoodAF_generative - visualAF_loglik_easy$loglikelihoodAF)

delta_LLhood_EvD[modality == "visual" & model == "AF" & level == "easy", "SEM_stim_only"] <- 
  stderror(visualStim_loglik_easy$loglikelihoodStim - visualAF_loglik_easy$loglikelihoodAF)

delta_LLhood_EvD[modality == "visual" & model == "AF" & level == "easy", "statistic"] <- 
  wilcox.test(visualAF_loglik_easy$loglikelihoodAF_generative - visualAF_loglik_easy$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "visual" & model == "AF" & level == "easy", "statistic_stim_only"] <- 
  wilcox.test(visualStim_loglik_easy$loglikelihoodStim - visualAF_loglik_easy$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "visual" & model == "AF" & level == "easy", "p.value"] <- 
  wilcox.test(visualAF_loglik_easy$loglikelihoodAF_generative - visualAF_loglik_easy$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

delta_LLhood_EvD[modality == "visual" & model == "AF" & level == "easy", "p.value_stim_only"] <- 
  wilcox.test(visualStim_loglik_easy$loglikelihoodStim - visualAF_loglik_easy$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


delta_LLhood_EvD[modality == "visual" & model == "AF" & level == "difficult", "mean_difference"] <- 
  mean(visualAF_loglik_difficult$loglikelihoodAF_generative - visualAF_loglik_difficult$loglikelihoodAF)

delta_LLhood_EvD[modality == "visual" & model == "AF" & level == "difficult", "mean_diff_to_stim_only"] <- 
  mean(visualStim_loglik_difficult$loglikelihoodStim - visualAF_loglik_difficult$loglikelihoodAF)

delta_LLhood_EvD[modality == "visual" & model == "AF" & level == "difficult", "SEM"] <- 
  stderror(visualAF_loglik_difficult$loglikelihoodAF_generative - visualAF_loglik_difficult$loglikelihoodAF)

delta_LLhood_EvD[modality == "visual" & model == "AF" & level == "difficult", "SEM_stim_only"] <- 
  stderror(visualStim_loglik_difficult$loglikelihoodStim - visualAF_loglik_difficult$loglikelihoodAF)

delta_LLhood_EvD[modality == "visual" & model == "AF" & level == "difficult", "statistic"] <- 
  wilcox.test(visualAF_loglik_difficult$loglikelihoodAF_generative - visualAF_loglik_difficult$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "visual" & model == "AF" & level == "difficult", "statistic_stim_only"] <- 
  wilcox.test(visualStim_loglik_difficult$loglikelihoodStim - visualAF_loglik_difficult$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

delta_LLhood_EvD[modality == "visual" & model == "AF" & level == "difficult", "p.value"] <- 
  wilcox.test(visualAF_loglik_difficult$loglikelihoodAF_generative - visualAF_loglik_difficult$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

delta_LLhood_EvD[modality == "visual" & model == "AF" & level == "difficult", "p.value_stim_only"] <- 
  wilcox.test(visualStim_loglik_difficult$loglikelihoodStim - visualAF_loglik_difficult$loglikelihoodAF, 
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# delta to compare learning models against each other
delta_LLhood_EvD <- delta_LLhood_EvD |> 
  mutate(Significance = ifelse(p.value < 0.05, yes = "Significant", no = "Non-Significant")) |> 
  mutate(Significance = factor(Significance, levels = c("Significant", "Non-Significant")))


# delta to compare learning models against stimulus-only model
delta_LLhood_EvD <- delta_LLhood_EvD |> 
  mutate(Significance_stim_only = ifelse(p.value_stim_only < 0.05, yes = "Significant", no = "Non-Significant")) |> 
  mutate(Significance_stim_only = factor(Significance_stim_only, levels = c("Significant", "Non-Significant"))) |> 
  mutate(p.symbol = ifelse(p.value <= 0.05 & p.value > 0.01, yes = "*",
                           ifelse(p.value <= 0.01 & p.value > 0.001, yes = "**",
                                  ifelse(p.value <= 0.001, yes = "***", no = " "))))


# Save data
save(delta_LLhood_EvD, file = "processed_data/Results/df_LLhood_delta.Rdata")

# Print LaTeX Table
delta_forLatex <- delta_LLhood_EvD |> 
  select(modality, model, level, mean_difference, SEM, statistic, p.value, Significance, p.symbol)

# Create LaTeX code
latex_table <- xtable::xtable(delta_forLatex, digits = 5)

# Print the LaTeX code
print(latex_table, include.rownames = FALSE)

