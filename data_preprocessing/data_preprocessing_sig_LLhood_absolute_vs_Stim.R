# Test significance between learning models and Stim-Only Model

# Purpose: For significance test (mainly for reply letter)

# Script for plot preparation: data_preprocessing_LLhood_absolute_vs_Stim.R

library(fs)
library(tidyverse)

### === LOAD DATA === ###

file_list <- dir_ls("processed_data/LLhood_Easy_vs_Difficult/")

# Load all data at once
lapply(file_list, load, .GlobalEnv)

# Prepare variables for data frame
modality <- rep(c("auditory", "vestibular", "visual"), each = 6)
comparison <- rep(c("TP vs Stim-Only", "IF vs Stim-Only", "AF vs Stim-Only"), each = 2, times = 3)
level <- rep(c("easy", "difficult"), times = 9) |> factor(levels = c("easy", "difficult"))

# Create pseudovariables
mean_difference <- 1:length(comparison) |> as.double()
statistic <- 1:length(comparison) |> as.double()
p.value <- 1:length(comparison) |> as.double()

# Create tibble
absolute_LLhood_learningmodels <- tibble(modality, 
                           level, 
                           comparison,
                           mean_difference, 
                           statistic, 
                           p.value) |> 
  mutate(across(where(is.character), factor))

### === SIGNIFICANCE TESTS === ###

# In wilcox.test --> [[1]] == statistic | [[3]] == p.value

## == AUDITORY == ##


# TP vs Stim-Only, easy
absolute_LLhood_learningmodels[modality == "auditory" & comparison == "TP vs Stim-Only" 
                               & level == "easy", "mean_difference"] <- 
  mean(auditoryTP_loglik_easy$loglikelihoodTP - auditoryStim_loglik_easy$loglikelihoodStim)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "TP vs Stim-Only" 
                               & level == "easy", "statistic"] <- 
  wilcox.test(auditoryTP_loglik_easy$loglikelihoodTP - auditoryStim_loglik_easy$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "TP vs Stim-Only" 
                               & level == "easy", "p.value"] <- 
  wilcox.test(auditoryTP_loglik_easy$loglikelihoodTP - auditoryStim_loglik_easy$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# TP vs Stim-Only, difficult
absolute_LLhood_learningmodels[modality == "auditory" & comparison == "TP vs Stim-Only" 
                               & level == "difficult", "mean_difference"] <- 
  mean(auditoryTP_loglik_difficult$loglikelihoodTP - auditoryStim_loglik_difficult$loglikelihoodStim)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "TP vs Stim-Only" 
                               & level == "difficult", "statistic"] <- 
  wilcox.test(auditoryTP_loglik_difficult$loglikelihoodTP - auditoryStim_loglik_difficult$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "TP vs Stim-Only" 
                               & level == "difficult", "p.value"] <- 
  wilcox.test(auditoryTP_loglik_difficult$loglikelihoodTP - auditoryStim_loglik_difficult$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# IF vs Stim-Only, easy
absolute_LLhood_learningmodels[modality == "auditory" & comparison == "IF vs Stim-Only" 
                               & level == "easy", "mean_difference"] <- 
  mean(auditoryIF_loglik_easy$loglikelihoodIF - auditoryStim_loglik_easy$loglikelihoodStim)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "IF vs Stim-Only" 
                               & level == "easy", "statistic"] <- 
  wilcox.test(auditoryIF_loglik_easy$loglikelihoodIF - auditoryStim_loglik_easy$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "IF vs Stim-Only" 
                               & level == "easy", "p.value"] <- 
  wilcox.test(auditoryIF_loglik_easy$loglikelihoodIF - auditoryStim_loglik_easy$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# IF vs Stim-Only, difficult
absolute_LLhood_learningmodels[modality == "auditory" & comparison == "IF vs Stim-Only" 
                               & level == "difficult", "mean_difference"] <- 
  mean(auditoryIF_loglik_difficult$loglikelihoodIF - auditoryStim_loglik_difficult$loglikelihoodStim)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "IF vs Stim-Only" 
                               & level == "difficult", "statistic"] <- 
  wilcox.test(auditoryIF_loglik_difficult$loglikelihoodIF - auditoryStim_loglik_difficult$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "IF vs Stim-Only" 
                               & level == "difficult", "p.value"] <- 
  wilcox.test(auditoryIF_loglik_difficult$loglikelihoodIF - auditoryStim_loglik_difficult$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)



# AF vs Stim-Only, easy
absolute_LLhood_learningmodels[modality == "auditory" & comparison == "AF vs Stim-Only" 
                               & level == "easy", "mean_difference"] <- 
  mean(auditoryAF_loglik_easy$loglikelihoodAF - auditoryStim_loglik_easy$loglikelihoodStim)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "AF vs Stim-Only" 
                               & level == "easy", "statistic"] <- 
  wilcox.test(auditoryAF_loglik_easy$loglikelihoodAF - auditoryStim_loglik_easy$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "AF vs Stim-Only" 
                               & level == "easy", "p.value"] <- 
  wilcox.test(auditoryAF_loglik_easy$loglikelihoodAF - auditoryStim_loglik_easy$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# AF vs Stim-Only, difficult
absolute_LLhood_learningmodels[modality == "auditory" & comparison == "AF vs Stim-Only" 
                               & level == "difficult", "mean_difference"] <- 
  mean(auditoryAF_loglik_difficult$loglikelihoodAF - auditoryStim_loglik_difficult$loglikelihoodStim)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "AF vs Stim-Only" 
                               & level == "difficult", "statistic"] <- 
  wilcox.test(auditoryAF_loglik_difficult$loglikelihoodAF - auditoryStim_loglik_difficult$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "AF vs Stim-Only" 
                               & level == "difficult", "p.value"] <- 
  wilcox.test(auditoryAF_loglik_difficult$loglikelihoodAF - auditoryStim_loglik_difficult$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)










## == VESTIBULAR == ##


# TP vs Stim-Only, easy
absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "TP vs Stim-Only" 
                               & level == "easy", "mean_difference"] <- 
  mean(vestibularTP_loglik_easy$loglikelihoodTP - vestibularStim_loglik_easy$loglikelihoodStim)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "TP vs Stim-Only" 
                               & level == "easy", "statistic"] <- 
  wilcox.test(vestibularTP_loglik_easy$loglikelihoodTP - vestibularStim_loglik_easy$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "TP vs Stim-Only" 
                               & level == "easy", "p.value"] <- 
  wilcox.test(vestibularTP_loglik_easy$loglikelihoodTP - vestibularStim_loglik_easy$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# TP vs Stim-Only, difficult
absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "TP vs Stim-Only" 
                               & level == "difficult", "mean_difference"] <- 
  mean(vestibularTP_loglik_difficult$loglikelihoodTP - vestibularStim_loglik_difficult$loglikelihoodStim)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "TP vs Stim-Only" 
                               & level == "difficult", "statistic"] <- 
  wilcox.test(vestibularTP_loglik_difficult$loglikelihoodTP - vestibularStim_loglik_difficult$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "TP vs Stim-Only" 
                               & level == "difficult", "p.value"] <- 
  wilcox.test(vestibularTP_loglik_difficult$loglikelihoodTP - vestibularStim_loglik_difficult$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# IF vs Stim-Only, easy
absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "IF vs Stim-Only" 
                               & level == "easy", "mean_difference"] <- 
  mean(vestibularIF_loglik_easy$loglikelihoodIF - vestibularStim_loglik_easy$loglikelihoodStim)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "IF vs Stim-Only" 
                               & level == "easy", "statistic"] <- 
  wilcox.test(vestibularIF_loglik_easy$loglikelihoodIF - vestibularStim_loglik_easy$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "IF vs Stim-Only" 
                               & level == "easy", "p.value"] <- 
  wilcox.test(vestibularIF_loglik_easy$loglikelihoodIF - vestibularStim_loglik_easy$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# IF vs Stim-Only, difficult
absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "IF vs Stim-Only" 
                               & level == "difficult", "mean_difference"] <- 
  mean(vestibularIF_loglik_difficult$loglikelihoodIF - vestibularStim_loglik_difficult$loglikelihoodStim)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "IF vs Stim-Only" 
                               & level == "difficult", "statistic"] <- 
  wilcox.test(vestibularIF_loglik_difficult$loglikelihoodIF - vestibularStim_loglik_difficult$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "IF vs Stim-Only" 
                               & level == "difficult", "p.value"] <- 
  wilcox.test(vestibularIF_loglik_difficult$loglikelihoodIF - vestibularStim_loglik_difficult$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)



# AF vs Stim-Only, easy
absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "AF vs Stim-Only" 
                               & level == "easy", "mean_difference"] <- 
  mean(vestibularAF_loglik_easy$loglikelihoodAF - vestibularStim_loglik_easy$loglikelihoodStim)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "AF vs Stim-Only" 
                               & level == "easy", "statistic"] <- 
  wilcox.test(vestibularAF_loglik_easy$loglikelihoodAF - vestibularStim_loglik_easy$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "AF vs Stim-Only" 
                               & level == "easy", "p.value"] <- 
  wilcox.test(vestibularAF_loglik_easy$loglikelihoodAF - vestibularStim_loglik_easy$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# AF vs Stim-Only, difficult
absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "AF vs Stim-Only" 
                               & level == "difficult", "mean_difference"] <- 
  mean(vestibularAF_loglik_difficult$loglikelihoodAF - vestibularStim_loglik_difficult$loglikelihoodStim)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "AF vs Stim-Only" 
                               & level == "difficult", "statistic"] <- 
  wilcox.test(vestibularAF_loglik_difficult$loglikelihoodAF - vestibularStim_loglik_difficult$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "AF vs Stim-Only" 
                               & level == "difficult", "p.value"] <- 
  wilcox.test(vestibularAF_loglik_difficult$loglikelihoodAF - vestibularStim_loglik_difficult$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)





## == VISUAL == ##

# TP vs Stim-Only, easy
absolute_LLhood_learningmodels[modality == "visual" & comparison == "TP vs Stim-Only" 
                               & level == "easy", "mean_difference"] <- 
  mean(visualTP_loglik_easy$loglikelihoodTP - visualStim_loglik_easy$loglikelihoodStim)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "TP vs Stim-Only" 
                               & level == "easy", "statistic"] <- 
  wilcox.test(visualTP_loglik_easy$loglikelihoodTP - visualStim_loglik_easy$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "TP vs Stim-Only" 
                               & level == "easy", "p.value"] <- 
  wilcox.test(visualTP_loglik_easy$loglikelihoodTP - visualStim_loglik_easy$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# TP vs Stim-Only, difficult
absolute_LLhood_learningmodels[modality == "visual" & comparison == "TP vs Stim-Only" 
                               & level == "difficult", "mean_difference"] <- 
  mean(visualTP_loglik_difficult$loglikelihoodTP - visualStim_loglik_difficult$loglikelihoodStim)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "TP vs Stim-Only" 
                               & level == "difficult", "statistic"] <- 
  wilcox.test(visualTP_loglik_difficult$loglikelihoodTP - visualStim_loglik_difficult$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "TP vs Stim-Only" 
                               & level == "difficult", "p.value"] <- 
  wilcox.test(visualTP_loglik_difficult$loglikelihoodTP - visualStim_loglik_difficult$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# IF vs Stim-Only, easy
absolute_LLhood_learningmodels[modality == "visual" & comparison == "IF vs Stim-Only" 
                               & level == "easy", "mean_difference"] <- 
  mean(visualIF_loglik_easy$loglikelihoodIF - visualStim_loglik_easy$loglikelihoodStim)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "IF vs Stim-Only" 
                               & level == "easy", "statistic"] <- 
  wilcox.test(visualIF_loglik_easy$loglikelihoodIF - visualStim_loglik_easy$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "IF vs Stim-Only" 
                               & level == "easy", "p.value"] <- 
  wilcox.test(visualIF_loglik_easy$loglikelihoodIF - visualStim_loglik_easy$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# IF vs Stim-Only, difficult
absolute_LLhood_learningmodels[modality == "visual" & comparison == "IF vs Stim-Only" 
                               & level == "difficult", "mean_difference"] <- 
  mean(visualIF_loglik_difficult$loglikelihoodIF - visualStim_loglik_difficult$loglikelihoodStim)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "IF vs Stim-Only" 
                               & level == "difficult", "statistic"] <- 
  wilcox.test(visualIF_loglik_difficult$loglikelihoodIF - visualStim_loglik_difficult$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "IF vs Stim-Only" 
                               & level == "difficult", "p.value"] <- 
  wilcox.test(visualIF_loglik_difficult$loglikelihoodIF - visualStim_loglik_difficult$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)



# AF vs Stim-Only, easy
absolute_LLhood_learningmodels[modality == "visual" & comparison == "AF vs Stim-Only" 
                               & level == "easy", "mean_difference"] <- 
  mean(visualAF_loglik_easy$loglikelihoodAF - visualStim_loglik_easy$loglikelihoodStim)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "AF vs Stim-Only" 
                               & level == "easy", "statistic"] <- 
  wilcox.test(visualAF_loglik_easy$loglikelihoodAF - visualStim_loglik_easy$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "AF vs Stim-Only" 
                               & level == "easy", "p.value"] <- 
  wilcox.test(visualAF_loglik_easy$loglikelihoodAF - visualStim_loglik_easy$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# AF vs Stim-Only, difficult
absolute_LLhood_learningmodels[modality == "visual" & comparison == "AF vs Stim-Only" 
                               & level == "difficult", "mean_difference"] <- 
  mean(visualAF_loglik_difficult$loglikelihoodAF - visualStim_loglik_difficult$loglikelihoodStim)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "AF vs Stim-Only" 
                               & level == "difficult", "statistic"] <- 
  wilcox.test(visualAF_loglik_difficult$loglikelihoodAF - visualStim_loglik_difficult$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "AF vs Stim-Only" 
                               & level == "difficult", "p.value"] <- 
  wilcox.test(visualAF_loglik_difficult$loglikelihoodAF - visualStim_loglik_difficult$loglikelihoodStim,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)



### IMPORTANT: ASSIGN TO NEW VARIABLE ###
absolute_LLhood_learningmodels_vs_Stim <- absolute_LLhood_learningmodels

rm(absolute_LLhood_learningmodels)


# Add Significance and p.symbol
absolute_LLhood_learningmodels_vs_Stim <- absolute_LLhood_learningmodels_vs_Stim |> 
  mutate(Significance_vs_Stim = ifelse(p.value < 0.05, yes = "Significant", "Non-Significant")) |> 
  mutate(Significance_vs_Stim = factor(Significance_vs_Stim, levels = c("Significant", "Non-Significant"))) |> 
  mutate(p.symbol = ifelse(p.value <= 0.05 & p.value > 0.01, yes = "*",
                        ifelse(p.value <= 0.01 & p.value > 0.001, yes = "**",
                            ifelse(p.value <= 0.001, yes = "***", no = " "))))

# Save data frame
save(absolute_LLhood_learningmodels_vs_Stim, file = "processed_data/Results/df_LLhood_sig_absolute_vs_Stim.Rdata")

# Create LaTeX code
latex_table <- xtable::xtable(absolute_LLhood_learningmodels_vs_Stim, digits = 5)

# Print the LaTeX code
print(latex_table, include.rownames = FALSE)

