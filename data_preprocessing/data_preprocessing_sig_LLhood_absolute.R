# Test significance between learning models 

# Purpose: For significance test

# Script for plot preparation: data_preprocessing_LLhood_absolute.R

library(fs)
library(tidyverse)

### === LOAD DATA === ###

file_list <- dir_ls("processed_data/LLhood_Easy_vs_Difficult/") |> 
  path_filter(" |Stim_loglik_easy.Rda|Stim_loglik_difficult.Rda", invert = TRUE)

# Load all data at once
lapply(file_list, load, .GlobalEnv)

glimpse(auditoryTP_loglik_easy)


# Prepare variables for data frame
modality <- rep(c("auditory", "vestibular", "visual"), each = 6)
comparison <- rep(c("TP vs IF", "TP vs AF", "IF vs AF"), each = 2, times = 3)
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

absolute_LLhood_learningmodels


### === SIGNIFICANCE TESTS === ###

# In wilcox.test --> [[1]] == statistic | [[3]] == p.value

## == AUDITORY == ##


# TP vs IF, easy
absolute_LLhood_learningmodels[modality == "auditory" & comparison == "TP vs IF" 
                               & level == "easy", "mean_difference"] <- 
  mean(auditoryTP_loglik_easy$loglikelihoodTP - auditoryIF_loglik_easy$loglikelihoodIF)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "TP vs IF" 
                               & level == "easy", "statistic"] <- 
  wilcox.test(auditoryTP_loglik_easy$loglikelihoodTP - auditoryIF_loglik_easy$loglikelihoodIF,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "TP vs IF" 
                               & level == "easy", "p.value"] <- 
  wilcox.test(auditoryTP_loglik_easy$loglikelihoodTP - auditoryIF_loglik_easy$loglikelihoodIF,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# TP vs IF, difficult
absolute_LLhood_learningmodels[modality == "auditory" & comparison == "TP vs IF" 
                               & level == "difficult", "mean_difference"] <- 
  mean(auditoryTP_loglik_difficult$loglikelihoodTP - auditoryIF_loglik_difficult$loglikelihoodIF)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "TP vs IF" 
                               & level == "difficult", "statistic"] <- 
  wilcox.test(auditoryTP_loglik_difficult$loglikelihoodTP - auditoryIF_loglik_difficult$loglikelihoodIF,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "TP vs IF" 
                               & level == "difficult", "p.value"] <- 
  wilcox.test(auditoryTP_loglik_difficult$loglikelihoodTP - auditoryIF_loglik_difficult$loglikelihoodIF,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)



# TP vs AF, easy
absolute_LLhood_learningmodels[modality == "auditory" & comparison == "TP vs AF" 
                               & level == "easy", "mean_difference"] <- 
  mean(auditoryTP_loglik_easy$loglikelihoodTP - auditoryAF_loglik_easy$loglikelihoodAF)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "TP vs AF" 
                               & level == "easy", "statistic"] <- 
  wilcox.test(auditoryTP_loglik_easy$loglikelihoodTP - auditoryAF_loglik_easy$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "TP vs AF" 
                               & level == "easy", "p.value"] <- 
  wilcox.test(auditoryTP_loglik_easy$loglikelihoodTP - auditoryAF_loglik_easy$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# TP vs AF, difficult
absolute_LLhood_learningmodels[modality == "auditory" & comparison == "TP vs AF" 
                               & level == "difficult", "mean_difference"] <- 
  mean(auditoryTP_loglik_difficult$loglikelihoodTP - auditoryAF_loglik_difficult$loglikelihoodAF)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "TP vs AF" 
                               & level == "difficult", "statistic"] <- 
  wilcox.test(auditoryTP_loglik_difficult$loglikelihoodTP - auditoryAF_loglik_difficult$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "TP vs AF" 
                               & level == "difficult", "p.value"] <- 
  wilcox.test(auditoryTP_loglik_difficult$loglikelihoodTP - auditoryAF_loglik_difficult$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)




# IF vs AF, easy
absolute_LLhood_learningmodels[modality == "auditory" & comparison == "IF vs AF" 
                               & level == "easy", "mean_difference"] <- 
  mean(auditoryIF_loglik_easy$loglikelihoodIF - auditoryAF_loglik_easy$loglikelihoodAF)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "IF vs AF" 
                               & level == "easy", "statistic"] <- 
  wilcox.test(auditoryIF_loglik_easy$loglikelihoodIF - auditoryAF_loglik_easy$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "IF vs AF" 
                               & level == "easy", "p.value"] <- 
  wilcox.test(auditoryIF_loglik_easy$loglikelihoodIF - auditoryAF_loglik_easy$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# IF vs AF, difficult
absolute_LLhood_learningmodels[modality == "auditory" & comparison == "IF vs AF" 
                               & level == "difficult", "mean_difference"] <- 
  mean(auditoryIF_loglik_difficult$loglikelihoodIF - auditoryAF_loglik_difficult$loglikelihoodAF)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "IF vs AF" 
                               & level == "difficult", "statistic"] <- 
  wilcox.test(auditoryIF_loglik_difficult$loglikelihoodIF - auditoryAF_loglik_difficult$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "auditory" & comparison == "IF vs AF" 
                               & level == "difficult", "p.value"] <- 
  wilcox.test(auditoryIF_loglik_difficult$loglikelihoodIF - auditoryAF_loglik_difficult$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


## == VESTIBULAR == ##


# TP vs IF, easy
absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "TP vs IF" 
                               & level == "easy", "mean_difference"] <- 
  mean(vestibularTP_loglik_easy$loglikelihoodTP - vestibularIF_loglik_easy$loglikelihoodIF)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "TP vs IF" 
                               & level == "easy", "statistic"] <- 
  wilcox.test(vestibularTP_loglik_easy$loglikelihoodTP - vestibularIF_loglik_easy$loglikelihoodIF,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "TP vs IF" 
                               & level == "easy", "p.value"] <- 
  wilcox.test(vestibularTP_loglik_easy$loglikelihoodTP - vestibularIF_loglik_easy$loglikelihoodIF,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# TP vs IF, difficult
absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "TP vs IF" 
                               & level == "difficult", "mean_difference"] <- 
  mean(vestibularTP_loglik_difficult$loglikelihoodTP - vestibularIF_loglik_difficult$loglikelihoodIF)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "TP vs IF" 
                               & level == "difficult", "statistic"] <- 
  wilcox.test(vestibularTP_loglik_difficult$loglikelihoodTP - vestibularIF_loglik_difficult$loglikelihoodIF,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "TP vs IF" 
                               & level == "difficult", "p.value"] <- 
  wilcox.test(vestibularTP_loglik_difficult$loglikelihoodTP - vestibularIF_loglik_difficult$loglikelihoodIF,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)



# TP vs AF, easy
absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "TP vs AF" 
                               & level == "easy", "mean_difference"] <- 
  mean(vestibularTP_loglik_easy$loglikelihoodTP - vestibularAF_loglik_easy$loglikelihoodAF)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "TP vs AF" 
                               & level == "easy", "statistic"] <- 
  wilcox.test(vestibularTP_loglik_easy$loglikelihoodTP - vestibularAF_loglik_easy$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "TP vs AF" 
                               & level == "easy", "p.value"] <- 
  wilcox.test(vestibularTP_loglik_easy$loglikelihoodTP - vestibularAF_loglik_easy$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# TP vs AF, difficult
absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "TP vs AF" 
                               & level == "difficult", "mean_difference"] <- 
  mean(vestibularTP_loglik_difficult$loglikelihoodTP - vestibularAF_loglik_difficult$loglikelihoodAF)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "TP vs AF" 
                               & level == "difficult", "statistic"] <- 
  wilcox.test(vestibularTP_loglik_difficult$loglikelihoodTP - vestibularAF_loglik_difficult$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "TP vs AF" 
                               & level == "difficult", "p.value"] <- 
  wilcox.test(vestibularTP_loglik_difficult$loglikelihoodTP - vestibularAF_loglik_difficult$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)




# IF vs AF, easy
absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "IF vs AF" 
                               & level == "easy", "mean_difference"] <- 
  mean(vestibularIF_loglik_easy$loglikelihoodIF - vestibularAF_loglik_easy$loglikelihoodAF)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "IF vs AF" 
                               & level == "easy", "statistic"] <- 
  wilcox.test(vestibularIF_loglik_easy$loglikelihoodIF - vestibularAF_loglik_easy$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "IF vs AF" 
                               & level == "easy", "p.value"] <- 
  wilcox.test(vestibularIF_loglik_easy$loglikelihoodIF - vestibularAF_loglik_easy$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# IF vs AF, difficult
absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "IF vs AF" 
                               & level == "difficult", "mean_difference"] <- 
  mean(vestibularIF_loglik_difficult$loglikelihoodIF - vestibularAF_loglik_difficult$loglikelihoodAF)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "IF vs AF" 
                               & level == "difficult", "statistic"] <- 
  wilcox.test(vestibularIF_loglik_difficult$loglikelihoodIF - vestibularAF_loglik_difficult$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "vestibular" & comparison == "IF vs AF" 
                               & level == "difficult", "p.value"] <- 
  wilcox.test(vestibularIF_loglik_difficult$loglikelihoodIF - vestibularAF_loglik_difficult$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)



## == VISUAL == ##


# TP vs IF, easy
absolute_LLhood_learningmodels[modality == "visual" & comparison == "TP vs IF" 
                               & level == "easy", "mean_difference"] <- 
  mean(visualTP_loglik_easy$loglikelihoodTP - visualIF_loglik_easy$loglikelihoodIF)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "TP vs IF" 
                               & level == "easy", "statistic"] <- 
  wilcox.test(visualTP_loglik_easy$loglikelihoodTP - visualIF_loglik_easy$loglikelihoodIF,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "TP vs IF" 
                               & level == "easy", "p.value"] <- 
  wilcox.test(visualTP_loglik_easy$loglikelihoodTP - visualIF_loglik_easy$loglikelihoodIF,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# TP vs IF, difficult
absolute_LLhood_learningmodels[modality == "visual" & comparison == "TP vs IF" 
                               & level == "difficult", "mean_difference"] <- 
  mean(visualTP_loglik_difficult$loglikelihoodTP - visualIF_loglik_difficult$loglikelihoodIF)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "TP vs IF" 
                               & level == "difficult", "statistic"] <- 
  wilcox.test(visualTP_loglik_difficult$loglikelihoodTP - visualIF_loglik_difficult$loglikelihoodIF,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "TP vs IF" 
                               & level == "difficult", "p.value"] <- 
  wilcox.test(visualTP_loglik_difficult$loglikelihoodTP - visualIF_loglik_difficult$loglikelihoodIF,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)



# TP vs AF, easy
absolute_LLhood_learningmodels[modality == "visual" & comparison == "TP vs AF" 
                               & level == "easy", "mean_difference"] <- 
  mean(visualTP_loglik_easy$loglikelihoodTP - visualAF_loglik_easy$loglikelihoodAF)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "TP vs AF" 
                               & level == "easy", "statistic"] <- 
  wilcox.test(visualTP_loglik_easy$loglikelihoodTP - visualAF_loglik_easy$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "TP vs AF" 
                               & level == "easy", "p.value"] <- 
  wilcox.test(visualTP_loglik_easy$loglikelihoodTP - visualAF_loglik_easy$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# TP vs AF, difficult
absolute_LLhood_learningmodels[modality == "visual" & comparison == "TP vs AF" 
                               & level == "difficult", "mean_difference"] <- 
  mean(visualTP_loglik_difficult$loglikelihoodTP - visualAF_loglik_difficult$loglikelihoodAF)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "TP vs AF" 
                               & level == "difficult", "statistic"] <- 
  wilcox.test(visualTP_loglik_difficult$loglikelihoodTP - visualAF_loglik_difficult$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "TP vs AF" 
                               & level == "difficult", "p.value"] <- 
  wilcox.test(visualTP_loglik_difficult$loglikelihoodTP - visualAF_loglik_difficult$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)




# IF vs AF, easy
absolute_LLhood_learningmodels[modality == "visual" & comparison == "IF vs AF" 
                               & level == "easy", "mean_difference"] <- 
  mean(visualIF_loglik_easy$loglikelihoodIF - visualAF_loglik_easy$loglikelihoodAF)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "IF vs AF" 
                               & level == "easy", "statistic"] <- 
  wilcox.test(visualIF_loglik_easy$loglikelihoodIF - visualAF_loglik_easy$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "IF vs AF" 
                               & level == "easy", "p.value"] <- 
  wilcox.test(visualIF_loglik_easy$loglikelihoodIF - visualAF_loglik_easy$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# IF vs AF, difficult
absolute_LLhood_learningmodels[modality == "visual" & comparison == "IF vs AF" 
                               & level == "difficult", "mean_difference"] <- 
  mean(visualIF_loglik_difficult$loglikelihoodIF - visualAF_loglik_difficult$loglikelihoodAF)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "IF vs AF" 
                               & level == "difficult", "statistic"] <- 
  wilcox.test(visualIF_loglik_difficult$loglikelihoodIF - visualAF_loglik_difficult$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_LLhood_learningmodels[modality == "visual" & comparison == "IF vs AF" 
                               & level == "difficult", "p.value"] <- 
  wilcox.test(visualIF_loglik_difficult$loglikelihoodIF - visualAF_loglik_difficult$loglikelihoodAF,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# In order that the analysis is correct

# Add Significance and p.symbol
absolute_LLhood_learningmodels <- absolute_LLhood_learningmodels |> 
  mutate(Significance = ifelse(p.value < 0.05, yes = "Significant", "Non-Significant")) |> 
  mutate(Significance = factor(Significance, levels = c("Significant", "Non-Significant"))) |> 
  mutate(p.symbol = ifelse(p.value <= 0.05 & p.value > 0.01, yes = "*",
                        ifelse(p.value <= 0.01 & p.value > 0.001, yes = "**",
                            ifelse(p.value <= 0.001, yes = "***", no = " "))))


# Save data frame
save(absolute_LLhood_learningmodels, file = "processed_data/Results/df_LLhood_sig_absolute.Rdata")

# Create LaTeX code
latex_table <- xtable::xtable(absolute_LLhood_learningmodels, digits = 5)

# Print the LaTeX code
print(latex_table, include.rownames = FALSE)

