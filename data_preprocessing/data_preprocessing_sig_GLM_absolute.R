# Test significance between learning models and a model with repetition preference

# Comparison:           - Pseudo R2 measure Nagelkerke
# Model specification:  - Probit regression (stim + intensity * learningmodel)

# Purpose: For significance test (supplemental information)

# Script for plot preparation: data_preprocessing_GLM_absolute.R

library(fs)
library(tidyverse)

### === LOAD DATA === ###

file_list <- dir_ls("processed_data/GLM")

# Load all data at once
lapply(file_list, load, .GlobalEnv)

# Prepare variables for data frame
modality <- rep(c("auditory", "vestibular", "visual"), each = 3)
comparison <- rep(c("TP vs RepPref", "IF vs RepPref", "AF vs RepPref"), times = 3)

# Create pseudovariables
mean_difference <- 1:length(comparison) |> as.double()
statistic <- 1:length(comparison) |> as.double()
p.value <- 1:length(comparison) |> as.double()


# Create tibble
absolute_GLM_learningmodels <- tibble(modality, 
                           comparison,
                           mean_difference, 
                           statistic, 
                           p.value) |> 
  mutate(across(where(is.character), factor))

absolute_GLM_learningmodels


### === SIGNIFICANCE TESTS === ###

# In wilcox.test --> [[1]] == statistic | [[3]] == p.value

## == AUDITORY == ##

# TP vs RepPref
absolute_GLM_learningmodels[modality == "auditory" & comparison == "TP vs RepPref", "mean_difference"] <- 
  mean(auditoryTP$Nagelkerke_TP - auditoryRepPref$Nagelkerke_RepPref)

absolute_GLM_learningmodels[modality == "auditory" & comparison == "TP vs RepPref", "statistic"] <- 
  wilcox.test(auditoryTP$Nagelkerke_TP - auditoryRepPref$Nagelkerke_RepPref,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_GLM_learningmodels[modality == "auditory" & comparison == "TP vs RepPref", "p.value"] <- 
  wilcox.test(auditoryTP$Nagelkerke_TP - auditoryRepPref$Nagelkerke_RepPref,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

# IF vs RepPref
absolute_GLM_learningmodels[modality == "auditory" & comparison == "IF vs RepPref", "mean_difference"] <- 
  mean(auditoryIF$Nagelkerke_IF - auditoryRepPref$Nagelkerke_RepPref)

absolute_GLM_learningmodels[modality == "auditory" & comparison == "IF vs RepPref", "statistic"] <- 
  wilcox.test(auditoryIF$Nagelkerke_IF - auditoryRepPref$Nagelkerke_RepPref,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_GLM_learningmodels[modality == "auditory" & comparison == "IF vs RepPref", "p.value"] <- 
  wilcox.test(auditoryIF$Nagelkerke_IF - auditoryRepPref$Nagelkerke_RepPref,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# AF vs RepPref
absolute_GLM_learningmodels[modality == "auditory" & comparison == "AF vs RepPref", "mean_difference"] <- 
  mean(auditoryAF$Nagelkerke_AF - auditoryRepPref$Nagelkerke_RepPref)

absolute_GLM_learningmodels[modality == "auditory" & comparison == "AF vs RepPref", "statistic"] <- 
  wilcox.test(auditoryAF$Nagelkerke_AF - auditoryRepPref$Nagelkerke_RepPref,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_GLM_learningmodels[modality == "auditory" & comparison == "AF vs RepPref", "p.value"] <- 
  wilcox.test(auditoryAF$Nagelkerke_AF - auditoryRepPref$Nagelkerke_RepPref,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)



## == VESTIBULAR == ##

# TP vs RepPref
absolute_GLM_learningmodels[modality == "vestibular" & comparison == "TP vs RepPref", "mean_difference"] <- 
  mean(vestibularTP$Nagelkerke_TP - vestibularRepPref$Nagelkerke_RepPref)

absolute_GLM_learningmodels[modality == "vestibular" & comparison == "TP vs RepPref", "statistic"] <- 
  wilcox.test(vestibularTP$Nagelkerke_TP - vestibularRepPref$Nagelkerke_RepPref,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_GLM_learningmodels[modality == "vestibular" & comparison == "TP vs RepPref", "p.value"] <- 
  wilcox.test(vestibularTP$Nagelkerke_TP - vestibularRepPref$Nagelkerke_RepPref,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

# IF vs RepPref
absolute_GLM_learningmodels[modality == "vestibular" & comparison == "IF vs RepPref", "mean_difference"] <- 
  mean(vestibularIF$Nagelkerke_IF - vestibularRepPref$Nagelkerke_RepPref)

absolute_GLM_learningmodels[modality == "vestibular" & comparison == "IF vs RepPref", "statistic"] <- 
  wilcox.test(vestibularIF$Nagelkerke_IF - vestibularRepPref$Nagelkerke_RepPref,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_GLM_learningmodels[modality == "vestibular" & comparison == "IF vs RepPref", "p.value"] <- 
  wilcox.test(vestibularIF$Nagelkerke_IF - vestibularRepPref$Nagelkerke_RepPref,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# AF vs RepPref
absolute_GLM_learningmodels[modality == "vestibular" & comparison == "AF vs RepPref", "mean_difference"] <- 
  mean(vestibularAF$Nagelkerke_AF - vestibularRepPref$Nagelkerke_RepPref)

absolute_GLM_learningmodels[modality == "vestibular" & comparison == "AF vs RepPref", "statistic"] <- 
  wilcox.test(vestibularAF$Nagelkerke_AF - vestibularRepPref$Nagelkerke_RepPref,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_GLM_learningmodels[modality == "vestibular" & comparison == "AF vs RepPref", "p.value"] <- 
  wilcox.test(vestibularAF$Nagelkerke_AF - vestibularRepPref$Nagelkerke_RepPref,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)




## == VISUAL == ##

# TP vs RepPref
absolute_GLM_learningmodels[modality == "visual" & comparison == "TP vs RepPref", "mean_difference"] <- 
  mean(visualTP$Nagelkerke_TP - visualRepPref$Nagelkerke_RepPref)

absolute_GLM_learningmodels[modality == "visual" & comparison == "TP vs RepPref", "statistic"] <- 
  wilcox.test(visualTP$Nagelkerke_TP - visualRepPref$Nagelkerke_RepPref,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_GLM_learningmodels[modality == "visual" & comparison == "TP vs RepPref", "p.value"] <- 
  wilcox.test(visualTP$Nagelkerke_TP - visualRepPref$Nagelkerke_RepPref,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)

# IF vs RepPref
absolute_GLM_learningmodels[modality == "visual" & comparison == "IF vs RepPref", "mean_difference"] <- 
  mean(visualIF$Nagelkerke_IF - visualRepPref$Nagelkerke_RepPref)

absolute_GLM_learningmodels[modality == "visual" & comparison == "IF vs RepPref", "statistic"] <- 
  wilcox.test(visualIF$Nagelkerke_IF - visualRepPref$Nagelkerke_RepPref,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_GLM_learningmodels[modality == "visual" & comparison == "IF vs RepPref", "p.value"] <- 
  wilcox.test(visualIF$Nagelkerke_IF - visualRepPref$Nagelkerke_RepPref,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)


# AF vs RepPref
absolute_GLM_learningmodels[modality == "visual" & comparison == "AF vs RepPref", "mean_difference"] <- 
  mean(visualAF$Nagelkerke_AF - visualRepPref$Nagelkerke_RepPref)

absolute_GLM_learningmodels[modality == "visual" & comparison == "AF vs RepPref", "statistic"] <- 
  wilcox.test(visualAF$Nagelkerke_AF - visualRepPref$Nagelkerke_RepPref,
              mu = 0, alternative = "two.sided")[[1]] |> round(6)

absolute_GLM_learningmodels[modality == "visual" & comparison == "AF vs RepPref", "p.value"] <- 
  wilcox.test(visualAF$Nagelkerke_AF - visualRepPref$Nagelkerke_RepPref,
              mu = 0, alternative = "two.sided")[[3]] |> round(6)



# Add Significance and p.symbol
absolute_GLM_learningmodels <- absolute_GLM_learningmodels |> 
  mutate(Significance_vs_RepPref = ifelse(p.value < 0.05, yes = "Significant", "Non-Significant")) |> 
  mutate(Significance_vs_RepPref = factor(Significance_vs_RepPref, levels = c("Significant", "Non-Significant"))) |> 
  mutate(p.symbol = ifelse(p.value <= 0.05 & p.value > 0.01, yes = "*",
                        ifelse(p.value <= 0.01 & p.value > 0.001, yes = "**",
                            ifelse(p.value <= 0.001, yes = "***", no = " "))))


# Save data frame
save(absolute_GLM_learningmodels, file = "processed_data/Results/df_GLM_sig_absolute.Rdata")
