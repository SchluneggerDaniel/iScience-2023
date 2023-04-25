# Data Preprocessing True Sequence of Stimuli (generative values)

library(tidyverse)
library(janitor)
library(stringr)

source("functions/CountEventMarkovNoJump.R")
source("functions/CountEventBernoulliNoJump.R")
source("functions/CountEventBernoulliAlternationNoJump.R")

# Auditory | Vestibular | Visual
df_auditory_generative <- read.csv(file = "raw_data/auditoryData.csv")
df_vestibular_generative <- read.csv(file = "raw_data/vestibularData.csv")
df_visual_generative <- read.csv(file = "raw_data/visualData.csv")

# Convert factors
df_auditory_generative <- df_auditory_generative |> 
  mutate(across(c("subject", "sex", "condition"), factor))

df_vestibular_generative <- df_vestibular_generative |> 
  mutate(across(c("subject", "axis", "condition", "group"), factor))

df_visual_generative <- df_visual_generative |> 
  mutate(across(c("subject", "block", "condition", "session"), factor))

# Pivot to wide format
df_auditory_wider_response_generative <- df_auditory_generative |> 
  pivot_wider(id_cols = c("trial"), 
              names_from = c("subject", "condition"), 
              values_from = "stimulus") ### ONLY CHANGE FROM data_preprocessing.R  

df_vestibular_wider_response_generative <- df_vestibular_generative |> 
  pivot_wider(id_cols = c("trial"), 
              names_from = c("subject", "axis", "condition"), 
              values_from = "stimulus") ### ONLY CHANGE FROM data_preprocessing.R 

df_visual_wider_response_generative <- df_visual_generative |> 
  pivot_wider(id_cols = c("trial"), 
              names_from = c("subject", "condition", "block", "session"), 
              values_from = "stimulus") ### ONLY CHANGE FROM data_preprocessing.R 

# Values for omega to be tested
omegaVector <- c(Inf, 1:30)

# Preallocate vectors
numberVector_auditory_generative <- c(1:nrow(df_auditory_generative))
numberVector_vestibular_generative <- c(1:nrow(df_vestibular_generative))
numberVector_visual_generative <- c(1:nrow(df_visual_generative))

# Preallocate vectors
allValuesMarkovBernoulli_auditory_generative <- tibble(numberVector_auditory_generative)
allValuesMarkovBernoulli_vestibular_generative <- tibble(numberVector_vestibular_generative)
allValuesMarkovBernoulli_visual_generative <- tibble(numberVector_visual_generative)

### === Count Event Markov Auditory === ###

for (k in 1:length(omegaVector)){
  
  leak_auditory_Markov_generative <- NULL
  leak_auditory_Bernoulli_generative <- NULL
  leak_auditory_Alternation_generative <- NULL
  
  for (i in 2:length(df_auditory_wider_response_generative)){
    
    leak_temp_auditory_Markov_generative <- CountEventMarkov(as_vector(df_auditory_wider_response_generative[,i]), omega = omegaVector[k])
    leak_temp_auditory_Bernoulli_generative <- CountEventBernoulli(as_vector(df_auditory_wider_response_generative[,i]), omega = omegaVector[k])
    leak_temp_auditory_Alternation_generative <- CountEventAlternation(as_vector(df_auditory_wider_response_generative[,i]), omega = omegaVector[k])
    
    leak_auditory_Markov_generative = rbind(leak_auditory_Markov_generative, leak_temp_auditory_Markov_generative)
    leak_auditory_Bernoulli_generative = rbind(leak_auditory_Bernoulli_generative, leak_temp_auditory_Bernoulli_generative)
    leak_auditory_Alternation_generative = rbind(leak_auditory_Alternation_generative, leak_temp_auditory_Alternation_generative)
    
  }
  
  allValuesMarkovBernoulli_auditory_generative <- bind_cols(allValuesMarkovBernoulli_auditory_generative, 
                                                 leak_auditory_Markov_generative,
                                                 leak_auditory_Bernoulli_generative,
                                                 leak_auditory_Alternation_generative)
  
}


### === Count Event Markov Vestibular === ###

for (k in 1:length(omegaVector)){
  
  leak_vestibular_Markov_generative <- NULL
  leak_vestibular_Bernoulli_generative <- NULL
  leak_vestibular_Alternation_generative <- NULL
  
  for (i in 2:length(df_vestibular_wider_response_generative)){
    
    leak_temp_vestibular_Markov_generative <- CountEventMarkov(as_vector(df_vestibular_wider_response_generative[,i]), omega = omegaVector[k]) 
    leak_temp_vestibular_Bernoulli_generative <- CountEventBernoulli(as_vector(df_vestibular_wider_response_generative[,i]), omega = omegaVector[k]) 
    leak_temp_vestibular_Alternation_generative <- CountEventAlternation(as_vector(df_vestibular_wider_response_generative[,i]), omega = omegaVector[k]) 
    
    leak_vestibular_Markov_generative = rbind(leak_vestibular_Markov_generative, leak_temp_vestibular_Markov_generative)
    leak_vestibular_Bernoulli_generative = rbind(leak_vestibular_Bernoulli_generative, leak_temp_vestibular_Bernoulli_generative)
    leak_vestibular_Alternation_generative = rbind(leak_vestibular_Alternation_generative, leak_temp_vestibular_Alternation_generative)
    
  }
  
  allValuesMarkovBernoulli_vestibular_generative <- bind_cols(allValuesMarkovBernoulli_vestibular_generative, 
                                                   leak_vestibular_Markov_generative,
                                                   leak_vestibular_Bernoulli_generative,
                                                   leak_vestibular_Alternation_generative)
  
}


### === Count Event Markov Visual === ###

for (k in 1:length(omegaVector)){
  
  leak_visual_Markov_generative <- NULL
  leak_visual_Bernoulli_generative <- NULL
  leak_visual_Alternation_generative <- NULL
  
  for (i in 2:length(df_visual_wider_response_generative)){
    
    leak_temp_visual_Markov_generative <- CountEventMarkov(as_vector(df_visual_wider_response_generative[,i]), omega = omegaVector[k]) 
    leak_temp_visual_Bernoulli_generative <- CountEventBernoulli(as_vector(df_visual_wider_response_generative[,i]), omega = omegaVector[k]) 
    leak_temp_visual_Alternation_generative <- CountEventAlternation(as_vector(df_visual_wider_response_generative[,i]), omega = omegaVector[k]) 
    
    leak_visual_Markov_generative = rbind(leak_visual_Markov_generative, leak_temp_visual_Markov_generative)
    leak_visual_Bernoulli_generative = rbind(leak_visual_Bernoulli_generative, leak_temp_visual_Bernoulli_generative)
    leak_visual_Alternation_generative = rbind(leak_visual_Alternation_generative, leak_temp_visual_Alternation_generative)
    
  }
  
  allValuesMarkovBernoulli_visual_generative <- bind_cols(allValuesMarkovBernoulli_visual_generative, 
                                               leak_visual_Markov_generative,
                                               leak_visual_Bernoulli_generative,
                                               leak_visual_Alternation_generative)
  
}

# Create dataframes
allValuesMarkovBernoulli_auditory_generative <- allValuesMarkovBernoulli_auditory_generative |> 
  select(-starts_with(c("trial","AorB","numberVector")))

allValuesMarkovBernoulli_vestibular_generative <- allValuesMarkovBernoulli_vestibular_generative |> 
  select(-starts_with(c("trial","AorB","numberVector")))

allValuesMarkovBernoulli_visual_generative <- allValuesMarkovBernoulli_visual_generative |> 
  select(-starts_with(c("trial","AorB","numberVector")))

# Clean up names
allValuesMarkovBernoulli_auditory_generative <- clean_names(allValuesMarkovBernoulli_auditory_generative, case = "lower_camel")
allValuesMarkovBernoulli_vestibular_generative <- clean_names(allValuesMarkovBernoulli_vestibular_generative, case = "lower_camel")
allValuesMarkovBernoulli_visual_generative <- clean_names(allValuesMarkovBernoulli_visual_generative, case = "lower_camel")

# Create the suffix for the new column names
suffix_omega <- rep(omegaVector, each = 6) |> as.character()

# Separately for eachd dataframe
newnames_auditory <- str_replace(names(allValuesMarkovBernoulli_auditory_generative), pattern = '\\d+', replacement = suffix_omega) 
newnames_vestibular <- str_replace(names(allValuesMarkovBernoulli_vestibular_generative), pattern = '\\d+', replacement = suffix_omega) 
newnames_visual <- str_replace(names(allValuesMarkovBernoulli_visual_generative), pattern = '\\d+', replacement = suffix_omega) 

# Add new colnames
colnames(allValuesMarkovBernoulli_auditory_generative) <- newnames_auditory
colnames(allValuesMarkovBernoulli_vestibular_generative) <- newnames_vestibular
colnames(allValuesMarkovBernoulli_visual_generative) <- newnames_visual

# Finish dataframes
allValuesAuditory_generative <- cbind(df_auditory_generative, allValuesMarkovBernoulli_auditory_generative)
allValuesVestibular_generative <- cbind(df_vestibular_generative, allValuesMarkovBernoulli_vestibular_generative)
allValuesVisual_generative <- cbind(df_visual_generative, allValuesMarkovBernoulli_visual_generative)

# Rename dataframes
allValuesAuditory_before_DPP_pt2_generative <- allValuesAuditory_generative
allValuesVestibular_before_DPP_pt2_generative <- allValuesVestibular_generative
allValuesVisual_before_DPP_pt2_generative <- allValuesVisual_generative

# Save dataframes
save(allValuesAuditory_before_DPP_pt2_generative, file = "processed_data/allValues/allValuesAuditory_before_DPP_pt2_generative.Rda")
save(allValuesVestibular_before_DPP_pt2_generative, file = "processed_data/allValues/allValuesVestibular_before_DPP_pt2_generative.Rda")
save(allValuesVisual_before_DPP_pt2_generative, file = "processed_data/allValues/allValuesVisual_before_DPP_pt2_generative.Rda")

