# Data Preprocessing Responses (run observer models)

library(tidyverse)
library(janitor)
library(stringr)

source("functions/CountEventMarkovNoJump.R") # TP
source("functions/CountEventBernoulliNoJump.R") # IF
source("functions/CountEventBernoulliAlternationNoJump.R") # AF

# Auditory | Vestibular | Visual
df_auditory <- read.csv(file = "raw_data/auditoryData.csv")
df_vestibular <- read.csv(file = "raw_data/vestibularData.csv")
df_visual <- read.csv(file = "raw_data/visualData.csv")

# Convert factors
df_auditory <- df_auditory |> 
  mutate(across(c("subject", "sex", "condition"), factor))

df_vestibular <- df_vestibular |> 
  mutate(across(c("subject", "axis", "condition", "group"), factor))

df_visual <- df_visual |> 
  mutate(across(c("subject", "block", "condition", "session"), factor))

# Pivot to wide format
df_auditory_wider_response <- df_auditory |> 
  pivot_wider(id_cols = c("trial"), 
              names_from = c("subject", "condition"), 
              values_from = "response")

df_vestibular_wider_response <- df_vestibular |> 
  pivot_wider(id_cols = c("trial"), 
              names_from = c("subject", "axis", "condition"), 
              values_from = "response")

df_visual_wider_response <- df_visual |> 
  pivot_wider(id_cols = c("trial"), 
              names_from = c("subject", "condition", "block", "session"), 
              values_from = "response")

# Values for omega to be tested
omegaVector <- c(Inf, 1:30)

# Preallocate vectors
numberVector_auditory <- c(1:nrow(df_auditory))
numberVector_vestibular <- c(1:nrow(df_vestibular))
numberVector_visual <- c(1:nrow(df_visual))

# Preallocate vectors
allValuesMarkovBernoulli_auditory <- tibble(numberVector_auditory)
allValuesMarkovBernoulli_vestibular <- tibble(numberVector_vestibular)
allValuesMarkovBernoulli_visual <- tibble(numberVector_visual)

### === Count Event Auditory === ###

for (k in 1:length(omegaVector)){
  
  leak_auditory_Markov <- NULL
  leak_auditory_Bernoulli <- NULL
  leak_auditory_Alternation <- NULL
  
  for (i in 2:length(df_auditory_wider_response)){
    
    leak_temp_auditory_Markov <- CountEventMarkov(as_vector(df_auditory_wider_response[,i]), omega = omegaVector[k])
    leak_temp_auditory_Bernoulli <- CountEventBernoulli(as_vector(df_auditory_wider_response[,i]), omega = omegaVector[k])
    leak_temp_auditory_Alternation <- CountEventAlternation(as_vector(df_auditory_wider_response[,i]), omega = omegaVector[k])
    
    leak_auditory_Markov = rbind(leak_auditory_Markov, leak_temp_auditory_Markov)
    leak_auditory_Bernoulli = rbind(leak_auditory_Bernoulli, leak_temp_auditory_Bernoulli)
    leak_auditory_Alternation = rbind(leak_auditory_Alternation, leak_temp_auditory_Alternation)
    
  }
  
  allValuesMarkovBernoulli_auditory <- bind_cols(allValuesMarkovBernoulli_auditory, 
                                                 leak_auditory_Markov,
                                                 leak_auditory_Bernoulli,
                                                 leak_auditory_Alternation)
  
}


### === Count Event Vestibular === ###

for (k in 1:length(omegaVector)){
  
  leak_vestibular_Markov <- NULL
  leak_vestibular_Bernoulli <- NULL
  leak_vestibular_Alternation <- NULL
  
  for (i in 2:length(df_vestibular_wider_response)){
    
    leak_temp_vestibular_Markov <- CountEventMarkov(as_vector(df_vestibular_wider_response[,i]), omega = omegaVector[k]) 
    leak_temp_vestibular_Bernoulli <- CountEventBernoulli(as_vector(df_vestibular_wider_response[,i]), omega = omegaVector[k]) 
    leak_temp_vestibular_Alternation <- CountEventAlternation(as_vector(df_vestibular_wider_response[,i]), omega = omegaVector[k]) 
    
    leak_vestibular_Markov = rbind(leak_vestibular_Markov, leak_temp_vestibular_Markov)
    leak_vestibular_Bernoulli = rbind(leak_vestibular_Bernoulli, leak_temp_vestibular_Bernoulli)
    leak_vestibular_Alternation = rbind(leak_vestibular_Alternation, leak_temp_vestibular_Alternation)
    
  }
  
  allValuesMarkovBernoulli_vestibular <- bind_cols(allValuesMarkovBernoulli_vestibular, 
                                                   leak_vestibular_Markov,
                                                   leak_vestibular_Bernoulli,
                                                   leak_vestibular_Alternation)
  
}


### === Count Event Visual === ###

for (k in 1:length(omegaVector)){
  
  leak_visual_Markov <- NULL
  leak_visual_Bernoulli <- NULL
  leak_visual_Alternation <- NULL
  
  for (i in 2:length(df_visual_wider_response)){
    
    leak_temp_visual_Markov <- CountEventMarkov(as_vector(df_visual_wider_response[,i]), omega = omegaVector[k]) 
    leak_temp_visual_Bernoulli <- CountEventBernoulli(as_vector(df_visual_wider_response[,i]), omega = omegaVector[k]) 
    leak_temp_visual_Alternation <- CountEventAlternation(as_vector(df_visual_wider_response[,i]), omega = omegaVector[k]) 
    
    leak_visual_Markov = rbind(leak_visual_Markov, leak_temp_visual_Markov)
    leak_visual_Bernoulli = rbind(leak_visual_Bernoulli, leak_temp_visual_Bernoulli)
    leak_visual_Alternation = rbind(leak_visual_Alternation, leak_temp_visual_Alternation)
    
  }
  
  allValuesMarkovBernoulli_visual <- bind_cols(allValuesMarkovBernoulli_visual, 
                                               leak_visual_Markov,
                                               leak_visual_Bernoulli,
                                               leak_visual_Alternation)
  
}

# Create dataframes
allValuesMarkovBernoulli_auditory <- allValuesMarkovBernoulli_auditory |> 
  select(-starts_with(c("trial","AorB","numberVector")))

allValuesMarkovBernoulli_vestibular <- allValuesMarkovBernoulli_vestibular |> 
  select(-starts_with(c("trial","AorB","numberVector")))

allValuesMarkovBernoulli_visual <- allValuesMarkovBernoulli_visual |> 
  select(-starts_with(c("trial","AorB","numberVector")))

# Clean up names
allValuesMarkovBernoulli_auditory <- clean_names(allValuesMarkovBernoulli_auditory, case = "lower_camel")
allValuesMarkovBernoulli_vestibular <- clean_names(allValuesMarkovBernoulli_vestibular, case = "lower_camel")
allValuesMarkovBernoulli_visual <- clean_names(allValuesMarkovBernoulli_visual, case = "lower_camel")

# Create the suffix for the new column names
suffix_omega <- rep(omegaVector, each = 6) |> as.character()

# Separately for each dataframe
newnames_auditory <- str_replace(names(allValuesMarkovBernoulli_auditory), pattern = '\\d+', replacement = suffix_omega) 
newnames_vestibular <- str_replace(names(allValuesMarkovBernoulli_vestibular), pattern = '\\d+', replacement = suffix_omega) 
newnames_visual <- str_replace(names(allValuesMarkovBernoulli_visual), pattern = '\\d+', replacement = suffix_omega) 

# Add new colnames
colnames(allValuesMarkovBernoulli_auditory) <- newnames_auditory
colnames(allValuesMarkovBernoulli_vestibular) <- newnames_vestibular
colnames(allValuesMarkovBernoulli_visual) <- newnames_visual

# Finish dataframes
allValuesAuditory <- cbind(df_auditory, allValuesMarkovBernoulli_auditory)
allValuesVestibular <- cbind(df_vestibular, allValuesMarkovBernoulli_vestibular)
allValuesVisual <- cbind(df_visual, allValuesMarkovBernoulli_visual)

# Rename dataframes
allValuesAuditory_before_DPP_pt2 <- allValuesAuditory
allValuesVestibular_before_DPP_pt2 <- allValuesVestibular
allValuesVisual_before_DPP_pt2 <- allValuesVisual

# Save dataframes
save(allValuesAuditory_before_DPP_pt2, file = "processed_data/allValues/allValuesAuditory_before_DPP_pt2.Rda")
save(allValuesVestibular_before_DPP_pt2, file = "processed_data/allValues/allValuesVestibular_before_DPP_pt2.Rda")
save(allValuesVisual_before_DPP_pt2, file = "processed_data/allValues/allValuesVisual_before_DPP_pt2.Rda")

