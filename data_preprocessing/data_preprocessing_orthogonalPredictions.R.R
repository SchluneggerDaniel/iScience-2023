# Data preprocessing for quantifying orthogonal predicitons between
# response-based models and stimulus-based models

library(fs)
library(tidyverse)

### === SOURCE FUNCTION === ###

source("functions/CountRatio.R")

### === LOAD DATA === ###

file_list <- dir_ls("processed_data/LLhood_Easy_vs_Difficult/") |> 
  path_filter(" |loglik_easy.Rda|Stim_loglik_difficult.Rda", invert = TRUE)

# Load all response-based model data at once
lapply(file_list, load, .GlobalEnv)

file_list_generative <- dir_ls("processed_data/LLhood_Easy_vs_Difficult_generative/") |> 
  path_filter(" |loglik_easy_generative.Rda", invert = TRUE)

# Load all stimulus-based model data at once
lapply(file_list_generative, load, .GlobalEnv)


### === COMPUTE ORTHOGONAL PREDICTIONS RATIO === ###

# Create a vector of indices for auditory data sets
indices_auditory <- 1:nrow(auditoryTP_loglik_difficult)
indices_vestibular <- 1:nrow(vestibularTP_loglik_difficult)
indices_visual <- 1:nrow(visualTP_loglik_difficult)


## Auditory, TP
# Use map to apply CountRatio to each element of the list
results_orthogonalTP_auditory <- map(indices_auditory, ~ CountRatio_TP(auditoryTP_loglik_difficult$data[[.]], 
                                                               auditoryTP_loglik_difficult_generative$data[[.]]))
# Convert to dataframe
orthogonalTP_auditory <- as.data.frame(do.call(rbind, results_orthogonalTP_auditory))


## Auditory, IF
# Use map to apply CountRatio to each element of the list
results_orthogonalIF_auditory <- map(indices_auditory, ~ CountRatio_IF(auditoryIF_loglik_difficult$data[[.]], 
                                                                       auditoryIF_loglik_difficult_generative$data[[.]]))
# Convert to dataframe
orthogonalIF_auditory <- as.data.frame(do.call(rbind, results_orthogonalIF_auditory))


## Auditory, AF
# Use map to apply CountRatio to each element of the list
results_orthogonalAF_auditory <- map(indices_auditory, ~ CountRatio_AF(auditoryAF_loglik_difficult$data[[.]], 
                                                                       auditoryAF_loglik_difficult_generative$data[[.]]))
# Convert to dataframe
orthogonalAF_auditory <- as.data.frame(do.call(rbind, results_orthogonalAF_auditory))



## Vestibular, TP
# Use map to apply CountRatio to each element of the list
results_orthogonalTP_vestibular <- map(indices_vestibular, ~ CountRatio_TP(vestibularTP_loglik_difficult$data[[.]], 
                                                                       vestibularTP_loglik_difficult_generative$data[[.]]))
# Convert to dataframe
orthogonalTP_vestibular <- as.data.frame(do.call(rbind, results_orthogonalTP_vestibular))


## Vestibular, IF
# Use map to apply CountRatio to each element of the list
results_orthogonalIF_vestibular <- map(indices_vestibular, ~ CountRatio_IF(vestibularIF_loglik_difficult$data[[.]], 
                                                                       vestibularIF_loglik_difficult_generative$data[[.]]))
# Convert to dataframe
orthogonalIF_vestibular <- as.data.frame(do.call(rbind, results_orthogonalIF_vestibular))


## Vestibular, AF
# Use map to apply CountRatio to each element of the list
results_orthogonalAF_vestibular <- map(indices_vestibular, ~ CountRatio_AF(vestibularAF_loglik_difficult$data[[.]], 
                                                                       vestibularAF_loglik_difficult_generative$data[[.]]))
# Convert to dataframe
orthogonalAF_vestibular <- as.data.frame(do.call(rbind, results_orthogonalAF_vestibular))




## Visual, TP
# Use map to apply CountRatio to each element of the list
results_orthogonalTP_visual <- map(indices_visual, ~ CountRatio_TP(visualTP_loglik_difficult$data[[.]], 
                                                                       visualTP_loglik_difficult_generative$data[[.]]))
# Convert to dataframe
orthogonalTP_visual <- as.data.frame(do.call(rbind, results_orthogonalTP_visual))


## Visual, IF
# Use map to apply CountRatio to each element of the list
results_orthogonalIF_visual <- map(indices_visual, ~ CountRatio_IF(visualIF_loglik_difficult$data[[.]], 
                                                                       visualIF_loglik_difficult_generative$data[[.]]))
# Convert to dataframe
orthogonalIF_visual <- as.data.frame(do.call(rbind, results_orthogonalIF_visual))


## Visual, AF
# Use map to apply CountRatio to each element of the list
results_orthogonalAF_visual <- map(indices_visual, ~ CountRatio_AF(visualAF_loglik_difficult$data[[.]], 
                                                                       visualAF_loglik_difficult_generative$data[[.]]))
# Convert to dataframe
orthogonalAF_visual <- as.data.frame(do.call(rbind, results_orthogonalAF_visual))

# Save data
save(orthogonalTP_auditory, file = "processed_data/LLhood_orthogonalPredictions/orthogonalTP_auditory.Rdata")
save(orthogonalIF_auditory, file = "processed_data/LLhood_orthogonalPredictions/orthogonalIF_auditory.Rdata")
save(orthogonalAF_auditory, file = "processed_data/LLhood_orthogonalPredictions/orthogonalAF_auditory.Rdata")

save(orthogonalTP_vestibular, file = "processed_data/LLhood_orthogonalPredictions/orthogonalTP_vestibular.Rdata")
save(orthogonalIF_vestibular, file = "processed_data/LLhood_orthogonalPredictions/orthogonalIF_vestibular.Rdata")
save(orthogonalAF_vestibular, file = "processed_data/LLhood_orthogonalPredictions/orthogonalAF_vestibular.Rdata")

save(orthogonalTP_visual, file = "processed_data/LLhood_orthogonalPredictions/orthogonalTP_visual.Rdata")
save(orthogonalIF_visual, file = "processed_data/LLhood_orthogonalPredictions/orthogonalIF_visual.Rdata")
save(orthogonalAF_visual, file = "processed_data/LLhood_orthogonalPredictions/orthogonalAF_visual.Rdata")

