# Data preprocesing for LLhood vs. Stimulus-Only

# IMPORTANT: It is absolutely necessary to start a new R Session before
# running this script!

# .rs.restartR()

# Script to run before plot_significance_absoluteBIC.R

library(fs)
library(tidyverse)

### === LOAD DATA === ###

file_list <- dir_ls("processed_data/LLhood_Easy_vs_Difficult/") 

# Load all data at once
lapply(file_list, load, .GlobalEnv)

# Add variables
auditoryTP_loglik_easy$modality <- "auditory"
auditoryTP_loglik_easy$learningmodel <- "TP"
auditoryTP_loglik_easy$level <- "easy"

auditoryTP_loglik_difficult$modality <- "auditory"
auditoryTP_loglik_difficult$learningmodel <- "TP"
auditoryTP_loglik_difficult$level <- "difficult"

auditoryIF_loglik_easy$modality <- "auditory"
auditoryIF_loglik_easy$learningmodel <- "IF"
auditoryIF_loglik_easy$level <- "easy"

auditoryIF_loglik_difficult$modality <- "auditory"
auditoryIF_loglik_difficult$learningmodel <- "IF"
auditoryIF_loglik_difficult$level <- "difficult"

auditoryAF_loglik_easy$modality <- "auditory"
auditoryAF_loglik_easy$learningmodel <- "AF"
auditoryAF_loglik_easy$level <- "easy"

auditoryAF_loglik_difficult$modality <- "auditory"
auditoryAF_loglik_difficult$learningmodel <- "AF"
auditoryAF_loglik_difficult$level <- "difficult"

auditoryStim_loglik_easy$modality <- "auditory"
auditoryStim_loglik_easy$learningmodel <- "Stim"
auditoryStim_loglik_easy$level <- "easy"

auditoryStim_loglik_difficult$modality <- "auditory"
auditoryStim_loglik_difficult$learningmodel <- "Stim"
auditoryStim_loglik_difficult$level <- "difficult"


vestibularTP_loglik_easy$modality <- "vestibular"
vestibularTP_loglik_easy$learningmodel <- "TP"
vestibularTP_loglik_easy$level <- "easy"

vestibularTP_loglik_difficult$modality <- "vestibular"
vestibularTP_loglik_difficult$learningmodel <- "TP"
vestibularTP_loglik_difficult$level <- "difficult"

vestibularIF_loglik_easy$modality <- "vestibular"
vestibularIF_loglik_easy$learningmodel <- "IF"
vestibularIF_loglik_easy$level <- "easy"

vestibularIF_loglik_difficult$modality <- "vestibular"
vestibularIF_loglik_difficult$learningmodel <- "IF"
vestibularIF_loglik_difficult$level <- "difficult"

vestibularAF_loglik_easy$modality <- "vestibular"
vestibularAF_loglik_easy$learningmodel <- "AF"
vestibularAF_loglik_easy$level <- "easy"

vestibularAF_loglik_difficult$modality <- "vestibular"
vestibularAF_loglik_difficult$learningmodel <- "AF"
vestibularAF_loglik_difficult$level <- "difficult"

vestibularStim_loglik_easy$modality <- "vestibular"
vestibularStim_loglik_easy$learningmodel <- "Stim"
vestibularStim_loglik_easy$level <- "easy"

vestibularStim_loglik_difficult$modality <- "vestibular"
vestibularStim_loglik_difficult$learningmodel <- "Stim"
vestibularStim_loglik_difficult$level <- "difficult"



visualTP_loglik_easy$modality <- "visual"
visualTP_loglik_easy$learningmodel <- "TP"
visualTP_loglik_easy$level <- "easy"

visualTP_loglik_difficult$modality <- "visual"
visualTP_loglik_difficult$learningmodel <- "TP"
visualTP_loglik_difficult$level <- "difficult"

visualIF_loglik_easy$modality <- "visual"
visualIF_loglik_easy$learningmodel <- "IF"
visualIF_loglik_easy$level <- "easy"

visualIF_loglik_difficult$modality <- "visual"
visualIF_loglik_difficult$learningmodel <- "IF"
visualIF_loglik_difficult$level <- "difficult"

visualAF_loglik_easy$modality <- "visual"
visualAF_loglik_easy$learningmodel <- "AF"
visualAF_loglik_easy$level <- "easy"

visualAF_loglik_difficult$modality <- "visual"
visualAF_loglik_difficult$learningmodel <- "AF"
visualAF_loglik_difficult$level <- "difficult"

visualStim_loglik_easy$modality <- "visual"
visualStim_loglik_easy$learningmodel <- "Stim"
visualStim_loglik_easy$level <- "easy"

visualStim_loglik_difficult$modality <- "visual"
visualStim_loglik_difficult$learningmodel <- "Stim"
visualStim_loglik_difficult$level <- "difficult"


# Prepare for smooth functioning full_join()

# Auditory
auditoryTP_loglik_easy <- auditoryTP_loglik_easy |>  rename_with(~str_remove(., '_TP'))
auditoryTP_loglik_easy <- auditoryTP_loglik_easy |>  rename_with(~str_remove(., 'TP'))

auditoryIF_loglik_easy <- auditoryIF_loglik_easy |>  rename_with(~str_remove(., '_IF'))
auditoryIF_loglik_easy <- auditoryIF_loglik_easy |>  rename_with(~str_remove(., 'IF'))

auditoryAF_loglik_easy <- auditoryAF_loglik_easy |>  rename_with(~str_remove(., '_AF'))
auditoryAF_loglik_easy <- auditoryAF_loglik_easy |>  rename_with(~str_remove(., 'AF'))

auditoryStim_loglik_easy <- auditoryStim_loglik_easy |> rename_with(~str_remove(., 'Stim'))

auditoryTP_loglik_difficult <- auditoryTP_loglik_difficult |>  rename_with(~str_remove(., '_TP'))
auditoryTP_loglik_difficult <- auditoryTP_loglik_difficult |>  rename_with(~str_remove(., 'TP'))

auditoryIF_loglik_difficult <- auditoryIF_loglik_difficult |>  rename_with(~str_remove(., '_IF'))
auditoryIF_loglik_difficult <- auditoryIF_loglik_difficult |>  rename_with(~str_remove(., 'IF'))

auditoryAF_loglik_difficult <- auditoryAF_loglik_difficult |>  rename_with(~str_remove(., '_AF'))
auditoryAF_loglik_difficult <- auditoryAF_loglik_difficult |>  rename_with(~str_remove(., 'AF'))

auditoryStim_loglik_difficult <- auditoryStim_loglik_difficult |> rename_with(~str_remove(., 'Stim'))

# Vestibular
vestibularTP_loglik_easy <- vestibularTP_loglik_easy |>  rename_with(~str_remove(., '_TP'))
vestibularTP_loglik_easy <- vestibularTP_loglik_easy |>  rename_with(~str_remove(., 'TP'))

vestibularIF_loglik_easy <- vestibularIF_loglik_easy |>  rename_with(~str_remove(., '_IF'))
vestibularIF_loglik_easy <- vestibularIF_loglik_easy |>  rename_with(~str_remove(., 'IF'))

vestibularAF_loglik_easy <- vestibularAF_loglik_easy |>  rename_with(~str_remove(., '_AF'))
vestibularAF_loglik_easy <- vestibularAF_loglik_easy |>  rename_with(~str_remove(., 'AF'))

vestibularStim_loglik_easy <- vestibularStim_loglik_easy |> rename_with(~str_remove(., 'Stim'))


vestibularTP_loglik_difficult <- vestibularTP_loglik_difficult |>  rename_with(~str_remove(., '_TP'))
vestibularTP_loglik_difficult <- vestibularTP_loglik_difficult |>  rename_with(~str_remove(., 'TP'))

vestibularIF_loglik_difficult <- vestibularIF_loglik_difficult |>  rename_with(~str_remove(., '_IF'))
vestibularIF_loglik_difficult <- vestibularIF_loglik_difficult |>  rename_with(~str_remove(., 'IF'))

vestibularAF_loglik_difficult <- vestibularAF_loglik_difficult |>  rename_with(~str_remove(., '_AF'))
vestibularAF_loglik_difficult <- vestibularAF_loglik_difficult |>  rename_with(~str_remove(., 'AF'))

vestibularStim_loglik_difficult <- vestibularStim_loglik_difficult |> rename_with(~str_remove(., 'Stim'))


# Visual
visualTP_loglik_easy <- visualTP_loglik_easy |>  rename_with(~str_remove(., '_TP'))
visualTP_loglik_easy <- visualTP_loglik_easy |>  rename_with(~str_remove(., 'TP'))

visualIF_loglik_easy <- visualIF_loglik_easy |>  rename_with(~str_remove(., '_IF'))
visualIF_loglik_easy <- visualIF_loglik_easy |>  rename_with(~str_remove(., 'IF'))

visualAF_loglik_easy <- visualAF_loglik_easy |>  rename_with(~str_remove(., '_AF'))
visualAF_loglik_easy <- visualAF_loglik_easy |>  rename_with(~str_remove(., 'AF'))

visualStim_loglik_easy <- visualStim_loglik_easy |> rename_with(~str_remove(., 'Stim'))

visualTP_loglik_difficult <- visualTP_loglik_difficult |>  rename_with(~str_remove(., '_TP'))
visualTP_loglik_difficult <- visualTP_loglik_difficult |>  rename_with(~str_remove(., 'TP'))

visualIF_loglik_difficult <- visualIF_loglik_difficult |>  rename_with(~str_remove(., '_IF'))
visualIF_loglik_difficult <- visualIF_loglik_difficult |>  rename_with(~str_remove(., 'IF'))

visualAF_loglik_difficult <- visualAF_loglik_difficult |>  rename_with(~str_remove(., '_AF'))
visualAF_loglik_difficult <- visualAF_loglik_difficult |>  rename_with(~str_remove(., 'AF'))

visualStim_loglik_difficult <- visualStim_loglik_difficult |> rename_with(~str_remove(., 'Stim'))



# Add all dataframes to a list 
dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

# Apply full join to all data frames in the list 'dfs'
dfabs_LLhood_vs_Stim <- Reduce(
  function(x, y, ...) full_join(x, y, ...),
  dfs
)

# Convert factors 
dfabs_LLhood_vs_Stim <- dfabs_LLhood_vs_Stim |> 
  mutate(across(c("modality", "learningmodel", "level"), factor))


# Try to scale this:
dfabs_LLhood_vs_Stim <- dfabs_LLhood_vs_Stim |> 
  group_by(level, modality) |> 
  mutate(loglikelihood_mean = mean(loglikelihood)) |> 
  mutate(loglikelihood_centered = (loglikelihood - loglikelihood_mean)) |> 
  ungroup()


# Save data 
save(dfabs_LLhood_vs_Stim, file = "processed_data/Results/df_LLhood_absolute_vs_Stim.Rdata")

