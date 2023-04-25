# Data preprocessing for GLM Results

# IMPORTANT: It is absolutely necessary to start a new R Session before
# running this script!

# .rs.restartR()

# Purpose: For plotting!

library(fs)
library(tidyverse)

### === LOAD DATA === ###

file_list <- dir_ls("processed_data/GLM")

# Load all data at once
lapply(file_list, load, .GlobalEnv)

# Auditory
auditoryTP$modality <- "auditory"
auditoryTP$learningmodel <- "TP"

auditoryIF$modality <- "auditory"
auditoryIF$learningmodel <- "IF"

auditoryAF$modality <- "auditory"
auditoryAF$learningmodel <- "AF"

auditoryRepPref$modality <- "auditory"
auditoryRepPref$learningmodel <- "RepPref"

# Vestibular
vestibularTP$modality <- "vestibular"
vestibularTP$learningmodel <- "TP"

vestibularIF$modality <- "vestibular"
vestibularIF$learningmodel <- "IF"

vestibularAF$modality <- "vestibular"
vestibularAF$learningmodel <- "AF"

vestibularRepPref$modality <- "vestibular"
vestibularRepPref$learningmodel <- "RepPref"

# Visual
visualTP$modality <- "visual"
visualTP$learningmodel <- "TP"

visualIF$modality <- "visual"
visualIF$learningmodel <- "IF"

visualAF$modality <- "visual"
visualAF$learningmodel <- "AF"

visualRepPref$modality <- "visual"
visualRepPref$learningmodel <- "RepPref"

# Prepare for full_join()

# Auditory
auditoryTP <- auditoryTP |>  rename_with(~str_remove(., '_TP'))
auditoryTP <- auditoryTP |>  rename_with(~str_remove(., 'TP'))

auditoryIF <- auditoryIF |>  rename_with(~str_remove(., '_IF'))
auditoryIF <- auditoryIF |>  rename_with(~str_remove(., 'IF'))

auditoryAF <- auditoryAF |>  rename_with(~str_remove(., '_AF'))
auditoryAF <- auditoryAF |>  rename_with(~str_remove(., 'AF'))

auditoryRepPref <- auditoryRepPref |>  rename_with(~str_remove(., '_RepPref'))
auditoryRepPref <- auditoryRepPref |>  rename_with(~str_remove(., 'RepPref'))

# Vestibular
vestibularTP <- vestibularTP |>  rename_with(~str_remove(., '_TP'))
vestibularTP <- vestibularTP |>  rename_with(~str_remove(., 'TP'))

vestibularIF <- vestibularIF |>  rename_with(~str_remove(., '_IF'))
vestibularIF <- vestibularIF |>  rename_with(~str_remove(., 'IF'))

vestibularAF <- vestibularAF |>  rename_with(~str_remove(., '_AF'))
vestibularAF <- vestibularAF |>  rename_with(~str_remove(., 'AF'))

vestibularRepPref <- vestibularRepPref |>  rename_with(~str_remove(., '_RepPref'))
vestibularRepPref <- vestibularRepPref |>  rename_with(~str_remove(., 'RepPref'))

# Visual
visualTP <- visualTP |>  rename_with(~str_remove(., '_TP'))
visualTP <- visualTP |>  rename_with(~str_remove(., 'TP'))

visualIF <- visualIF |>  rename_with(~str_remove(., '_IF'))
visualIF <- visualIF |>  rename_with(~str_remove(., 'IF'))

visualAF <- visualAF |>  rename_with(~str_remove(., '_AF'))
visualAF <- visualAF |>  rename_with(~str_remove(., 'AF'))

visualRepPref <- visualRepPref |>  rename_with(~str_remove(., '_RepPref'))
visualRepPref <- visualRepPref |>  rename_with(~str_remove(., 'RepPref'))


# Add all dataframes to a list 
dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

# Apply full join to all data frames in the list 'dfs'
dfabs_GLM <- Reduce(
  function(x, y, ...) full_join(x, y, ...),
  dfs
)

# Convert factors 
dfabs_GLM <- dfabs_GLM |> 
  mutate(across(c("modality", "learningmodel"), factor))

# Save data
save(dfabs_GLM, file = "processed_data/Results/df_GLM_absolute.Rdata")

