# Analyze Repetition Preference

# For comparison with analysis_GLM.R and analysis_GLM_LearningModels.R, include
# the interaction

library(tidyverse)
library(DescTools)

source("functions/CountRepetitionPreference.R")

# Auditory | Vestibular | Visual
df_auditory <- read.csv(file = "raw_data/auditoryData.csv")
df_vestibular <- read.csv(file = "raw_data/vestibularData.csv")
df_visual <- read.csv(file = "raw_data/visualData.csv")

# Prepare data - auditory
df_auditory <- df_auditory |> 
  mutate(across(c("subject", "sex", "condition"), factor))

auditoryRepPref <- df_auditory|> 
  group_by(subject, condition) |> 
  nest() |> 
  mutate(modelRepPref = map(data, CountRepetitionPreference)) |> 
  select(-(data)) |> 
  unnest(modelRepPref) |> 
  drop_na()
  
# Prepare data - vestibular
df_vestibular <- df_vestibular |> 
  mutate(across(c("subject", "axis", "condition", "group"), factor))


vestibularRepPref <- df_vestibular|> 
  group_by(subject, condition, axis) |> 
  nest() |> 
  mutate(modelRepPref = map(data, CountRepetitionPreference)) |> 
  select(-(data)) |> 
  unnest(modelRepPref) |> 
  drop_na()

# Prepare data - visual
df_visual <- df_visual |> 
  mutate(across(c("subject", "block", "condition", "session"), factor))

visualRepPref <- df_visual|> 
  group_by(subject, block, condition, session) |> 
  nest() |> 
  mutate(modelRepPref = map(data, CountRepetitionPreference)) |> 
  select(-(data)) |> 
  unnest(modelRepPref) |> 
  drop_na()


# Load processed data
load("processed_data/allValues/allValuesAuditory.Rda")
load("processed_data/allValues/allValuesVestibular.Rda")
load("processed_data/allValues/allValuesVisual.Rda")


# Create Function
function_modelPredRep <- function(df) {
  glm(response ~ stimulus + intensity_ord * predRepPref,
      family = binomial(probit),
      data = df)
}


auditory <- allValuesAuditory |> 
  select(-(starts_with(c("pred")))) |> 
  mutate(stimulus = if_else(stimulus == 0, -0.5, 0.5)) |> 
  drop_na()

vestibular <- allValuesVestibular |> 
  select(-(starts_with(c("pred")))) |> 
  mutate(stimulus = if_else(stimulus == 0, -0.5, 0.5)) |> 
  drop_na()

visual <- allValuesVisual |> 
  select(-(starts_with(c("pred")))) |> 
  mutate(stimulus = if_else(stimulus == 0, -0.5, 0.5)) |> 
  drop_na()

# Add to original data frame
auditory$predRepPref <- auditoryRepPref$predRepPref
vestibular$predRepPref <- vestibularRepPref$predRepPref
visual$predRepPref <- visualRepPref$predRepPref

auditoryRepPref <- auditory |> 
  group_by(subject) |> 
  nest() |> 
  mutate(modelRepPref = map(data, function_modelPredRep)) |> 
  mutate(AIC_RepPref = map(modelRepPref, AIC),
         BIC_RepPref = map(modelRepPref, BIC),
         Nagelkerke_RepPref = map(modelRepPref, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_RepPref = map(modelRepPref, function(x) PseudoR2(x, which = "McFaddenAdj"))) |> 
  unnest(cols = c(AIC_RepPref, BIC_RepPref, Nagelkerke_RepPref, McFaddenAdj_RepPref))

vestibularRepPref <- vestibular |> 
  group_by(subject) |> 
  nest() |> 
  mutate(modelRepPref = map(data, function_modelPredRep)) |> 
  mutate(AIC_RepPref = map(modelRepPref, AIC),
         BIC_RepPref = map(modelRepPref, BIC),
         Nagelkerke_RepPref = map(modelRepPref, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_RepPref = map(modelRepPref, function(x) PseudoR2(x, which = "McFaddenAdj"))) |> 
  unnest(cols = c(AIC_RepPref, BIC_RepPref, Nagelkerke_RepPref, McFaddenAdj_RepPref))

visualRepPref <- visual |> 
  group_by(subject) |> 
  nest() |> 
  mutate(modelRepPref = map(data, function_modelPredRep)) |> 
  mutate(AIC_RepPref = map(modelRepPref, AIC),
         BIC_RepPref = map(modelRepPref, BIC),
         Nagelkerke_RepPref = map(modelRepPref, function(x) PseudoR2(x, which = "Nagelkerke")),
         McFaddenAdj_RepPref = map(modelRepPref, function(x) PseudoR2(x, which = "McFaddenAdj"))) |> 
  unnest(cols = c(AIC_RepPref, BIC_RepPref, Nagelkerke_RepPref, McFaddenAdj_RepPref))


# Save data
save(auditoryRepPref, file = "processed_data/GLM/auditoryRepPref.Rda")
save(vestibularRepPref, file = "processed_data/GLM/vestibularRepPref.Rda")
save(visualRepPref, file = "processed_data/GLM/visualRepPref.Rda")

