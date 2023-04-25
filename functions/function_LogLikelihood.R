# Function: Log-Likelihood analysis for the three learning models

# Functions for TP, IF, AF, and Stimulus-Only 

# Important notes: 

# (1) Because for all the other analyses, data is transformed
#     using qlogis(). For this analysis, we transform it back, using plogis()
# (2) for loglike_Stim: Data is transformed with an ifelse statement because
#     in the GLM Analyses, effect coding (i.e. -0.5 and 0.5 was applied)

# Transition Probability
logLike_TP <- function(df){
  
  dbinom(x = df$response, size = 1, prob = plogis(df$TP), log = TRUE) |> 
    sum()
  
}

# Item Frequency
logLike_IF <- function(df){
  
  dbinom(x = df$response, size = 1, prob = plogis(df$IF), log = TRUE) |> 
    sum()
  
}

# Alternation Frequency
logLike_AF <- function(df){
  
  dbinom(x = df$response, size = 1, prob = plogis(df$AF), log = TRUE) |> 
    sum()
  
}

# Stimulus-Only
logLike_Stim <- function(df){
  
  stim = ifelse(test = df$stimulus == -0.5, 
                       yes = df$stimulus + 0.501,
                       no = df$stimulus + 0.499)
  
  dbinom(x = df$response, size = 1, prob = stim, log = TRUE) |> 
    sum()
  
}
















