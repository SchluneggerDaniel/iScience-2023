# Function for quantifying the conditional probabilities

# Input:
#   - s: Binary sequence
#   - intensity: Stimulus intensities (optional, needs to be a factor) 

# Output: Function returns a dataframe containing the conditional probabilites

library(tidyverse)

# Function to shift down vector n rows and replace with NA
shiftdown <- function(x, n){
  c(rep(NA, n), head(x, -n))
}

# Main function
CountConditionalProbabilityExtended <- function(s){

  
  # Create dataframe
  df <- tibble(s, 
               shiftdown(s, 1), 
               shiftdown(s, 2),
               .name_repair = ~ c("Original", "Shift1", "Shift2"))
  
  # Preceding responses were: (logical indexing)
  df <- df |> 
    mutate(givenAA = ifelse(test = Shift1 == 0 & Shift2 == 0, yes = 1, no = 0),
           givenAB = ifelse(test = Shift1 == 1 & Shift2 == 0, yes = 1, no = 0),
           givenBA = ifelse(test = Shift1 == 0 & Shift2 == 1, yes = 1, no = 0),
           givenBB = ifelse(test = Shift1 == 1 & Shift2 == 1, yes = 1, no = 0))
  
  return(df)
      

}