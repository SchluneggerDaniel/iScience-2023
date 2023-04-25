# Function to create a predictor for repetiton preference

# Add a simple preference for repetition (0 == 0.4 | 1 == 0.6)

library(tidyverse)

# Add function to shift down vector n rows and replace with NA
shiftdown <- function(x, n){
  c(rep(NA, n), head(x, -n))
}

CountRepetitionPreference <- function(df){
  
  # Store input sequence in dataframe
  df <- enframe(df$response, name = "trial", value = "AorB")
  
  # Drop NA's:
  df <- df |> 
    drop_na()

  df <- df |> 
    mutate(predRepPrefbeforeShiftdown = ifelse(test = AorB == 0,
                                               yes = 0.4,
                                               no = 0.6))
  
  # Ungroup df (because of group_by outside of the function call)
  df <- ungroup(df)
  
  # Shiftdown for prediction
  df <- df |> 
    mutate(predRepPref = shiftdown(predRepPrefbeforeShiftdown, 1),
           predRepPref = qlogis(predRepPref))
  
  # Select releveant variables
  df <- df |> 
    select(trial, AorB, predRepPref)
  
  # Return df
  return(df)


}
