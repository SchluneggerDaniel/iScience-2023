# Observer Model (Item Frequency) - 2022

library(tidyverse)

# Add function to shift down vector n rows and replace with NA
shiftdown <- function(x, n){
  c(rep(NA, n), head(x, -n))
}

# Main function
CountEventBernoulli <- function(s, omega = Inf){
  
  # Store input sequence in dataframe
  df <- enframe(s, name = "trial", value = "AorB")
  
  # Drop NA's:
  df <- df |> 
    drop_na()
  
  # Preallocation for the counting variables
  NumberA <- NULL
  NumberB <- NULL
  
  for (k in 1:length(df$AorB)){
    
    # Calculate decay factor / leaky factor
    decay <- NULL
    for (i in 0:k-1){
      decay[i+1] <- exp(1)**(-i/omega)
    }
    
    # Continuously update subs with true generative values
    subs <- df$AorB[1:k]
    
    # Store decay und "turn it around"
    decay <- rev(decay)
    
    subsA <- subs == 0
    subsB <- subs == 1
    
    subsDecayA <- decay[subsA]
    subsDecayB <- decay[subsB]
    
    NumberA[k] <- sum(subsDecayA)
    NumberB[k] <- sum(subsDecayB)
    
  }
  
  # Compute transition probabilities 
  df <- df |>  
    mutate(pA = (NumberA) / (NumberA + NumberB),
           pB = (NumberB) / (NumberA + NumberB))
  
  # Compute mean posterior probability
  df <- df |>  
    mutate(predA = (NumberA + 1) / (NumberA + NumberB + 2),
           predB = (NumberB + 1) / (NumberA + NumberB + 2))
  
  # shiftdown for prediction
  df <- df |>  
    mutate(predA_IF = shiftdown(predA, 1),
           predB_IF = shiftdown(predB, 1))
  
  
  # Select relevant variables (trial, AorB, predA, predB, NumberA, NumberB)
  df <- df |> select(trial, AorB, predA_IF, predB_IF)
  
  return(df)
  
}