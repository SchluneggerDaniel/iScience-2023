# Observer Model (Alternation Frequency) - 2022

library(tidyverse)

# Add function to shift down vector n rows and replace with NA
shiftdown <- function(x, n){
  c(rep(NA, n), head(x, -n))
}

# Main function
CountEventAlternation <- function(s, omega = Inf){
  
  # Store input sequence in dataframe
  df <- enframe(s, name = "trial", value = "AorB")
  
  # Drop NA's:
  df <- df |> 
    drop_na()
  
  # Shift down vector for comparison 
  df <- df |> 
    mutate(compareTP = shiftdown(AorB, 1))
  
  # Count the occurence of Repetitions and Alternations
  df <- df |> 
    mutate(RepCount = ifelse(test = (AorB == 0 & compareTP == 0) | 
                                    (AorB == 1 & compareTP == 1), 
                             yes = 1, 
                             no = 0), 
           AltCount = ifelse(test = (AorB == 0 & compareTP == 1) |
                                    (AorB == 1 & compareTP == 0),
                             yes = 1, 
                             no = 0))
  
  # Ungroup df
  df <- ungroup(df)
  
  # Replace NA's with 0 (this is for the first row)
  df[is.na(df)] <- 0

  # Preallocation for the counting variables
  NumberRep <- NULL
  NumberAlt <- NULL
  
  for (k in 1:length(df$AorB)){
    
    # Calculate decay factor / leaky factor
    decay <- NULL
    for (i in 0:k-1){
      decay[i+1] <- exp(1)**(-i/omega)
    }
    
    # Continuously update subs
    subsRep <- df$RepCount[1:k]
    subsAlt <- df$AltCount[1:k]
    
    # Store decay und "turn it around"
    decay <- rev(decay)
    
    subsRepIndex <- subsRep == 1
    subsAltIndex <- subsAlt == 1
    
    subsDecayRep <- decay[subsRepIndex]
    subsDecayAlt <- decay[subsAltIndex]
    
    NumberRep[k] <- sum(subsDecayRep)
    NumberAlt[k] <- sum(subsDecayAlt)
    
  }
  
  # Compute mean posterior probability
  df <- df |>  
    mutate(predRep = (NumberRep + 1) / (NumberRep + NumberAlt + 2),
           predAlt = (NumberAlt + 1) / (NumberRep + NumberAlt + 2))
  
  # Apply the actual prediction for response "a" (0)  or "b" (1)
  df <- df |>
    mutate(predBeforeShiftdownA = ifelse(AorB == 0, predRep, predAlt),
           predBeforeShiftdownB = ifelse(AorB == 1, predRep, predAlt))
  
  # shiftdown for prediction (this predicts Repetition or Alternation)
  # df <- df |>  
  #   mutate(predRep = shiftdown(predRep, 1),
  #          predAlt = shiftdown(predAlt, 1))
  
  # Use the AF model to predict the response (not repetition or alternation)
  df <- df |>  
    mutate(predA_AF = shiftdown(predBeforeShiftdownA, 1),
           predB_AF = shiftdown(predBeforeShiftdownB, 1))
  
  # Select relevant variables
  df <- df |> select(trial, AorB, predA_AF, predB_AF)
  
  return(df)
  
}





