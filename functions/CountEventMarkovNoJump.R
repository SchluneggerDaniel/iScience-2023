# Observer Model (Transition Probabilities) - 2022

library(tidyverse)

# Add function to shift down vector n rows and replace with NA
shiftdown <- function(x, n){
  c(rep(NA, n), head(x, -n))
}

# Main function
CountEventMarkov <- function(s, omega = Inf){
  
  # Store input sequence in dataframe
  df <- enframe(s, name = "trial", value = "AorB")
  
  # Drop NA's:
  df <- df |> 
    drop_na()
  
  # Shift down vector for comparison 
  df <- df |> 
    mutate(compareTP = shiftdown(AorB, 1))
  
  # Count the occurence of every transition type
  df <- df |> 
    mutate(AgivenAcount = ifelse(test = (AorB == 0 & compareTP == 0), yes = 1, no = 0),
           AgivenBcount = ifelse(test = (AorB == 0 & compareTP == 1), yes = 1, no = 0),
           BgivenAcount = ifelse(test = (AorB == 1 & compareTP == 0), yes = 1, no = 0),
           BgivenBcount = ifelse(test = (AorB == 1 & compareTP == 1), yes = 1, no = 0))
  
  # AgivenA
  df <- df |> 
    group_by(AgivenAcount) |> 
    mutate(AgivenA = sequence(n())) |> 
    mutate(AgivenA = ifelse(test = (AgivenAcount == 1), yes = sequence(n()), no = 0))
  
  # AgivenB
  df <- df |> 
    group_by(AgivenBcount) |> 
    mutate(AgivenB = sequence(n())) |> 
    mutate(AgivenB = ifelse(test = (AgivenBcount == 1), yes = sequence(n()), no = 0))
  
  # BgivenA
  df <- df |> 
    group_by(BgivenAcount) |> 
    mutate(BgivenA = sequence(n())) |> 
    mutate(BgivenA = ifelse(test = (BgivenAcount == 1), yes = sequence(n()), no = 0))
  
  # BgivenB
  df <- df |> 
    group_by(BgivenBcount) |> 
    mutate(BgivenB = sequence(n())) |> 
    mutate(BgivenB = ifelse(test = (BgivenBcount == 1), yes = sequence(n()), no = 0))
  
  # Ungroup df
  df <- ungroup(df)
  
  # Replace NA's with 0's
  df[is.na(df)] <- 0
  
  # AgivenA
  df <- df |> 
    mutate(AgivenA = replace(AgivenA, cumsum(AgivenA != 0) > 0 & AgivenA == 0, NA)) |> 
    fill(AgivenA)
  
  # BgivenA
  df <- df |> 
    mutate(BgivenA = replace(BgivenA, cumsum(BgivenA != 0) > 0 & BgivenA == 0, NA)) |> 
    fill(BgivenA)
  
  # AgivenB
  df <- df |> 
    mutate(AgivenB = replace(AgivenB, cumsum(AgivenB != 0) > 0 & AgivenB == 0, NA)) |> 
    fill(AgivenB)
  
  # BgivenB
  df <- df |> 
    mutate(BgivenB = replace(BgivenB, cumsum(BgivenB != 0) > 0 & BgivenB == 0, NA)) |> 
    fill(BgivenB)
  
  # Preallocation for the counting variables
  NAgA <- NULL
  NBgA <- NULL
  NAgB <- NULL
  NBgB <- NULL
  
  for (k in 1:length(df$AorB)){
    
    # Calculate decay factor / leaky factor
    decay <- NULL
    for (i in 0:k-1){
      decay[i+1] <- exp(1)**(-i/omega)
    }
    
    # Make subs4turn
    subs4trn <- diff(df$AgivenA[1:k] + df$BgivenA[1:k])
    
    # Make trn
    trn <- diff(df$AgivenB[1:k] + df$BgivenA[1:k])
    trnIndex <- df$AgivenBcount[2:k]
    trn <- ifelse(trnIndex == 1, -1, trn)
    
    # Store decay und "turn it around" using rev
    decay <- decay[2:k]
    decay <- rev(decay)
    
    # Indexing
    subs4trnIndexGA <- subs4trn == 1
    subs4trnIndexGB <- subs4trn == 0
    
    trnIndexNAgA <- trn == 0 
    trnIndexNBgA <- trn == 1
    trnIndexNAgB <- trn == -1
    trnIndexNBgB <- trn == 0
    
    leftsideIndexNAgA <- trnIndexNAgA[subs4trnIndexGA]
    leftsideIndexNBgA <- trnIndexNBgA[subs4trnIndexGA]
    leftsideIndexNAgB <- trnIndexNAgB[subs4trnIndexGB]
    leftsideIndexNBgB <- trnIndexNBgB[subs4trnIndexGB]
    
    trnDecayGA <- decay[subs4trnIndexGA]
    trnDecayGB <- decay[subs4trnIndexGB]
    
    # Counting (sum up the weights)
    NAgA[k] <- sum(trnDecayGA[leftsideIndexNAgA])
    NBgA[k] <- sum(trnDecayGA[leftsideIndexNBgA])
    NAgB[k] <- sum(trnDecayGB[leftsideIndexNAgB])
    NBgB[k] <- sum(trnDecayGB[leftsideIndexNBgB])
    
  }
  
  # Compute transition probabilities (Maximum a posteriori transition probabilities)
  df <- df |> 
    mutate(pAgivenA = NAgA / (NAgA + NBgA),
           pBgivenA = NBgA / (NBgA + NAgA), # == 1 - pAgivenA
           pAgivenB = NAgB / (NAgB + NBgB),
           pBgivenB = NBgB / (NBgB + NAgB)) # == 1 - pAgivenB
  
  # Compute predictive likelihood of what simulus will follow next
  # This corresponds to the mean of the distribution
  df <- df |>
    mutate(predAgivenA = (NAgA + 1) / (NAgA + NBgA + 2),
           predBgivenA = (NBgA + 1) / (NBgA + NAgA + 2),
           predAgivenB = (NAgB + 1) / (NAgB + NBgB + 2),
           predBgivenB = (NBgB + 1) / (NBgB + NAgB + 2))
  
  # Extract the actual predictive likelihood for both 
  df <- df |>
    mutate(tpBeforeShiftdownA = ifelse(AgivenAcount == 1 | BgivenAcount == 1, predAgivenA, predAgivenB),
           tpBeforeShiftdownB = ifelse(AgivenBcount == 1 | BgivenBcount == 1, predBgivenB, predBgivenA))

  # Shiftdown for prediction
  df <- df |>
    mutate(predA_TP = shiftdown(tpBeforeShiftdownA, 1),
           predB_TP = shiftdown(tpBeforeShiftdownB, 1))

  # Return relevant variables
  df <- df |> select(trial, AorB, predA_TP, predB_TP)
  
  return(df)  
  
}

















