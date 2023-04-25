# Data Pre-Processing Psychometric Analysis Visual
library(tidyverse)

source("functions/CountConditionalProbabilitiesExtended.R")

load("processed_data/allValues/allValuesVisual.Rda")

df_visual <- allValuesVisual

# Function: Extract Psychometric Shift
extractPsychometricShift <- function(x){
  
  y <- x |> select(starts_with("yseq")) |> names()
  
  x[y] <- round(x[y],2)
  
  x[x[y] == 0.5, "xseq"] |> mean()
  
}



# Preallocation

visual_shift_givenAA_neut <- NULL
visual_shift_givenAB_neut <- NULL
visual_shift_givenBA_neut <- NULL
visual_shift_givenBB_neut <- NULL

visual_shift_givenAA_rep <- NULL
visual_shift_givenAB_rep <- NULL
visual_shift_givenBA_rep <- NULL
visual_shift_givenBB_rep <- NULL

visual_shift_givenAA_alt <- NULL
visual_shift_givenAB_alt <- NULL
visual_shift_givenBA_alt <- NULL
visual_shift_givenBB_alt <- NULL


### Visual ###

for (i in 1:length(levels(df_visual$subject))){
  
  df_visual <- allValuesVisual |> 
    select(-(starts_with("tp"))) |> 
    filter(subject %in% levels(df_visual$subject)[i])
  
  # Visual
  df_visual <- df_visual |> 
    mutate(rightresponse = response,
           leftresponse = if_else(response == 0, 1, 0)) |> 
    tibble()
  
  
  
  df_visual_neut <- df_visual |> 
    filter(condition %in% "Neutral")
  
  df_visual_rep <- df_visual |> 
    filter(condition %in% "Repetitive")
  
  df_visual_alt <- df_visual |> 
    filter(condition %in% "Alternating")
  
  
  
  # Condition on the previous response
  df_function_neut <- CountConditionalProbabilityExtended(df_visual_neut$response)
  df_function_rep <- CountConditionalProbabilityExtended(df_visual_rep$response)
  df_function_alt <- CountConditionalProbabilityExtended(df_visual_alt$response)
  
  df_neut <- bind_cols(df_visual_neut, df_function_neut)
  df_rep <- bind_cols(df_visual_rep, df_function_rep)
  df_alt <- bind_cols(df_visual_alt, df_function_alt)
  
  df_neut <- df_neut |> 
    mutate(across(c(starts_with("given")), factor))
  
  df_rep <- df_rep |> 
    mutate(across(c(starts_with("given")), factor))
  
  df_alt <- df_alt |> 
    mutate(across(c(starts_with("given")), factor))
  
  
  filtergivenAA <- function(x){x <- x |> filter(x$givenAA %in% "1")}
  filtergivenAB <- function(x){x <- x |> filter(x$givenAB %in% "1")}
  filtergivenBA <- function(x){x <- x |> filter(x$givenBA %in% "1")}
  filtergivenBB <- function(x){x <- x |> filter(x$givenBB %in% "1")}
  
  # Fully stochastic
  df_givenAA_neut <- filtergivenAA(df_neut)
  df_givenAB_neut <- filtergivenAB(df_neut)
  df_givenBA_neut <- filtergivenBA(df_neut)
  df_givenBB_neut <- filtergivenBB(df_neut)
  
  # Repetition-bised
  df_givenAA_rep <- filtergivenAA(df_rep)
  df_givenAB_rep <- filtergivenAB(df_rep)
  df_givenBA_rep <- filtergivenBA(df_rep)
  df_givenBB_rep <- filtergivenBB(df_rep)
  
  # Alternation-biased
  df_givenAA_alt <- filtergivenAA(df_alt)
  df_givenAB_alt <- filtergivenAB(df_alt)
  df_givenBA_alt <- filtergivenBA(df_alt)
  df_givenBB_alt <- filtergivenBB(df_alt)
  
  
  # Neutral
  df1_all_neut <- df_neut |> group_by(intensity) |> 
    summarise(nRight = sum(rightresponse),
              nLeft = sum(leftresponse))
  
  df1_givenAA_neut <- df_givenAA_neut |> group_by(intensity) |> 
    summarise(nRight = sum(rightresponse),
              nLeft = sum(leftresponse))
  
  df1_givenAB_neut <- df_givenAB_neut |> group_by(intensity) |> 
    summarise(nRight = sum(rightresponse),
              nLeft = sum(leftresponse))
  
  df1_givenBA_neut <- df_givenBA_neut |> group_by(intensity) |> 
    summarise(nRight = sum(rightresponse),
              nLeft = sum(leftresponse))
  
  df1_givenBB_neut <- df_givenBB_neut |> group_by(intensity) |> 
    summarise(nRight = sum(rightresponse),
              nLeft = sum(leftresponse))
  
  
  # Repetition-biased
  df1_all_rep <- df_rep |> group_by(intensity) |> 
    summarise(nRight = sum(rightresponse),
              nLeft = sum(leftresponse))
  
  df1_givenAA_rep <- df_givenAA_rep |> group_by(intensity) |> 
    summarise(nRight = sum(rightresponse),
              nLeft = sum(leftresponse))
  
  df1_givenAB_rep <- df_givenAB_rep |> group_by(intensity) |> 
    summarise(nRight = sum(rightresponse),
              nLeft = sum(leftresponse))
  
  df1_givenBA_rep <- df_givenBA_rep |> group_by(intensity) |> 
    summarise(nRight = sum(rightresponse),
              nLeft = sum(leftresponse))
  
  df1_givenBB_rep <- df_givenBB_rep |> group_by(intensity) |> 
    summarise(nRight = sum(rightresponse),
              nLeft = sum(leftresponse))
  
  
  # Alternation-biased
  df1_all_alt <- df_alt |> group_by(intensity) |> 
    summarise(nRight = sum(rightresponse),
              nLeft = sum(leftresponse))
  
  df1_givenAA_alt <- df_givenAA_alt |> group_by(intensity) |> 
    summarise(nRight = sum(rightresponse),
              nLeft = sum(leftresponse))
  
  df1_givenAB_alt <- df_givenAB_alt |> group_by(intensity) |> 
    summarise(nRight = sum(rightresponse),
              nLeft = sum(leftresponse))
  
  df1_givenBA_alt <- df_givenBA_alt |> group_by(intensity) |> 
    summarise(nRight = sum(rightresponse),
              nLeft = sum(leftresponse))
  
  df1_givenBB_alt <- df_givenBB_alt |> group_by(intensity) |> 
    summarise(nRight = sum(rightresponse),
              nLeft = sum(leftresponse))
  
  
  
  # Fully stochastic: Add proportion
  df1_all_neut$pRight <- df1_all_neut$nRight / (df1_all_neut$nRight + df1_all_neut$nLeft)
  df1_givenAA_neut$pRight <- df1_givenAA_neut$nRight / (df1_givenAA_neut$nRight + df1_givenAA_neut$nLeft)
  df1_givenAB_neut$pRight <- df1_givenAB_neut$nRight / (df1_givenAB_neut$nRight + df1_givenAB_neut$nLeft)
  df1_givenBA_neut$pRight <- df1_givenBA_neut$nRight / (df1_givenBA_neut$nRight + df1_givenBA_neut$nLeft)
  df1_givenBB_neut$pRight <- df1_givenBB_neut$nRight / (df1_givenBB_neut$nRight + df1_givenBB_neut$nLeft)
  
  # Repetition-baised: Add proportion
  df1_all_rep$pRight <- df1_all_rep$nRight / (df1_all_rep$nRight + df1_all_rep$nLeft)
  df1_givenAA_rep$pRight <- df1_givenAA_rep$nRight / (df1_givenAA_rep$nRight + df1_givenAA_rep$nLeft)
  df1_givenAB_rep$pRight <- df1_givenAB_rep$nRight / (df1_givenAB_rep$nRight + df1_givenAB_rep$nLeft)
  df1_givenBA_rep$pRight <- df1_givenBA_rep$nRight / (df1_givenBA_rep$nRight + df1_givenBA_rep$nLeft)
  df1_givenBB_rep$pRight <- df1_givenBB_rep$nRight / (df1_givenBB_rep$nRight + df1_givenBB_rep$nLeft)
  
  # Alternation-baised: Add proportion
  df1_all_alt$pRight <- df1_all_alt$nRight / (df1_all_alt$nRight + df1_all_alt$nLeft)
  df1_givenAA_alt$pRight <- df1_givenAA_alt$nRight / (df1_givenAA_alt$nRight + df1_givenAA_alt$nLeft)
  df1_givenAB_alt$pRight <- df1_givenAB_alt$nRight / (df1_givenAB_alt$nRight + df1_givenAB_alt$nLeft)
  df1_givenBA_alt$pRight <- df1_givenBA_alt$nRight / (df1_givenBA_alt$nRight + df1_givenBA_alt$nLeft)
  df1_givenBB_alt$pRight <- df1_givenBB_alt$nRight / (df1_givenBB_alt$nRight + df1_givenBB_alt$nLeft)
  
  
  # Neutral: Fit glm
  model_all_neut <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_all_neut, family = binomial())
  model_givenAA_neut <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAA_neut, family = binomial())
  model_givenAB_neut <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAB_neut, family = binomial())
  model_givenBA_neut <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBA_neut, family = binomial())
  model_givenBB_neut <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBB_neut, family = binomial())
  
  # Repetitive: Fit glm
  model_all_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_all_rep, family = binomial())
  model_givenAA_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAA_rep, family = binomial())
  model_givenAB_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAB_rep, family = binomial())
  model_givenBA_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBA_rep, family = binomial())
  model_givenBB_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBB_rep, family = binomial())
  
  # Alternating: Fit glm
  model_all_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_all_alt, family = binomial())
  model_givenAA_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAA_alt, family = binomial())
  model_givenAB_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAB_alt, family = binomial())
  model_givenBA_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBA_alt, family = binomial())
  model_givenBB_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBB_alt, family = binomial())
  
  # Create data points
  xseq <- seq(-0.6, 0.6, len = 1000)
  
  # Neutral: Predict
  yseq_all_neut <- predict(model_all_neut, data.frame(intensity = xseq), type = "response")
  yseq_givenAA_neut <- predict(model_givenAA_neut, data.frame(intensity = xseq), type = "response")
  yseq_givenAB_neut <- predict(model_givenAB_neut, data.frame(intensity = xseq), type = "response")
  yseq_givenBA_neut <- predict(model_givenBA_neut, data.frame(intensity = xseq), type = "response")
  yseq_givenBB_neut <- predict(model_givenBB_neut, data.frame(intensity = xseq), type = "response")
  
  # Repetitive: Predict
  yseq_all_rep <- predict(model_all_rep, data.frame(intensity = xseq), type = "response")
  yseq_givenAA_rep <- predict(model_givenAA_rep, data.frame(intensity = xseq), type = "response")
  yseq_givenAB_rep <- predict(model_givenAB_rep, data.frame(intensity = xseq), type = "response")
  yseq_givenBA_rep <- predict(model_givenBA_rep, data.frame(intensity = xseq), type = "response")
  yseq_givenBB_rep <- predict(model_givenBB_rep, data.frame(intensity = xseq), type = "response")
  
  # Alternating: Predict
  yseq_all_alt <- predict(model_all_alt, data.frame(intensity = xseq), type = "response")
  yseq_givenAA_alt <- predict(model_givenAA_alt, data.frame(intensity = xseq), type = "response")
  yseq_givenAB_alt <- predict(model_givenAB_alt, data.frame(intensity = xseq), type = "response")
  yseq_givenBA_alt <- predict(model_givenBA_alt, data.frame(intensity = xseq), type = "response")
  yseq_givenBB_alt <- predict(model_givenBB_alt, data.frame(intensity = xseq), type = "response")
  
  # Neutral: Curve
  curve_all_neut <- data.frame(xseq, yseq_all_neut)
  curve_givenAA_neut <- data.frame(xseq, yseq_givenAA_neut)
  curve_givenAB_neut <- data.frame(xseq, yseq_givenAB_neut)
  curve_givenBA_neut <- data.frame(xseq, yseq_givenBA_neut)
  curve_givenBB_neut <- data.frame(xseq, yseq_givenBB_neut)
  
  # Repetitive: Curve
  curve_all_rep <- data.frame(xseq, yseq_all_rep)
  curve_givenAA_rep <- data.frame(xseq, yseq_givenAA_rep)
  curve_givenAB_rep <- data.frame(xseq, yseq_givenAB_rep)
  curve_givenBA_rep <- data.frame(xseq, yseq_givenBA_rep)
  curve_givenBB_rep <- data.frame(xseq, yseq_givenBB_rep)
  
  # Alternating: Curve
  curve_all_alt <- data.frame(xseq, yseq_all_alt)
  curve_givenAA_alt <- data.frame(xseq, yseq_givenAA_alt)
  curve_givenAB_alt <- data.frame(xseq, yseq_givenAB_alt)
  curve_givenBA_alt <- data.frame(xseq, yseq_givenBA_alt)
  curve_givenBB_alt <- data.frame(xseq, yseq_givenBB_alt)
  
  
  visual_shift_givenAA_neut[0+i] <- extractPsychometricShift(curve_givenAA_neut)
  visual_shift_givenAB_neut[0+i] <- extractPsychometricShift(curve_givenAB_neut)
  visual_shift_givenBA_neut[0+i] <- extractPsychometricShift(curve_givenBA_neut)
  visual_shift_givenBB_neut[0+i] <- extractPsychometricShift(curve_givenBB_neut)
  
  visual_shift_givenAA_rep[0+i] <- extractPsychometricShift(curve_givenAA_rep)
  visual_shift_givenAB_rep[0+i] <- extractPsychometricShift(curve_givenAB_rep)
  visual_shift_givenBA_rep[0+i] <- extractPsychometricShift(curve_givenBA_rep)
  visual_shift_givenBB_rep[0+i] <- extractPsychometricShift(curve_givenBB_rep)
  
  visual_shift_givenAA_alt[0+i] <- extractPsychometricShift(curve_givenAA_alt)
  visual_shift_givenAB_alt[0+i] <- extractPsychometricShift(curve_givenAB_alt)
  visual_shift_givenBA_alt[0+i] <- extractPsychometricShift(curve_givenBA_alt)
  visual_shift_givenBB_alt[0+i] <- extractPsychometricShift(curve_givenBB_alt)
  
  
  
}


subject <- levels(df_visual$subject)

visualPsychometricData <- tibble(subject,
                                 visual_shift_givenAA_neut,
                                 visual_shift_givenAB_neut,
                                 visual_shift_givenBA_neut,
                                 visual_shift_givenBB_neut,
                                 visual_shift_givenAA_rep,
                                 visual_shift_givenAB_rep,
                                 visual_shift_givenBA_rep,
                                 visual_shift_givenBB_rep,
                                 visual_shift_givenAA_alt,
                                 visual_shift_givenAB_alt,
                                 visual_shift_givenBA_alt,
                                 visual_shift_givenBB_alt)



save(visualPsychometricData, file = "processed_data/Psychometric/visualPsychometricData.Rda")

