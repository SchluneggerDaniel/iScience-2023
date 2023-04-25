# # Data pre-processing for significance tests for shifts in psychometric curves

# For each subject and condition:
# - obtain the predicted value for probability 0.5
# - conditioned on the previous choices

library(tidyverse)

source("functions/CountConditionalProbabilitiesExtended.R")

load("processed_data/allValues/allValuesAuditory.Rda")

df_auditory <- allValuesAuditory

### Add functions ###

# Function: Extract Psychometric Shift
extractPsychometricShift <- function(x){
  
  y <- x |> select(starts_with("yseq")) |> names()
  
  x[y] <- round(x[y],2)
  
  x[x[y] == 0.5, "xseq"] |> mean()
  
}


# Preallocation
auditory_shift_givenAA_fs <- NULL
auditory_shift_givenAB_fs <- NULL
auditory_shift_givenBA_fs <- NULL
auditory_shift_givenBB_fs <- NULL

auditory_shift_givenAA_freq <- NULL
auditory_shift_givenAB_freq <- NULL
auditory_shift_givenBA_freq <- NULL
auditory_shift_givenBB_freq <- NULL

auditory_shift_givenAA_rep <- NULL
auditory_shift_givenAB_rep <- NULL
auditory_shift_givenBA_rep <- NULL
auditory_shift_givenBB_rep <- NULL

auditory_shift_givenAA_alt <- NULL
auditory_shift_givenAB_alt <- NULL
auditory_shift_givenBA_alt <- NULL
auditory_shift_givenBB_alt <- NULL

### Auditory
for (i in 1:length(levels(allValuesAuditory$subject))){

    df_auditory <- allValuesAuditory |> 
      # select(-(starts_with("pred"))) |> 
      filter(subject %in% levels(allValuesAuditory$subject)[i])
    
    
    df_auditory <- df_auditory |> 
      mutate(rightresponse = response,
             leftresponse = if_else(response == 0, 1, 0)) |> 
      tibble()
    
    df_auditory_fs <- df_auditory |> 
      filter(condition %in% "Fully stochastic")
    
    df_auditory_freq <- df_auditory |> 
      filter(condition %in% "Frequency-biased")
    
    df_auditory_rep <- df_auditory |> 
      filter(condition %in% "Repetition-biased")
    
    df_auditory_alt <- df_auditory |> 
      filter(condition %in% "Alternation-biased")
    
    # Condition on the previous response
    df_function_fs <- CountConditionalProbabilityExtended(df_auditory_fs$response)
    df_function_freq <- CountConditionalProbabilityExtended(df_auditory_freq$response)
    df_function_rep <- CountConditionalProbabilityExtended(df_auditory_rep$response)
    df_function_alt <- CountConditionalProbabilityExtended(df_auditory_alt$response)
    
    df_fs <- bind_cols(df_auditory_fs, df_function_fs)
    df_freq <- bind_cols(df_auditory_freq, df_function_freq)
    df_rep <- bind_cols(df_auditory_rep, df_function_rep)
    df_alt <- bind_cols(df_auditory_alt, df_function_alt)
    
    df_fs <- df_fs |> 
      mutate(across(c(starts_with("given")), factor))
    
    df_freq <- df_freq |> 
      mutate(across(c(starts_with("given")), factor))
    
    df_rep <- df_rep |> 
      mutate(across(c(starts_with("given")), factor))
    
    df_alt <- df_alt |> 
      mutate(across(c(starts_with("given")), factor))
    
    
    # Subset dataframes (you need 5 dataframes, for 1 plot)
    
    filtergivenAA <- function(x){x <- x |> filter(x$givenAA %in% "1")}
    filtergivenAB <- function(x){x <- x |> filter(x$givenAB %in% "1")}
    filtergivenBA <- function(x){x <- x |> filter(x$givenBA %in% "1")}
    filtergivenBB <- function(x){x <- x |> filter(x$givenBB %in% "1")}
    
    # Fully stochastic
    df_givenAA_fs <- filtergivenAA(df_fs)
    df_givenAB_fs <- filtergivenAB(df_fs)
    df_givenBA_fs <- filtergivenBA(df_fs)
    df_givenBB_fs <- filtergivenBB(df_fs)
    
    # Frequency biased condtion
    df_givenAA_freq <- filtergivenAA(df_freq)
    df_givenAB_freq <- filtergivenAB(df_freq)
    df_givenBA_freq <- filtergivenBA(df_freq)
    df_givenBB_freq <- filtergivenBB(df_freq)
    
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
    
    xseq <- seq(-6, 6, len = 1000)
    
    # Fully stochastic
    df1_all_fs <- df_fs |> group_by(intensity) |> 
      summarise(nRight = sum(rightresponse),
                nLeft = sum(leftresponse))
    
    df1_givenAA_fs <- df_givenAA_fs |> group_by(intensity) |> 
      summarise(nRight = sum(rightresponse),
                nLeft = sum(leftresponse))
    
    df1_givenAB_fs <- df_givenAB_fs |> group_by(intensity) |> 
      summarise(nRight = sum(rightresponse),
                nLeft = sum(leftresponse))
    
    df1_givenBA_fs <- df_givenBA_fs |> group_by(intensity) |> 
      summarise(nRight = sum(rightresponse),
                nLeft = sum(leftresponse))
    
    df1_givenBB_fs <- df_givenBB_fs |> group_by(intensity) |> 
      summarise(nRight = sum(rightresponse),
                nLeft = sum(leftresponse))
    
    
    # Frequency-biased
    df1_all_freq <- df_freq |> group_by(intensity) |> 
      summarise(nRight = sum(rightresponse),
                nLeft = sum(leftresponse))
    
    df1_givenAA_freq <- df_givenAA_freq |> group_by(intensity) |> 
      summarise(nRight = sum(rightresponse),
                nLeft = sum(leftresponse))
    
    df1_givenAB_freq <- df_givenAB_freq |> group_by(intensity) |> 
      summarise(nRight = sum(rightresponse),
                nLeft = sum(leftresponse))
    
    df1_givenBA_freq <- df_givenBA_freq |> group_by(intensity) |> 
      summarise(nRight = sum(rightresponse),
                nLeft = sum(leftresponse))
    
    df1_givenBB_freq <- df_givenBB_freq |> group_by(intensity) |> 
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
    df1_all_fs$pRight <- df1_all_fs$nRight / (df1_all_fs$nRight + df1_all_fs$nLeft)
    df1_givenAA_fs$pRight <- df1_givenAA_fs$nRight / (df1_givenAA_fs$nRight + df1_givenAA_fs$nLeft)
    df1_givenAB_fs$pRight <- df1_givenAB_fs$nRight / (df1_givenAB_fs$nRight + df1_givenAB_fs$nLeft)
    df1_givenBA_fs$pRight <- df1_givenBA_fs$nRight / (df1_givenBA_fs$nRight + df1_givenBA_fs$nLeft)
    df1_givenBB_fs$pRight <- df1_givenBB_fs$nRight / (df1_givenBB_fs$nRight + df1_givenBB_fs$nLeft)
    
    # Frequency-baised: Add proportion
    df1_all_freq$pRight <- df1_all_freq$nRight / (df1_all_freq$nRight + df1_all_freq$nLeft)
    df1_givenAA_freq$pRight <- df1_givenAA_freq$nRight / (df1_givenAA_freq$nRight + df1_givenAA_freq$nLeft)
    df1_givenAB_freq$pRight <- df1_givenAB_freq$nRight / (df1_givenAB_freq$nRight + df1_givenAB_freq$nLeft)
    df1_givenBA_freq$pRight <- df1_givenBA_freq$nRight / (df1_givenBA_freq$nRight + df1_givenBA_freq$nLeft)
    df1_givenBB_freq$pRight <- df1_givenBB_freq$nRight / (df1_givenBB_freq$nRight + df1_givenBB_freq$nLeft)
    
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
    
    # Fully stochastic: Fit glm
    model_all_fs <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_all_fs, family = binomial())
    model_givenAA_fs <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAA_fs, family = binomial())
    model_givenAB_fs <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAB_fs, family = binomial())
    model_givenBA_fs <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBA_fs, family = binomial())
    model_givenBB_fs <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBB_fs, family = binomial())
    
    # Frequency-biased: Fit glm
    model_all_freq <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_all_freq, family = binomial())
    model_givenAA_freq <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAA_freq, family = binomial())
    model_givenAB_freq <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAB_freq, family = binomial())
    model_givenBA_freq <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBA_freq, family = binomial())
    model_givenBB_freq <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBB_freq, family = binomial())
    
    # Repetition-biased: Fit glm
    model_all_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_all_rep, family = binomial())
    model_givenAA_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAA_rep, family = binomial())
    model_givenAB_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAB_rep, family = binomial())
    model_givenBA_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBA_rep, family = binomial())
    model_givenBB_rep <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBB_rep, family = binomial())
    
    # Alternation-biased: Fit glm
    model_all_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_all_alt, family = binomial())
    model_givenAA_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAA_alt, family = binomial())
    model_givenAB_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenAB_alt, family = binomial())
    model_givenBA_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBA_alt, family = binomial())
    model_givenBB_alt <- glm(cbind(nRight, nLeft) ~ intensity, data = df1_givenBB_alt, family = binomial())
    
    # Fully stochastic: Predict
    yseq_all_fs <- predict(model_all_fs, data.frame(intensity = xseq), type = "response")
    yseq_givenAA_fs <- predict(model_givenAA_fs, data.frame(intensity = xseq), type = "response")
    yseq_givenAB_fs <- predict(model_givenAB_fs, data.frame(intensity = xseq), type = "response")
    yseq_givenBA_fs <- predict(model_givenBA_fs, data.frame(intensity = xseq), type = "response")
    yseq_givenBB_fs <- predict(model_givenBB_fs, data.frame(intensity = xseq), type = "response")
    
    # Frequency-biased: Predict
    yseq_all_freq <- predict(model_all_freq, data.frame(intensity = xseq), type = "response")
    yseq_givenAA_freq <- predict(model_givenAA_freq, data.frame(intensity = xseq), type = "response")
    yseq_givenAB_freq <- predict(model_givenAB_freq, data.frame(intensity = xseq), type = "response")
    yseq_givenBA_freq <- predict(model_givenBA_freq, data.frame(intensity = xseq), type = "response")
    yseq_givenBB_freq <- predict(model_givenBB_freq, data.frame(intensity = xseq), type = "response")
    
    # Repetition-biased: Predict
    yseq_all_rep <- predict(model_all_rep, data.frame(intensity = xseq), type = "response")
    yseq_givenAA_rep <- predict(model_givenAA_rep, data.frame(intensity = xseq), type = "response")
    yseq_givenAB_rep <- predict(model_givenAB_rep, data.frame(intensity = xseq), type = "response")
    yseq_givenBA_rep <- predict(model_givenBA_rep, data.frame(intensity = xseq), type = "response")
    yseq_givenBB_rep <- predict(model_givenBB_rep, data.frame(intensity = xseq), type = "response")
    
    # Alternation-biased: Predict
    yseq_all_alt <- predict(model_all_alt, data.frame(intensity = xseq), type = "response")
    yseq_givenAA_alt <- predict(model_givenAA_alt, data.frame(intensity = xseq), type = "response")
    yseq_givenAB_alt <- predict(model_givenAB_alt, data.frame(intensity = xseq), type = "response")
    yseq_givenBA_alt <- predict(model_givenBA_alt, data.frame(intensity = xseq), type = "response")
    yseq_givenBB_alt <- predict(model_givenBB_alt, data.frame(intensity = xseq), type = "response")
    
    # Fully stochastic: Curve
    curve_all_fs <- data.frame(xseq, yseq_all_fs)
    curve_givenAA_fs <- data.frame(xseq, yseq_givenAA_fs)
    curve_givenAB_fs <- data.frame(xseq, yseq_givenAB_fs)
    curve_givenBA_fs <- data.frame(xseq, yseq_givenBA_fs)
    curve_givenBB_fs <- data.frame(xseq, yseq_givenBB_fs)
    
    # Frequency-biased: Curve
    curve_all_freq <- data.frame(xseq, yseq_all_freq)
    curve_givenAA_freq <- data.frame(xseq, yseq_givenAA_freq)
    curve_givenAB_freq <- data.frame(xseq, yseq_givenAB_freq)
    curve_givenBA_freq <- data.frame(xseq, yseq_givenBA_freq)
    curve_givenBB_freq <- data.frame(xseq, yseq_givenBB_freq)
    
    # Repetition-baised: Curve
    curve_all_rep <- data.frame(xseq, yseq_all_rep)
    curve_givenAA_rep <- data.frame(xseq, yseq_givenAA_rep)
    curve_givenAB_rep <- data.frame(xseq, yseq_givenAB_rep)
    curve_givenBA_rep <- data.frame(xseq, yseq_givenBA_rep)
    curve_givenBB_rep <- data.frame(xseq, yseq_givenBB_rep)
    
    # Alternation-baised: Curve
    curve_all_alt <- data.frame(xseq, yseq_all_alt)
    curve_givenAA_alt <- data.frame(xseq, yseq_givenAA_alt)
    curve_givenAB_alt <- data.frame(xseq, yseq_givenAB_alt)
    curve_givenBA_alt <- data.frame(xseq, yseq_givenBA_alt)
    curve_givenBB_alt <- data.frame(xseq, yseq_givenBB_alt)
    
    
    auditory_shift_givenAA_fs[0+i] <- extractPsychometricShift(curve_givenAA_fs)
    auditory_shift_givenAB_fs[0+i] <- extractPsychometricShift(curve_givenAB_fs)
    auditory_shift_givenBA_fs[0+i] <- extractPsychometricShift(curve_givenBA_fs)
    auditory_shift_givenBB_fs[0+i] <- extractPsychometricShift(curve_givenBB_fs)
    
    auditory_shift_givenAA_freq[0+i] <- extractPsychometricShift(curve_givenAA_freq)
    auditory_shift_givenAB_freq[0+i] <- extractPsychometricShift(curve_givenAB_freq)
    auditory_shift_givenBA_freq[0+i] <- extractPsychometricShift(curve_givenBA_freq)
    auditory_shift_givenBB_freq[0+i] <- extractPsychometricShift(curve_givenBB_freq)
    
    auditory_shift_givenAA_rep[0+i] <- extractPsychometricShift(curve_givenAA_rep)
    auditory_shift_givenAB_rep[0+i] <- extractPsychometricShift(curve_givenAB_rep)
    auditory_shift_givenBA_rep[0+i] <- extractPsychometricShift(curve_givenBA_rep)
    auditory_shift_givenBB_rep[0+i] <- extractPsychometricShift(curve_givenBB_rep)
    
    auditory_shift_givenAA_alt[0+i] <- extractPsychometricShift(curve_givenAA_alt)
    auditory_shift_givenAB_alt[0+i] <- extractPsychometricShift(curve_givenAB_alt)
    auditory_shift_givenBA_alt[0+i] <- extractPsychometricShift(curve_givenBA_alt)
    auditory_shift_givenBB_alt[0+i] <- extractPsychometricShift(curve_givenBB_alt)



}



subject <- levels(allValuesAuditory$subject)

auditoryPsychometricData <- tibble(subject,
                                 auditory_shift_givenAA_fs,
                                 auditory_shift_givenAB_fs,
                                 auditory_shift_givenBA_fs,
                                 auditory_shift_givenBB_fs,
                                 auditory_shift_givenAA_freq,
                                 auditory_shift_givenAB_freq,
                                 auditory_shift_givenBA_freq,
                                 auditory_shift_givenBB_freq,
                                 auditory_shift_givenAA_rep,
                                 auditory_shift_givenAB_rep,
                                 auditory_shift_givenBA_rep,
                                 auditory_shift_givenBB_rep,
                                 auditory_shift_givenAA_alt,
                                 auditory_shift_givenAB_alt,
                                 auditory_shift_givenBA_alt,
                                 auditory_shift_givenBB_alt)


save(auditoryPsychometricData, file = "processed_data/Psychometric/auditoryPsychometricData.Rda")

