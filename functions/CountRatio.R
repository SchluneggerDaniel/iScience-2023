# Quantify orthogonal predictions between models

# df = data from response-based model
# df_generative = data from stimulus-based model

# Transition Probability
CountRatio_TP <- function(df, df_generative) {
  
  # Compute response-based correct prediction
  respcorrect <- tibble(df$response,  
                        df$TP,
                        df_generative$TP) |> 
    rename(response = `df$response`,
           TP = `df$TP`,
           TP_generative = `df_generative$TP`) |> 
    filter((TP > 0 & TP_generative < 0 & response == 1) | 
             (TP < 0 & TP_generative > 0 & response == 0)) |> 
    summarise(countrespcorrect = n())
  
  # Compute stimulus-based correct prediction
  stimcorrect <- tibble(df$response,  
                        df$TP,
                        df_generative$TP) |> 
    rename(response = `df$response`,
           TP = `df$TP`,
           TP_generative = `df_generative$TP`) |> 
    filter((TP < 0 & TP_generative > 0 & response == 1) | 
             (TP > 0 & TP_generative < 0 & response == 0)) |> 
    summarise(countstimcorrect = n())
  
  ratioTP <- (respcorrect / stimcorrect) |> 
    rename(ratioTP = countrespcorrect) # Rename the output variable (?)
  
  return(ratioTP)
  
}


# Item Frequeny
CountRatio_IF <- function(df, df_generative) {
  
  # Compute response-based correct prediction
  respcorrect <- tibble(df$response,  
                        df$IF,
                        df_generative$IF) |> 
    rename(response = `df$response`,
           IF = `df$IF`,
           IF_generative = `df_generative$IF`) |> 
    filter((IF > 0 & IF_generative < 0 & response == 1) | 
             (IF < 0 & IF_generative > 0 & response == 0)) |> 
    summarise(countrespcorrect = n())
  
  # Compute stimulus-based correct prediction
  stimcorrect <- tibble(df$response,  
                        df$IF,
                        df_generative$IF) |> 
    rename(response = `df$response`,
           IF = `df$IF`,
           IF_generative = `df_generative$IF`) |> 
    filter((IF < 0 & IF_generative > 0 & response == 1) | 
             (IF > 0 & IF_generative < 0 & response == 0)) |> 
    summarise(countstimcorrect = n())
  
  ratioIF <- (respcorrect / stimcorrect) |> 
    rename(ratioIF = countrespcorrect) # Rename the output variable (?)
  
  return(ratioIF)
  
}


# Alternation Frequeny
CountRatio_AF <- function(df, df_generative) {
  
  # Compute response-based correct prediction
  respcorrect <- tibble(df$response,  
                        df$AF,
                        df_generative$AF) |> 
    rename(response = `df$response`,
           AF = `df$AF`,
           AF_generative = `df_generative$AF`) |> 
    filter((AF > 0 & AF_generative < 0 & response == 1) | 
             (AF < 0 & AF_generative > 0 & response == 0)) |> 
    summarise(countrespcorrect = n())
  
  # Compute stimulus-based correct prediction
  stimcorrect <- tibble(df$response,  
                        df$AF,
                        df_generative$AF) |> 
    rename(response = `df$response`,
           AF = `df$AF`,
           AF_generative = `df_generative$AF`) |> 
    filter((AF < 0 & AF_generative > 0 & response == 1) | 
             (AF > 0 & AF_generative < 0 & response == 0)) |> 
    summarise(countstimcorrect = n())
  
  ratioAF <- (respcorrect / stimcorrect) |> 
    rename(ratioAF = countrespcorrect) # Rename the output variable (?)
  
  return(ratioAF)
  
}
