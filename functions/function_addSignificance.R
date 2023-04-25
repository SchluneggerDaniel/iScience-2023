# Add Factor Significance to respective dataframe

addSignificance_TP <- function(x){
  
  x <- x |> 
    mutate(Significance = ifelse(estimate_TP < low_quantile_TP | estimate_TP > high_quantile_TP,
                                 "significant",
                                 "non-significant"),
           Significance = factor(Significance,
                                 levels = c("significant",
                                            "non-significant")))
}

addSignificance_IF <- function(x){
  
  x <- x |> 
    mutate(Significance = ifelse(estimate_IF < low_quantile_IF | estimate_IF > high_quantile_IF,
                                 "significant",
                                 "non-significant"),
           Significance = factor(Significance,
                                 levels = c("significant",
                                            "non-significant")))
}

addSignificance_AF <- function(x){
  
  x <- x |> 
    mutate(Significance = ifelse(estimate_AF < low_quantile_AF | estimate_AF > high_quantile_AF,
                                 "significant",
                                 "non-significant"),
           Significance = factor(Significance,
                                 levels = c("significant",
                                            "non-significant")))
}