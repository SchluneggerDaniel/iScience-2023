# Extract Coefficients for different learning models from glm objects

extractCoefficients_TP <- function(df){
  unnest(df, cols = coef_TP) |> 
    select(-c("statistic", "p.value")) |> 
    pivot_wider(names_from = term, values_from = c("estimate", "std.error"))
}


extractCoefficients_IF <- function(df){
  unnest(df, cols = coef_IF) |> 
    select(-c("statistic", "p.value")) |> 
    pivot_wider(names_from = term, values_from = c("estimate", "std.error"))
}


extractCoefficients_AF <- function(df){
  unnest(df, cols = coef_AF) |> 
    select(-c("statistic", "p.value")) |> 
    pivot_wider(names_from = term, values_from = c("estimate", "std.error"))
}