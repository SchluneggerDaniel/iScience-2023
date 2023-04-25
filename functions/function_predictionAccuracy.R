# Function: Prediction Accuracy from logistic / probit regression

predictionAccuracy <-  function(x){
  
  classify <- x$data |> 
    mutate(predicted = if_else((fitted(x)) < 0.5, "0", "1"), 
           predicted = factor(predicted, levels = (c("0", "1"))))
  
  class_table <- table(classify$predicted, classify$response)
  sum(diag(class_table))/sum(class_table)
  
}