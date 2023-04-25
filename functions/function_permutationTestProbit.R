# Function: Run permutation tests on glm-objects

# Main function
permutationTest_TP <- function(x) {
  model <- update(x, data = transform(x$data, TP = sample(TP)))
}

permutationTest_IF <- function(x) {
  model <- update(x, data = transform(x$data, IF = sample(IF)))
}

permutationTest_AF <- function(x) {
  model <- update(x, data = transform(x$data, AF = sample(AF)))
}