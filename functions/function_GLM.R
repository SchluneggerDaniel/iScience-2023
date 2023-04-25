# Functions: Probit regression for stimulus and respective learning model or
# stimulus-only model

# Transition Probability
modelTP <- function(df) {
  glm(response ~ stimulus + TP,
      family = binomial(probit),
      data = df)
}

# Item Frequency
modelIF <- function(df) {
  glm(response ~ stimulus + IF,
      family = binomial(probit),
      data = df)
}

# Alternation Frequency
modelAF <- function(df) {
  glm(response ~ stimulus + AF,
      family = binomial(probit),
      data = df)
}

modelStimulus <- function(df) {
  glm(response ~ stimulus,
      family = binomial(probit),
      data = df)
}

### === More complex models === ###

# Only as comment

# Function: Probit regression for stimulus and transition probability

# modelTPIF <- function(df) {
#   glm(response ~ stimulus + TP + IF,
#       family = binomial(probit),
#       data = df)
# }