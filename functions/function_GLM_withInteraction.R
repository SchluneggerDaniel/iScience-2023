# Functions: Probit regression with stimulus, intensity, and respective
# learning model (interaction)

# Transition Probability
fullModelTP <- function(df) {
  glm(response ~ stimulus + intensity_ord * TP,
      family = binomial(probit),
      data = df)
}

# Item Frequency
fullModelIF <- function(df) {
  glm(response ~ stimulus + intensity_ord * IF,
      family = binomial(probit),
      data = df)
}

# Alternation Frequency
fullModelAF <- function(df) {
  glm(response ~ stimulus + intensity_ord * AF,
      family = binomial(probit),
      data = df)
}

# Stimulus and intensity
fullModelStim <- function(df) {
  glm(response ~ stimulus + intensity_ord,
      family = binomial(probit),
      data = df)
}


### === More complex models === ###

# Only as comment (problems with multicollinearity)

# Function: Probit regression for stimulus, transition probability and item frequency

# fullModelTPIF <- function(df) {
#   glm(response ~ stimulus + intensity_ord + TP + IF,
#       family = binomial(probit),
#       data = df)
# }