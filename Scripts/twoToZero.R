twoToZero <- function(x) {
  # To convert 2s to 0s when dealing with Yes/No questions
  ifelse(x == 2, 0, x)
}