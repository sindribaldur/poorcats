fct_unique <- function(f) {
  factor(levels(f), levels(f), exclude = NULL, ordered = is.ordered(f))
}
