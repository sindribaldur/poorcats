fct_drop <- function(f, only) {
  f <- check_factor(f)

  levels <- levels(f)
  count <- table(f)

  to_drop <- levels[count == 0]
  if (!missing(only)) {
    to_drop <- intersect(to_drop, only)
  }

  refactor(f, new_levels = setdiff(levels, to_drop))
}
