fct_expand <- function(f, ...) {
  f <- check_factor(f)

  new_levels <- chr(...)
  lvls_expand(f, union(levels(f), new_levels))
}
