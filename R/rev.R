fct_rev <- function(f) {
  f <- check_factor(f)

  lvls_reorder(f, rev(lvls_seq(f)))
}
