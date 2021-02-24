fct_shuffle <- function(f) {
  f <- check_factor(f)

  lvls_reorder(f, sample(lvls_seq(f)))
}
