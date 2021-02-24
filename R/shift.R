fct_shift <- function(f, n = 1L) {
  lvls_reorder(f, shift(nlevels(f), n))
}

shift <- function(m, n) {
  stopifnot(is.numeric(m), length(m) == 1L)
  stopifnot(is.numeric(n), length(n) == 1L)

  ((seq_len(m) - 1) + n) %% m + 1
}
