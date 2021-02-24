fct_anon <- function(f, prefix = "") {
  levels <- paste0(prefix, zero_pad(seq_len(nlevels(f))))

  f <- lvls_revalue(f, sample(levels))
  lvls_reorder(f, match(levels, levels(f)))
}

digits <- function(x) nchar(max(x, na.rm = TRUE))

zero_pad <- function(x) {
  sprintf(paste0("%0", digits(x), "d"), x)
}
