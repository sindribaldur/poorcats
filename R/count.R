fct_count <- function(f, sort = FALSE, prop = FALSE) {
  f2 <- check_factor(f)
  n_na <- sum(is.na(f))

  df <- tibble::tibble(
    f = fct_inorder(c(levels(f2), if (n_na > 0) NA)),
    n = c(tabulate(f2, nlevels(f)), if (n_na > 0) n_na)
  )

  if (sort) {
    df <- df[order(df$n, decreasing = TRUE), ]
  }

  if (prop) {
    df$p <- prop.table(df$n)
  }

  df
}
