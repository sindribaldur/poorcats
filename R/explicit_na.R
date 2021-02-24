fct_explicit_na <- function(f, na_level = "(Missing)") {
  f <- check_factor(f)

  is_missing <- is.na(f)
  is_missing_level <- is.na(levels(f))

  if (any(is_missing)) {
    f <- fct_expand(f, na_level)
    f[is_missing] <- na_level

    f
  } else if (any(is_missing_level)) {
    levs <- levels(f)
    levs[is.na(levs)] <- na_level

    lvls_revalue(f, levs)
  } else {
    f
  }
}
