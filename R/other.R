fct_other <- function(f, keep, drop, other_level = "Other") {
  f <- check_factor(f)

  if (!xor(missing(keep), missing(drop))) {
    stop("Must supply exactly one of `keep` and `drop`", call. = FALSE)
  }

  levels <- levels(f)
  if (!missing(keep)) {
    levels[!levels %in% keep] <- other_level
  } else {
    levels[levels %in% drop] <- other_level
  }

  f <- lvls_revalue(f, levels)
  fct_relevel(f, other_level, after = Inf)
}
