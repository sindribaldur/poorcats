fct_c <- function(...) {
  fs <- rlang::list2(...)
  fs <- check_factor_list(fs, "...")

  if (length(fs) == 0) {
    return(factor())
  }

  levels <- lvls_union(fs)
  all <- unlist(fct_unify(fs, levels), use.names = FALSE)
  factor(all, levels = levels, exclude = NULL)
}

fct_unify <- function(fs, levels = lvls_union(fs)) {
  fs <- check_factor_list(fs)

  lapply(fs, lvls_expand, new_levels = levels)
}
