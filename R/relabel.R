fct_relabel <- function(.f, .fun, ...) {
  f <- check_factor(.f)
  .fun <- as_function(.fun)

  old_levels <- levels(f)
  new_levels <- .fun(old_levels, ...)

  lvls_revalue(f, new_levels)
}
