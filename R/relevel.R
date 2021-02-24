fct_relevel <- function(.f, ..., after = 0L) {
  f <- check_factor(.f)

  old_levels <- levels(f)
  if (dots_n(...) == 1L && (is.function(..1) || is_formula(..1))) {
    fun <- as_function(..1)
    first_levels <- fun(old_levels)
    if (!is.character(first_levels)) {
      stop("Re-leveling function must return character vector", call. = FALSE)
    }
  } else {
    first_levels <- chr(...)
  }

  unknown <- setdiff(first_levels, old_levels)
  if (length(unknown) > 0) {
    warning("Unknown levels in `f`: ", paste(unknown, collapse = ", "), call. = FALSE)
    first_levels <- intersect(first_levels, old_levels)
  }

  new_levels <- append(setdiff(old_levels, first_levels), first_levels, after = after)

  lvls_reorder(f, match(new_levels, old_levels))
}
