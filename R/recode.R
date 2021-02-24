fct_recode <- function(.f, ...) {
  f <- check_factor(.f)

  new_levels <- check_recode_levels(...)

  # Remove any named NULL and finish if all NULLs
  nulls <- names(new_levels) == "NULL"
  if (any(nulls)) {
    f <- factor(f, levels = setdiff(levels(f), new_levels[nulls]))
    new_levels <- new_levels[!nulls]
  }

  # Match old levels with new levels
  old_levels <- levels(f)
  idx <- match(new_levels, old_levels)

  # Handle levels that don't exist
  if (any(is.na(idx))) {
    bad <- new_levels[is.na(idx)]
    warning("Unknown levels in `f`: ", paste(bad, collapse = ", "), call. = FALSE)

    new_levels <- new_levels[!is.na(idx)]
    idx <- idx[!is.na(idx)]
  }

  old_levels[idx] <- names(new_levels)

  lvls_revalue(f, old_levels)
}

check_recode_levels <- function(...) {
  levels <- rlang::list2(...)

  is_ok <- function(x) is.character(x) && length(x) == 1
  ok <- vapply(levels, is_ok, logical(1))

  if (!all(ok)) {
    stop(
      "Each input to fct_recode must be a single named string. ",
      "Problems at positions: ", paste0(which(!ok), collapse = ", "),
      call. = FALSE
    )
  }

  unlist(levels)
}
