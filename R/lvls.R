NULL

lvls_reorder <- function(f, idx, ordered = NA) {
  f <- check_factor(f)
  if (!is.numeric(idx)) {
    stop("`idx` must be numeric", call. = FALSE)
  }
  if (!setequal(idx, lvls_seq(f)) || length(idx) != nlevels(f)) {
    stop("`idx` must contain one integer for each level of `f`", call. = FALSE)
  }

  refactor(f, levels(f)[idx], ordered = ordered)
}

lvls_revalue <- function(f, new_levels) {
  f <- check_factor(f)

  if (!is.character(new_levels)) {
    stop("`new_levels` must be a character vector", call. = FALSE)
  }

  if (length(new_levels) != nlevels(f)) {
    stop(
      "`new_levels` must be the same length as `levels(f)`: expected ",
      nlevels(f), " new levels, got ", length(new_levels), ".",
      call. = FALSE
    )
  }

  if (anyDuplicated(new_levels)) {
    # Collapse levels, creating a new factor
    u_levels <- unique(new_levels)
    index <- match(new_levels, u_levels)

    out <- index[f]
    attributes(out) <- attributes(f)
    attr(out, "levels") <- u_levels
    out
  } else {
    attr(f, "levels") <- new_levels
    f
  }
}

lvls_expand <- function(f, new_levels) {
  f <- check_factor(f)

  missing <- setdiff(levels(f), new_levels)
  if (length(missing) > 0) {
    stop(
      "Must include all existing levels. Missing: ", paste0(missing, collapse = ", "),
      call. = FALSE)
  }

  refactor(f, new_levels)
}

lvls_seq <- function(f) {
  seq_along(levels(f))
}

refactor <- function(f, new_levels, ordered = NA) {
  if (is.na(ordered)) {
    ordered <- is.ordered(f)
  }

  new_f <- factor(f, levels = new_levels, exclude = NULL, ordered = ordered)
  attributes(new_f) <- utils::modifyList(attributes(f), attributes(new_f))
  new_f
}


lvls_union <- function(fs) {
  fs <- check_factor_list(fs)
  Reduce(function(x, y) union(x, levels(y)), fs, init = character())
}
