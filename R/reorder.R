fct_reorder <- function(.f, .x, .fun = median, ..., .desc = FALSE) {
  f <- check_factor(.f)
  stopifnot(length(f) == length(.x))
  ellipsis::check_dots_used()

  summary <- tapply(.x, .f, .fun, ...)
  # This is a bit of a weak test, but should detect the most common case
  # where `.fun` returns multiple values.
  if (is.list(summary)) {
    stop("`fun` must return a single value per group", call. = FALSE)
  }

  lvls_reorder(f, order(summary, decreasing = .desc))
}

fct_reorder2 <- function(.f, .x, .y, .fun = last2, ..., .desc = TRUE) {
  f <- check_factor(.f)
  stopifnot(length(f) == length(.x), length(.x) == length(.y))
  ellipsis::check_dots_used()

  summary <- tapply(seq_along(.x), f, function(i) .fun(.x[i], .y[i], ...))
  if (is.list(summary)) {
    stop("`fun` must return a single value per group", call. = FALSE)
  }

  lvls_reorder(.f, order(summary, decreasing = .desc))
}


last2 <- function(.x, .y) {
  .y[order(.x, na.last = FALSE)][length(.y)]
}

first2 <- function(.x, .y) {
  .y[order(.x)][1]
}



fct_inorder <- function(f, ordered = NA) {
  f <- check_factor(f)

  idx <- as.integer(f)[!duplicated(f)]
  idx <- idx[!is.na(idx)]
  lvls_reorder(f, idx, ordered = ordered)
}

fct_infreq <- function(f, ordered = NA) {
  f <- check_factor(f)

  lvls_reorder(f, order(table(f), decreasing = TRUE), ordered = ordered)
}

fct_inseq <- function(f, ordered = NA) {
  f <- check_factor(f)

  num_levels <- suppressWarnings(as.numeric(levels(f)))

  if (all(is.na(num_levels))) {
    stop("At least one existing level must be coercible to numeric.", call. = FALSE)
  }

  lvls_reorder(f, order(num_levels), ordered = ordered)
}
