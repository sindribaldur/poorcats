fct_lump <- function(f, n, prop, w = NULL, other_level = "Other",
                     ties.method = c("min", "average", "first", "last", "random", "max")) {

  ties.method <- match.arg(ties.method)
  check_calc_levels(f, w)


  if (missing(n) && missing(prop)) {
    fct_lump_lowfreq(f, other_level = other_level)
  } else if (missing(prop)) {
    fct_lump_n(f, n, w, other_level, ties.method)
  } else if (missing(n)) {
    fct_lump_prop(f, prop, w, other_level)
  } else {
    abort("Must supply only one of `n` and `prop`")
  }
}

fct_lump_min <- function(f, min, w = NULL, other_level = "Other") {

  calcs <- check_calc_levels(f, w)
  f <- calcs$f

  if (!is.numeric(min) || length(min) != 1 || min < 0) {
    rlang::abort("`min` must be a positive number")
  }

  new_levels <- ifelse(calcs$count >= min, levels(f), other_level)

  if (other_level %in% new_levels) {
    f <- lvls_revalue(f, new_levels)
    fct_relevel(f, other_level, after = Inf)
  } else {
    f
  }

}

fct_lump_prop <- function(f, prop, w = NULL, other_level = "Other") {

  calcs <- check_calc_levels(f, w)
  f <- calcs$f

  if (!is.numeric(prop) || length(prop) != 1) {
    rlang::abort("`prop` must be a number")
  }

  prop_n <- calcs$count / calcs$total
  if (prop < 0) {
    new_levels <- ifelse(prop_n <= -prop, levels(f), other_level)
  } else {
    new_levels <- ifelse(prop_n > prop, levels(f), other_level)
  }

  if (prop > 0 && sum(prop_n <= prop) <= 1) {
    # No lumping needed
    return(f)
  }

  if (other_level %in% new_levels) {
    f <- lvls_revalue(f, new_levels)
    fct_relevel(f, other_level, after = Inf)
  } else {
    f
  }
}


fct_lump_n <- function(f, n, w = NULL, other_level = "Other",
                       ties.method = c("min", "average", "first", "last", "random", "max")) {

  ties.method <- match.arg(ties.method)
  calcs <- check_calc_levels(f, w)
  f <- calcs$f

  if (!is.numeric(n) || length(n) != 1) {
    rlang::abort("`n` must be a number")
  }

  if (n < 0) {
    rank <- rank(calcs$count, ties.method = ties.method)
    n <- -n
  } else {
    rank <- rank(-calcs$count, ties.method = ties.method)
  }

  new_levels <- ifelse(rank <= n, levels(f), other_level)

  if (sum(rank > n) <= 1) {
    # No lumping needed
    return(f)
  }

  if (other_level %in% new_levels) {
    f <- lvls_revalue(f, new_levels)
    fct_relevel(f, other_level, after = Inf)
  } else {
    f
  }

}

fct_lump_lowfreq <- function(f, other_level = "Other") {

  calcs <- check_calc_levels(f, NULL)
  f <- calcs$f

  new_levels <- ifelse(!in_smallest(calcs$count), levels(f), other_level)

  if (other_level %in% new_levels) {
    f <- lvls_revalue(f, new_levels)
    fct_relevel(f, other_level, after = Inf)
  } else {
    f
  }

}

check_calc_levels <- function(f, w = NULL) {
  f <- check_factor(f)
  w <- check_weights(w, length(f))

  if (is.null(w)) {
    count <- as.vector(table(f))
    total <- length(f)
  } else {
    count <- as.vector(tapply(w, f, FUN = sum))
    total <- sum(w)
  }
  list(f = f, count = count, total = total)
}

# Lump together smallest groups, ensuring that the collective
# "other" is still the smallest group. Assumes x is vector
# of counts in descending order
lump_cutoff <- function(x) {
  left <- sum(x)

  for (i in seq_along(x)) {
    # After group, there are this many left
    left <- left - x[i]

    if (x[i] > left)
      return(i + 1)
  }

  length(x) + 1
}

# Given vector of counts, returns logical vector if in
# smallest groups
in_smallest <- function(x) {
  ord_x <- order(x, decreasing = TRUE)
  idx <- lump_cutoff(x[ord_x])

  to_lump <- seq_along(x) >= idx
  # Undo initial ordering
  to_lump[order(ord_x)]
}

check_weights <- function(w, n = length(w)) {
  if (is.null(w)) {
    return(w)
  }

  if (!is.numeric(w)) {
    stop("`w` must be a numeric vector", call. = FALSE)
  }

  if (length(w) != n) {
    stop(
      "`w` must be the same length as `f` (", n, "), not length ", length(w),
      call. = FALSE
    )
  }

  bad <- w < 0 | is.na(w)
  if (any(bad)) {
    stop(
      "All `w` must be non-negative and non-missing. Problems at positions: ",
      paste0(which(bad), collapse = ", "),
      call. = FALSE
    )
  }

  w
}
