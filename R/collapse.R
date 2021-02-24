fct_collapse <- function(.f, ..., other_level = NULL, group_other = "DEPRECATED") {

  if (!missing(group_other)) {
    warning("`group_other` is deprecated. Please use `other_level` instead")
    if (isTRUE(group_other) && is.null(other_level)) {
      other_level <- "Other"
    }
  }

  new <- rlang::list2(...)
  levs <- as.list(unlist(new, use.names = FALSE))

  if (!is.null(other_level)){
    f <- check_factor(.f)
    levels <- levels(f)
    new[[other_level]] <- levels[!levels %in% levs]
    levs <- c(levs, new[[other_level]])
  }

  names(levs) <- names(new)[rep(seq_along(new), vapply(new, length, integer(1)))]
  out <- fct_recode(.f, !!!levs)

  if (any(levels(out) == other_level)){
    fct_relevel(out, other_level, after = Inf)
  } else return(out)

}
