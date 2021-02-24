fct_match <- function(f, lvls) {
  f <- check_factor(f)

  bad_lvls <- setdiff(lvls, levels(f))
  bad_lvls <- bad_lvls[!is.na(bad_lvls)]
  if (length(bad_lvls) > 0) {
    stop(
      "Levels not present in factor: ",
      paste0(encodeString(bad_lvls, quote = '"'), collapse = ", "),
      call. = FALSE
    )
  }

  f %in% lvls
}
