fct_cross <- function(..., sep = ":", keep_empty = FALSE) {

  flist <- list2(...)
  if (length(flist) == 0) {
    return(factor())
  }

  .data <- tibble::as_tibble(flist, .name_repair = "minimal")
  .data <- lapply(.data, check_factor)

  newf <- exec(paste, !!!.data, sep = sep)

  old_levels <- lapply(.data, levels)
  grid <- exec(expand.grid, old_levels)
  new_levels <- exec(paste, !!!grid, sep = sep)

  if (!keep_empty) {
    new_levels <- intersect(new_levels, newf)
  }
  factor(newf, levels = new_levels)
}
