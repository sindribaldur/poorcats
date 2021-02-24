as_factor <- function(x, ...) {
  ellipsis::check_dots_used()
  UseMethod("as_factor")
}

as_factor.factor <- function(x, ...) {
  x
}

as_factor.character <- function(x, ...) {
  # Preserve label for future haven compatibility
  structure(
    fct_inorder(x),
    label = attr(x, "label", exact = TRUE)
  )
}

as_factor.numeric <- function(x, ...) {
  factor(x)
}

as_factor.logical <- function(x, ...) {
  factor(x, levels = c("FALSE", "TRUE"))
}
