as_lbl_vec <- function(x = vector(), label = character(), ...) {
  if (!rlang::is_atomic(x)) rlang::abort("`x` must be an atomic vector")
  if (vctrs::vec_size(label) > 1) rlang::abort("`label` must be length 1")
  structure(x,
    label = as.character(label %||% deparse(substitute(x))),
    class = c("lbl_vec", class(x))
  )
}

print.lbl_vec <- function(x, ...) {
  cat(attr(x, "label"), "\n", sep = "")
  attr(x, "label") <- NULL
  class(x) <- class(x)[!class(x) %in% "lbl_vec"]
  NextMethod("print")
}

format.lbl_vec <- function(x, ...) {
  cat(attr(x, "label"), "\n", sep = "")
  attr(x, "lbl_vec") <- NULL
  class(x) <- class(x)[!class(x) %in% "lbl_vec"]
  format(x)
}

as_lbl_vec(letters, "the alphabet")

library(pillar)

pillar_shaft.lbl_vec <- function(x, ...) {
  default_shaft <- NextMethod()
  default_shaft
}

new_pillar_title.lbl_vec <- function(x, ...) {
  vctrs::vec_cast
  as.vector(x)
}

tibble::tibble(
  id = labels(letters),
  letters = as_lbl_vec(letters, "The Alphabet")
)
