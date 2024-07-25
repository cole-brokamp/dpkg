#' as labeled vector
#'
#' Convert a vector into a labeled vector (`lbl_vec`) by providing a label
#' @param x a vector (see `vctrs::obj_check_vector()`)
#' type `lbl_vec`, a labeled vector
#' @export
#' @examples
#' as_lbl_vec(letters, "the alphabet")
#' as_lbl_vec(1:10, "the numbers")
#' as_lbl_vec(c(T, T, F, T), "is foofy")
#' x <- as_lbl_vec(letters, "The Alphabet")
as_lbl_vec <- function(x = vector(), label, ...) {
  vctrs::obj_check_vector(x)
  if (vctrs::vec_size(label) > 1) rlang::abort("`label` must be length 1")
  structure(x,
    label = as.character(label %||% deparse(substitute(x))),
    class = c("lbl_vec", class(x))
  )
}

#' get label
#'
#' get the label of a labeled vector
#' @param x a `lbl_vec` object (see `as_lbl_vec()`)
#' @param missing_label what to return in the case of no label attribute: `"na"` returns missing values,
#' `"deparse"` substitutes and deparses the code used to define `x` into a character string, and
#' `"as_label"` tries to convert the code supplied as `x` to a label with `rlang::as_label()`
#' @returns character string of label or `NA` if label does not exist
#' @export
#' @examples
#' as_lbl_vec(letters, "The Letters") |>
#'   get_label()
#' get_label(letters)
#' get_label(letters, missing_label = "deparse")
#' get_label(letters, missing_label = "as_label")
#' toupper(letters) |>
#'   get_label(missing_label = "deparse")
#' toupper(letters) |>
#'   get_label(missing_label = "as_label")
get_label <- function(x, missing_label = c("na", "deparse", "as_label")) {
  switch(
    rlang::arg_match(missing_label),
    na = attr(x, "label") %||% NA_character_,
    deparse = attr(x, "label") %||% deparse(substitute(x)),
    as_label = attr(x, "label") %||% rlang::as_label(x)
  )
}

#' @export
print.lbl_vec <- function(x, ...) {
  x_label <- get_label(x)
  if (!is.na(x_label)) {
    cat(attr(x, "label"), "\n", sep = "")
  }
  attr(x, "label") <- NULL
  class(x) <- class(x)[!class(x) %in% "lbl_vec"]
  NextMethod("print")
}

