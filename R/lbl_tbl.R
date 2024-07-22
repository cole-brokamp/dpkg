#' @importFrom pillar tbl_format_setup
#' @export
tbl_format_setup.lbl_tbl <- function(x, width, ...) {
  setup <- NextMethod()
  setup <- modifyList(setup, attributes(x), keep.null = FALSE)
  setup
}

#' @importFrom pillar tbl_format_header
#' @export
tbl_format_header.lbl_tbl <- function(x, setup, ...) {
  default_header <- NextMethod()
  id_header <- pillar::style_subtle(paste0("# ", cli::symbol$menu, " ", setup$name, " v", setup$version))
  extra_header <-
    pillar::style_subtle(
      paste0("# ", cli::symbol$em_dash, " title: ",
             cli::symbol$dquote_left, setup$title, cli::symbol$dquote_right))
  c(id_header, default_header, extra_header)
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.lbl_tbl <- function(x, ...) {
  c("A labeled data frame" = pillar::dim_desc(x))
}

#' @importFrom pillar tbl_format_footer
#' @export
tbl_format_footer.lbl_tbl <- function(x, ...) {
  default_footer <- NextMethod()
  extra_info <- c(
    "Use `lbl::get_labels()` to get column-specific labels",
    "Use `lbl::get_md()` to get table-specific metadata"
  )
  extra_footer <- pillar::style_subtle(paste0("# ", cli::symbol$info, " ", extra_info))
  c(default_footer, extra_footer)
}

#' as labeled table
#'
#' Convert a data frame or tibble into a labeled table (`lbl_tbl`) by providing metadata labels.
#' Metadata labels can include `name`, `title`, `version`, `description`, `created`.
#' By default, `x` is deparsed to create the `name` label and the current system time is used as the `created` label.
#' @param x a tibble or data frame
#' @param ... `name = value` pairs (or a list) of data frame labels
#' @returns a labeled tibble (`lbl_tbl`) object
#' @export
#' @examples
#' as_lbl_tbl(mtcars, name = "mtcars", title = "Motor Trend Road Car Tests")
#' d <-
#'   tibble::tibble(
#'     id = labels(letters[1:5]),
#'     letters = as_lbl_vec(letters[1:5], "The Alphabet"),
#'     numbers = as_lbl_vec(1:5, "the numbers"),
#'     logicals = as_lbl_vec(c(TRUE, TRUE, FALSE, TRUE, FALSE), "is foofy")
#'   ) |>
#'   as_lbl_tbl(d)
#' d
#' d$letters
as_lbl_tbl <- function(x, ...) {
  dots <- rlang::dots_list(..., .homonyms = "error", .ignore_empty = "all", .check_assign = TRUE)
  x_tbl <- tibble::as_tibble(x)
  x_tbl <- labelVector::set_label(x_tbl, .dots = as.list(dots$schema))
  out <- list(x = x_tbl, class = c("lbl_tbl", "tbl"))
  out$name <- as.character(dots$name %||% deparse(substitute(x)))
  out$title <- as.character(dots$title)
  out$version <- as.numeric_version(dots$version)
  out$description <- as.character(dots$description)
  out$created <- as.POSIXct(dots$created %||% Sys.time())
  do.call(vctrs::new_data_frame, out)
}

#' get labels
#'
#' Get labels of all vectors in a labeled table.
#' By default, vectors without a label are labeled based on their name.
#' @param x a `lbl_tbl` object (see `as_lbl_tbl()`)
#' @param missing_label what to return in the case of no label attribute: `"na"` returns missing values
#' and `"names"` uses the column names
#' @returns character vector of labels
#' @export
#' @examples
#' d <- example_lbl_tbl()
#' get_labels(d)
#' get_labels(d, missing_label = "na")
get_labels <- function(x, missing_label = c("names", "na")) {
  missing_label <- rlang::arg_match(missing_label)
  if (!inherits(x, "lbl_tbl")) {
    rlang::abort("x must be a `lbl_tbl` object; use `as_lbl_tbl(x)` to convert")
  }
  out <-
    lapply(x, get_label, missing_label = "na") |>
    setNames(names(x))
  if (missing_label == "names") {
    out[is.na(out)] <- names(out)[is.na(out)]
  }
  return(out)
}

#' get metadata
#'
#' Get table-specific metadata associated with a labeled table (`lbl_tbl`) object.
#' @param x a `lbl_tbl` object (see `as_lbl_tbl()`)
#' @param which character vector of which metadata values to return
#' @returns a list of table-specific metadata
#' @export
#' @examples
#' d_md <- get_md(example_lbl_tbl())
#' d_md[c("name", "title")]
#' str(d_md)
get_md <- function(x, which = c("name", "title", "version", "description", "created")) {
  if (!inherits(x, "lbl_tbl")) rlang::abort("x must be a `lbl_tbl` object")
  attributes(x)[which]
}

# TODO as_readme.lbl_df
# TODO as_datapackage.lbl_df
