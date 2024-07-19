
# TODO as_readme.lbl_df
# TODO as_datapackage.lbl_df

# convert a data frame or tibble into a labeled table (`lbl_tbl`) object
#' @param x a tibble or data frame
#' @param strict logical; require exactly one label for every column in x?
as_lbl_df <- function(x, ...) {
  dots <- rlang::dots_list(..., .homonyms = "error", .ignore_empty = "all", .check_assign = TRUE)
  x_tbl <- tibble::as_tibble(x)
  x_tbl <- labelVector::set_label(x_tbl, .dots = as.list(dots$schema))
  out <- list(x = x_tbl, class = c("lbl_df", "tbl"))
  out$name <- as.character(dots$name %||% deparse(substitute(x)))
  out$title <- as.character(dots$title)
  out$version <- as.numeric_version(dots$version)
  out$description <- as.character(dots$description)
  out$created <- as.POSIXct(dots$created %||% Sys.time())
  do.call(vctrs::new_data_frame, out)
}

library(pillar)

  ## labelVector:::set_label.default

  ## labelVector:::set_label.data.frame

## simple example
as_lbl_df(mtcars, name = "mtcars", title = "Motor Trend Road Car Tests")

## complex example
lbl_md <-
  list(
    name = "mtcars",
    title = "Motor Trend Road Car Tests",
    version = "0.1.0",
    schema = c(mpg = "Miles Per Gallon", gear = "Number of Gears")
    )

d <- as_lbl_df(mtcars, !!!lbl_md)

get_labels <- function(x) setNames(labelVector::get_label(x), names(x))
get_md <- function(x) attributes(x)[c("name", "title", "version", "description", "created")]

get_labels(d)

get_md(d)

library(pillar)

tbl_format_setup.lbl_df <- function(x, width, ...) {
  setup <- NextMethod()
  setup <- modifyList(setup, attributes(x), keep.null = FALSE)
  setup
}

tbl_format_header.lbl_df <- function(x, setup, ...) {
  default_header <- NextMethod()
  id_header <- pillar::style_subtle(paste0("# ", cli::symbol$menu, " ", setup$name, " v", setup$version))
  extra_header <-
    pillar::style_subtle(
      paste0("# ", cli::symbol$em_dash, " title: ",
             cli::symbol$dquote_left, setup$title, cli::symbol$dquote_right))
  c(id_header, default_header, extra_header)
}

tbl_sum.lbl_df <- function(x, ...) {
  c("A labeled data frame" = dim_desc(x))
}

tbl_format_footer.lbl_df <- function(x, ...) {
  default_footer <- NextMethod()
  extra_info <- c("Use `lbl::get_labels()` to get column-specific labels",
                  "Use `lbl::get_md()` to get table-specific metadata")
  extra_footer <- pillar::style_subtle(paste0("# ", cli::symbol$info, " ", extra_info))
  c(default_footer, extra_footer)
}

