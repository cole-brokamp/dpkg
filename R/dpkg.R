#' Use a data.frame and metadata to create a data package
#'
#' Convert a data frame into a data package (`dpkg`) by providing specific metadata in the arguments.
#'
#' `name` should be specified, but if is not will be deparsed from code defining `x`;
#' this might not result in a valid `name` (e.g., when piping code to create a data frame)
#' @param x a tibble or data frame
#' @param name a lowercase character string consisting of only `a-z`, `0-9`, `-`, `_`, or `.` to be used as a data package identifier
#' @param version a character string representing a [semantic version](https://datapackage.org/recipes/data-package-version/) (e.g., "0.2.1")
#' @param title a character string that is a title of the data package for humans
#' @param homepage a valid URL that links to a webpage with code or descriptions related to creation of the data package
#' @param description a character string (markdown encouraged!) of more details about how the data was created, including the data sources,
#' references to code or packages used, relevant details for any specific columns, and notes about (mis)usage of the data
#' @returns a dpkg object
#' @export
#' @examples
#' as_dpkg(mtcars, name = "mtcars", title = "Motor Trend Road Car Tests")
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
as_dpkg <- function(x, name = deparse(substitute(x)), version = "0.0.0.9000", title = NULL, homepage = NULL, description = NULL) {
  out$name <- as.character(dots$name %||% deparse(substitute(x)))
  new_dpkg(
    x = x,
    name = name,
    version = version,
    title = title,
    homepage = homepage,
    description = description
  )
}

prop_label <- S7::new_property(
  class = S7::class_character,
  validator = function(value) {
    if (length(value) != 1L) "must be length 1"
  }
)

prop_label_maybe <- S7::new_property(
  class = S7::class_character,
  validator = function(value) {
    if (length(value) > 1L) "must be length 1 (or 0)"
  }
)

prop_name <- S7::new_property(
  class = S7::class_character,
  validator = function(value) {
    if (length(value) != 1L) "must be length 1"
    if (!identical(tolower(value), value)) "name must be all lowercase"
    if (grepl("[^a-zA-Z0-9._-]", value)) "name must only contain alphanumeric characters, except for `-`, `_`, and `.`"
  }
)

new_dpkg <- S7::new_class(
  name = "dpkg",
  parent = S7::class_data.frame,
  package = "dpkg",
  properties = list(
    name = prop_name,
    version = prop_label,
    title = prop_label_maybe,
    homepage = prop_label_maybe,
    description = prop_label_maybe
  ),
  validator = function(self) {
    if (length(self@homepage) == 1 && !grepl("^((http|ftp)s?|sftp)://", self@homepage)) {
      "homepage must be a valid http, https, or sftp URL"
    }
    if (length(self@version) != 1) "version must be length 1"
    if (!is.package_version(as.package_version(self@version))) "version should be coercable with `as.package_version()`"
    check_name(self@name)
  }
)
