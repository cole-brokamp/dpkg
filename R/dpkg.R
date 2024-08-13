#' Use a data.frame and metadata to create a data package
#'
#' Convert a data frame into a data package (`dpkg`) by providing specific metadata in the arguments.
#'
#' `name` should be specified, but if is not will be deparsed from code defining `x`;
#' this might not result in a valid `name` (e.g., when piping code to create a data frame)
#' @param x a tibble or data frame
#' @param name a lowercase character string consisting of only
#' `a-z`, `0-9`, `-`, `_`, or `.` to be used as a data package identifier
#' @param version a character string representing a
#' [semantic version](https://datapackage.org/recipes/data-package-version/) (e.g., "0.2.1")
#' @param title a character string that is a title of the data package for humans
#' @param homepage a valid URL that links to a webpage with code or descriptions related to creation of the data package
#' @param description a character string (markdown encouraged!) of more details
#' about how the data was created, including the data sources,
#' references to code or packages used, relevant details for any
#' specific columns, and notes about (mis)usage of the data
#' @returns a dpkg object
#' @export
#' @examples
#'
#' x <- as_dpkg(mtcars, name = "mtcars", title = "Motor Trend Road Car Tests")
#' x@description <- "This is a data set all about characteristics of different cars"
#' x@homepage <- "https://github.com/cole-brokamp/dpkg"
#' x
as_dpkg <-
  function(x, name = deparse(substitute(x)), version = "0.0.0.9000",
           title = character(), homepage = character(), description = character()) {
    new_dpkg(
      x,
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

new_dpkg <- S7::new_class(
  name = "dpkg",
  parent = S7::class_data.frame,
  package = "dpkg",
  properties = list(
    name = prop_label,
    version = prop_label,
    title = prop_label_maybe,
    homepage = prop_label_maybe,
    description = prop_label_maybe
  ),
  validator = function(self) {
    if (!is.package_version(as.package_version(self@version))) {
      "@version should be coercable with `as.package_version()`"
    } else if (!identical(tolower(self@name), self@name)) {
      "@name must be all lowercase"
    } else if (grepl("[^a-zA-Z0-9._-]", self@name)) {
      "@name must only contain alphanumeric characters, except for `-`, `_`, and `.`"
    } else if (length(self@homepage) == 1 && !grepl("^((http|ftp)s?|sftp)://", self@homepage)) {
      "@homepage must be a valid http, https, or sftp URL"
    }
  }
)

S7::method(print, new_dpkg) <- function(x, ...) {
  cli::cli_text("# [{cli::symbol$menu}] {x@name} v{x@version}")
  cli::cli_text("# {cli::symbol$info} Use `dpkg_meta() to get all metadata")
  print(tibble::as_tibble(x), ...)
}

# extract
S7::method(`[`, new_dpkg) <- function(x, i, ...) {
  d <- as.data.frame(S7::S7_data(x))
  do.call(as_dpkg, c(list(d[i, ]), dpkg_meta(x)))
}

#' get the metadata associated with a data package
#'
#' @param x a dpkg object
#' @returns a list of metadata key value pairs
#' @export
dpkg_meta <- function(x) {
  S7::props(x)
}
