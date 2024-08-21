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
#' attr(x, "description") <- "This is a data set all about characteristics of different cars"
#' attr(x, "homepage") <- "https://github.com/cole-brokamp/dpkg"
#' x
as_dpkg <- function(x, name = deparse(substitute(x)), version = "0.0.0.9000",
                    title = character(), homepage = character(), description = character()) {
  invisible(check_label(name, "name", required = TRUE))
  invisible(check_label(version, "version", required = TRUE))
  if (!is.package_version(as.package_version(version))) {
    rlang::abort("version should be coercable with `as.package_version()`")
  } else if (!identical(tolower(name), name)) {
    rlang::abort("name must be all lowercase")
  } else if (grepl("[^a-zA-Z0-9._-]", name)) {
    rlang::abort("name must only contain alphanumeric characters, except for `-`, `_`, and `.`")
  } else if (length(homepage) == 1 && !grepl("^((http|ftp)s?|sftp)://", homepage)) {
    rlang::abort("homepage must be a valid http, https, or sftp URL")
  }
  tibble::new_tibble(tibble::validate_tibble(x),
    class = "dpkg",
    name = check_label(name, "name", required = TRUE),
    version = check_label(version, "version", required = TRUE),
    title = check_label(title, "title", required = FALSE),
    homepage = check_label(homepage, "homepage", required = FALSE),
    description = check_label(description, "description", required = FALSE)
  )
}

#' @export
print.dpkg <- function(x, ...) {
  cli::cli_text("# [{cli::symbol$menu}] {attr(x, 'name')} v{attr(x, 'version')}")
  cli::cli_text("# {cli::symbol$info} Use `dpkg_meta() to get all metadata")
  print(tibble::as_tibble(x), ...)
}

#' get the metadata associated with a data package
#'
#' @param x a dpkg object
#' @returns a list of metadata key value pairs
#' @export
dpkg_meta <- function(x) {
  attributes(x)[c("name", "version", "title", "homepage", "description")]
}

check_label <- function(x, label_name, required = FALSE) {
  if (!is.character(x)) {
    rlang::abort(glue::glue("`{label_name}` must be <character>, not <{class(x)}>"))
  } else if (required && length(x) != 1) {
    rlang::abort(glue::glue("`{label_name}` must be length 1"))
  } else if (length(x) > 1) {
    rlang::abort(glue::glue("{label_name} must be length 1 (or 0)"))
  }
  return(x)
}
