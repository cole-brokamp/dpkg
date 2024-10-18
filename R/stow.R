#' stow a remote file locally, caching it for later use
#'
#' Use stow to abstract away the process of downloading a file
#' or a GitHub release asset to a user's data directory, only
#' downloading files that have not already been downloaded.
#'
#' Supported URI prefixes include:
#'
#' - `https://`, `http://`: download from a file
#' - `gh://`: download a github release asset, formatted as `gh://owner/repo/name`
#'
#' Stow downloads files to the users data directory; see `?tools::R_user_dir`.
#' Specify an alternative download location by setting the `R_USER_DATA_DIR`
#' environment variable.
#' The stow cache works by name only; that is, if a file with the same URI
#' has already been downloaded once, it will not be re-downloaded again
#' (unless `overwrite = TRUE`).
#' @param uri character string universal resource identifier; currently, must begin
#' with `http://`, `https://`, or `gh://`
#' @param overwrite logical; re-download the remote file even though
#' a local file with the same name exists?
#' @returns path to the stowed file or url to github release
#' @export
#' @examples
#' Sys.setenv(R_USER_DATA_DIR = tempfile("stow"))
#' # get by using URL
#' stow("https://github.com/geomarker-io/appc/releases/download/v0.1.0/nei_2020.rds",
#'      overwrite = TRUE) |>
#'   readRDS()
#'
#' # will be faster (even in later R sessions) next time
#' stow("https://github.com/geomarker-io/appc/releases/download/v0.1.0/nei_2020.rds") |>
#'   readRDS()
#'
#' # get a data package from a GitHub release
#' stow("gh://cole-brokamp/dpkg/mtcars-v0.0.0.9000", overwrite = TRUE) |>
#'   arrow::read_parquet()
#' 
#' stow("gh://cole-brokamp/dpkg/mtcars-v0.0.0.9000") |>
#'   arrow::read_parquet()
#' 
stow <- function(uri, overwrite = FALSE) {
  if (grepl("^https?://", uri)) {
    out <- stow_url(url = uri, overwrite = overwrite)
    return(out)
  }
  if (grepl("^gh://", uri)) {
    uri_parts <-
      strsplit(uri, "/", fixed = TRUE)[[1]][3:5] |>
      as.list() |>
      stats::setNames(c("owner", "repo", "dpkg"))
    out <-
      stow_gh_release(uri_parts$owner,
        repo = uri_parts$repo,
        dpkg = uri_parts$dpkg,
        overwrite = overwrite
      )
    return(out)
  }
  rlang::abort("uri must begin with `https://`, or `http://`, or `gh://`")
}

#' download a file to the `stow` R user directory
#'
#' @rdname stow
#' @param url a URL string starting with `http://` or `https://`
#' @export
stow_url <- function(url, overwrite = FALSE) {
  if (!grepl("^https?://", url)) rlang::abort("x must start with `http://` or `https://`")
  dest_path <- stow_path(fs::path_file(url))
  if (fs::file_exists(dest_path) && !overwrite) {
    return(dest_path)
  }
  httr2::req_perform(httr2::request(url), path = dest_path)
  return(dest_path)
}

#' get info about stowed files
#'
#' @param filename character filename of stowed file; if NULL, then information about
#' *all* stowed files or the directory where files are stowed is returned
#' @param .delete_stow_dir_confirm set to TRUE in order to delete the entire stow directory without interactive
#' user confirmation
#' @returns for `stow_info()`, a tibble of file or folder information;
#' for `stow_path()`, a character path to the stowed file or stow directory;
#' for `stow_exists()`, a logical;
#' for `stow_size()`, a fs::
#' @export
#' @examples
#' Sys.setenv(R_USER_DATA_DIR = tempfile("stow"))
#'
#' stow_path()
#'
#' stow("https://github.com/geomarker-io/appc/releases/download/v0.1.0/nei_2020.rds")
#'
#' stow_path("nei_2020.rds")
#'
#' stow_exists("nei_2020.rds")
#'
#' stow_size("nei_2020.rds")
#'
#' stow("https://github.com/geomarker-io/appc/releases/download/v0.1.0/nei_2017.rds")
#'
#' stow_info("nei_2017.rds")
#'
#' stow_info()
#'
#' stow_size()
#'
#' stow_remove(.delete_stow_dir_confirm = TRUE)
stow_info <- function(filename = NULL) {
  if (!stow_exists(filename)) rlang::abort("file or folder does not exist")
  if (is.null(filename)) {
    return(fs::dir_info(stow_path()))
  }
  return(fs::file_info(stow_path(filename)))
}

#' get the path to a stowed file (or the stow directory)
#' @rdname stow_info
#' @export
stow_path <- function(filename = NULL) {
  the_path <- fs::path(tools::R_user_dir("stow", "data"))
  fs::dir_create(the_path)
  if (!is.null(filename)) the_path <- fs::path(the_path, filename)
  return(the_path)
}

#' test if a stowed file (or the stow directory) exists
#' @rdname stow_info
#' @export
stow_exists <- function(filename = NULL) {
  fs::file_exists(stow_path(filename)) |>
    stats::setNames(NULL)
}

#' get the size of a stowed file
#' @rdname stow_info
#' @export
stow_size <- function(filename = NULL) {
  sum(stow_info(filename)$size)
}

#' remove a stowed file (or the stow entire directory)
#' @rdname stow_info
#' @export
stow_remove <- function(filename = NULL, .delete_stow_dir_confirm = FALSE) {
  if (is.null(filename)) {
    message(stow_path(), " has a total size of ", stow_size())
    if (!.delete_stow_dir_confirm) answer <- utils::askYesNo("Are you sure you want to delete the entire stow directory?")
    if (.delete_stow_dir_confirm) answer <- TRUE
    if (answer) fs::dir_delete(stow_path())
    return(invisible(NULL))
  }
  fs::file_delete(stow_path(filename))
  return(invisible(NULL))
}
