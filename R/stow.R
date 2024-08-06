#' download a file to the `stow` R user directory
#'
#' Downloaded files are available across all of an R user’s sessions and projects.
#' If a file with the same name has already been downloaded once,
#' it will not be re-downloaded again, unless `force = TRUE`.
#' Specify an alternative download location by setting the `R_USER_DATA_DIR`
#' environment variable; see `?tools::R_user_dir`.
#' @param url a URL string starting with `http://` or `https://`
#' @param overwrite logical; overwrite stowed file with the same name?
#' @export
#' @examples
#' Sys.setenv(R_USER_DATA_DIR = tempfile("stow"))
#' stow("https://github.com/geomarker-io/appc/releases/download/v0.1.0/nei_2020.rds")
stow <- function(url, overwrite = FALSE) {
  if (!grepl("^https?://", url)) rlang::abort("x must start with `http://` or `https://`")
  fs::dir_create(stow_path())
  dest_path <- stow_path(fs::path_file(url))
  httr2::req_perform(httr2::request(url), path = dest_path)
  return(dest_path)
}

#' get info about stowed files
#'
#' @param filename character filename of stowed file; if NULL, then information about
#' *all* stowed files or the directory where files are stowed is returned
#' @returns for `stow_info()`, a tibble of file or folder information;
#' for `stow_path()`, a character path to the stowed file or stow directory;
#' for `stow_exists()`, a logical;
#' for `stow_size()`, a fs::
#' @export
#' @examples
#' Sys.setenv(R_USER_DATA_DIR = tempfile("stow"))
#' stow_path()
#' stow("https://github.com/geomarker-io/appc/releases/download/v0.1.0/nei_2020.rds")
#' stow_path("nei_2020.rds")
#' stow_exists("nei_2020.rds")
#' stow_size("nei_2020.rds")
#'
#' stow("https://github.com/geomarker-io/appc/releases/download/v0.1.0/nei_2017.rds")
#' stow_info("nei_2017.rds")
#' stow_info()
#' stow_size()
#' stow_remove()
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
stow_remove <- function(filename = NULL) {
  if (is.null(filename)) {
    message(stow_path(), " has a total size of ", stow_size())
    answer <- utils::askYesNo("Are you sure you want to delete the entire stow directory?")
    if (answer) fs::dir_delete(stow_path())
    return(invisible(NULL))
  }
  fs::file_delete(stow_path(filename))
  return(invisible(NULL))
}
