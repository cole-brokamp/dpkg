#' download a file to the `stow` R user directory
#' 
#' Downloaded files are available across all of an R user’s sessions and projects.
#' If a file with the same name has already been downloaded once,
#' it will not be re-downloaded again, unless `force = TRUE`.
#' Specify an alternative download location by setting the `R_USER_DATA_DIR`
#' environment variable; see `?tools::R_user_dir`.
#' @param x a URL starting with `http://` or `https://`
#' @param overwrite logical; overwrite stowed file with the same name?
#' @examples
#' Sys.getenv("R_USER_DATA_DIR")
#' Sys.setenv(R_USER_DATA_DIR = tempfile("stow"))
#' stow("https://github.com/geomarker-io/appc/releases/download/v0.1.0/nei_2020.rds")
#' stow_path("nei_2020.rds")
#' stow_exists("nei_2020.rds")
#' })
stow <- function(url, overwrite = FALSE) {
  if (!grepl("^https?://", url)) rlang::abort("x must start with `http://` or `https://`")
  fs::dir_create(stow_path())
  dest_path <- stow_path(fs::path_file(url))
  httr2::req_perform(httr2::request(url), path = dest_path)
  return(dest_path)
}

#' get the path to where files are stowed
#'
#' @param filename optional; character filename to use in path
#' @returns path to stowed file; if filename is NULL, then the path
#' to the directory where files are stowed
#' @export
#' @examples
#' 
#' stow_path()
#' 
#' stow_path("foofy.rds")
#' 
#' local({
#'   withr::local_envvar(R_USER_DATA_DIR = tempfile("stow"))
#'   stow_path()
#' })
#' 
stow_path <- function(filename = NULL) {
  the_path <- fs::path(tools::R_user_dir("stow", "data"))
  if (!is.null(filename)) the_path <- fs::path(the_path, filename)
  return(the_path)
}

#' get info about stowed files
#'
#' @export
stow_info <- function() {}

#' stow exists
#' @rdname stow_info
#' @export
stow_exists <- function(filename) {
  stats::setNames(fs::file_exists(stow_path(filename)), NULL)
}

## stow_remove

## stow_clear

## stow_size

