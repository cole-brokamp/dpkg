.onLoad <- function(...) {
  S7::methods_register()
  fs::dir_create(stow_path())
}

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

has_internet <- function() {
  !is.null(curl::nslookup("captive.apple.com", error = FALSE))
}
