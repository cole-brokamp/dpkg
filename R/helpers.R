.onLoad <- function(...) {
  S7::methods_register()
}

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

check_name <- function(name) {
  # name is a character string
  if (!is.character(name)) {
    return("`name` must be a character string")
  }
  # name does not have uppercase letters
  if (stringr::str_detect(name, "[[:upper:]]")) {
    return("`name` must be all lowercase")
  }
  # name does not have spaces
  if (stringr::str_detect(name, " ")) {
    return("`name` must not contain spaces")
  }
  # nonalphanumeric characters are either -, _, or .
  if (!all(stringr::str_detect(unlist(stringr::str_extract_all(name, "[^[:alnum:]]")), "[_.-]"))) {
    return("`name` must only contain a-z, 0-9, -, _, .")
  }
  return(invisible(NULL))
}
