#' read data package from an online location
## dpkg <- function()

#' read metadata from dpkg on disk
#' @param x path to data package (`.parquet` file) on disk
#' @returns a list of metadata collected from parquet_info and the key value metadata
#' @export
read_dpkg_metadata <- function(x) {
  pmd <-
    nanoparquet::parquet_metadata(x)$file_meta_data$key_value_metadata[[1]] |>
    tibble::deframe() |>
    as.list()
  pmd$`ARROW:schema` <- NULL
  pmd$created <- as.POSIXct(as.character(pmd$created))
  pmd$version <- pmd$version
  pmd$columns_rtype <-
    nanoparquet::parquet_column_types(x)[, c("name", "r_type")] |>
    tibble::deframe()
  pmd$file_size <- nanoparquet::parquet_info(x)$file_size
  pmd$file_size <- prettyunits::pretty_bytes(pmd$file_size)
  pmd$num_rows <- nanoparquet::parquet_info(x)$num_rows
  pmd$num_cols <- nanoparquet::parquet_info(x)$num_cols
  return(pmd)
}

#' read dpkg from disk
#'
#' @param x path to data package (`.parquet` file) on disk
#' @returns a dpkg object
#' @export
read_dpkg <- function(x) {
  x_md <- read_dpkg_metadata(x)
  new_dpkg(
    nanoparquet::read_parquet(x),
    name = x_md$name,
    version = x_md$version,
    title = x_md$title,
    homepage = x_md$homepage,
    description = x_md$description
  )
}

#' write dpkg to disk
#'
#' @param x a data package (`dpkg`) object
#' @param dir path to directory where dpkg parquet file will be written
#' @returns path to the written file, invisibly
write_dpkg <- function(x, dir) {
  if (!inherits(x, "dpkg::dpkg")) rlang::abort("x must be a `dpkg` object`")
  out_path <- fs::path(dir, glue::glue("{x@name}_{x@version}"), ext = "parquet")
  out_md <- c(
    unlist(dpkg_meta(x)),
    created = as.character(Sys.time()),
    hash = rlang::hash(as.data.frame(S7::S7_data(x)))
  )
  nanoparquet::write_parquet(x, out_path, metadata = out_md)
  return(invisible(out_path))
}
