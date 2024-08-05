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
  pmd$created <- as.POSIXct(as.numeric(pmd$created))
  pmd$`CoDEC:version` <- as.package_version(pmd$`CoDEC:version`)
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
    name = x_md$`CoDEC:name`,
    version = as.character(x_md$`CoDEC:version`),
    title = x_md$`CoDEC:title`,
    homepage = x_md$`CoDEC:homepage` %||% character()
  )
}

#' write dpkg to disk
write_dpkg <- function(x) {

}
