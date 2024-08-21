#' @rdname read_dpkg
#' @export
read_dpkg_metadata <- function(x) {
  x_a <- arrow::open_dataset(x)
  if (length(x_a$metadata$r$attributes$class) == 0) {
    rlang::abort("parquet file does not appear to contain an R object")
  }
  if (!"dpkg" %in% x_a$metadata$r$attributes$class) {
    rlang::abort("R object in the parquet file must be class 'dpkg'")
  }
  out <- x_a$metadata$r$attributes[c("name", "version", "title", "homepage", "description", "hash", "created")]
  out$created <- as.POSIXct(as.character(out$created))
  out$num_rows <- x_a$num_rows
  out$num_cols <- x_a$num_cols
  out$fields <- x_a$schema$names
  out$file_size <- prettyunits::pretty_bytes(file.size(x))
  return(out)
}

#' read (meta)data from dpkg on disk
#'
#' @param x path to data package (`.parquet` file) on disk
#' @returns for `read_dpkg()`, a dpkg object; for `read_dpkg_metadata()`, a list of metadata
#' @export
#' @examples
#'
#' d <- as_dpkg(mtcars, version = "0.1.0", title = "Motor Trend Road Car Tests")
#' attr(d, "description") <- "This is a data set all about characteristics of different cars"
#' attr(d, "homepage") <- "https://github.com/cole-brokamp/dpkg"
#'
#' write_dpkg(d, dir = tempdir()) |>
#'   read_dpkg()
#'
#' # geo objects are supported via the `geoarrow_vctr` in the geoarrow package
#' library(geoarrow)
#' sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf")) |>
#'   as_dpkg(name = "nc_data") |>
#'   write_dpkg(tempdir())
#' d <- read_dpkg(fs::path_temp("nc_data-v0.0.0.9000.parquet"))
#' d
#'
#' # as a simple features object
#' d$geom <- sf::st_as_sfc(d$geom)
#' sf::st_as_sf(d)
read_dpkg <- function(x) {
  invisible(read_dpkg_metadata(x))
  out <- arrow::read_parquet(x)
  return(out)
}

#' write dpkg to disk
#'
#' @param x a data package (`dpkg`) object
#' @param dir path to directory where dpkg parquet file will be written
#' @returns path to the written file, invisibly
#' @export
write_dpkg <- function(x, dir) {
  if (!inherits(x, "dpkg")) rlang::abort("x must be a `dpkg` object`")
  out_path <- fs::path(dir, glue::glue("{attr(x, 'name')}-v{attr(x, 'version')}"), ext = "parquet")
  attr(x, "hash") <- rlang::hash(x)
  attr(x, "created") <- as.character(Sys.time())
  if ("sfc" %in% unlist(sapply(x, class))) {
    rlang::check_installed("geoarrow", "to write geographic data in parquet files.")
    requireNamespace("geoarrow")
  }
  arrow::write_parquet(x, out_path)
  return(invisible(out_path))
}
