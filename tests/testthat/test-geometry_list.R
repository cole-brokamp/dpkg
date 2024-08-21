test_that("dpkg works with geometry list columns", {
  sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf")) |>
    as_dpkg(name = "nc_data") |>
    write_dpkg(tempdir())
  d <- read_dpkg(fs::path_temp("nc_data-v0.0.0.9000.parquet"))
  expect_s3_class(d$geom, "geoarrow_vctr")
})
