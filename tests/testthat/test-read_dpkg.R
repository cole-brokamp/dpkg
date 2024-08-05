test_that("read_dpkg() and read_dpkg_metadata() works", {
  d <- as_dpkg(mtcars, version = "0.1.0", title = "Motor Trend Road Car Tests")
  d@description <- "This is a data set all about characteristics of different cars"
  d@homepage <- "https://github.com/cole-brokamp/dpkg"

  out <- write_dpkg(d, dir = tempdir())

  d_in <- read_dpkg(out)

  expect_s3_class(d_in, "dpkg::dpkg")

  expect_equal(d, d_in, ignore_attr = TRUE)

  expect_identical(nrow(d_in), 32L)
  expect_identical(ncol(d_in), 11L)

  the_md <- read_dpkg_metadata(out)
  the_md$created <- NULL
  the_md |>
    expect_identical(list(
      name = "mtcars", version = "0.1.0", title = "Motor Trend Road Car Tests",
      homepage = "https://github.com/cole-brokamp/dpkg",
      description = "This is a data set all about characteristics of different cars",
      rlang_hash = "0fe2d26c242a70a56f22d71838d27685",
      columns_rtype = c(
        mpg = "double", cyl = "double", disp = "double",
        hp = "double", drat = "double", wt = "double", qsec = "double",
        vs = "double", am = "double", gear = "double", carb = "double"
      ), file_size = "2.99 kB", num_rows = 32, num_cols = 11L
    ))
})
