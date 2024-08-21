test_that("write_dpkg() works", {
  d <- as_dpkg(mtcars, version = "0.1.0", title = "Motor Trend Road Car Tests")
  attr(d, "description") <- "This is a data set all about characteristics of different cars"
  attr(d, "homepage") <- "https://github.com/cole-brokamp/dpkg"

  out <- write_dpkg(d, dir = tempdir())
  expect_true(fs::file_exists(out))

  test_read <- arrow::read_parquet(out)
  expect_s3_class(test_read, "tbl")
  expect_identical(nrow(test_read), 32L)
  expect_identical(ncol(test_read), 11L)
})
