test_that("write_dpkg() works", {
  d <- as_dpkg(mtcars, version = "0.1.0", title = "Motor Trend Road Car Tests")
  d@description <- "This is a data set all about characteristics of different cars"
  d@homepage <- "https://github.com/cole-brokamp/dpkg"

  out <- write_dpkg(d, dir = tempdir())
  expect_true(fs::file_exists(out))

  test_read <- nanoparquet::read_parquet(out)
  expect_s3_class(test_read, "tbl")
  expect_identical(nrow(test_read), 32L)
  expect_identical(ncol(test_read), 11L)

  checking_md <- nanoparquet::parquet_metadata(out)$file_meta_data$key_value_metadata[[1]]
  expect_true(all(c("name", "version", "title", "homepage", "description", "created", "rlang_hash") %in% checking_md$key))
})
