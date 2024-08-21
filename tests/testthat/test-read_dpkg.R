test_that("read_dpkg() and read_dpkg_metadata() works", {
  d <- as_dpkg(mtcars, version = "0.1.0", title = "Motor Trend Road Car Tests")
  attr(d, "description") <- "This is a data set all about characteristics of different cars"
  attr(d, "homepage") <- "https://github.com/cole-brokamp/dpkg"

  out <- write_dpkg(d, dir = tempdir())

  d_in <- read_dpkg(out)

  expect_s3_class(d_in, "dpkg")

  expect_equal(d, d_in, ignore_attr = TRUE)

  expect_identical(nrow(d_in), 32L)
  expect_identical(ncol(d_in), 11L)

  the_md <- read_dpkg_metadata(out)
  expect_true(the_md$created < Sys.time())
  the_md$created <- NULL
  expect_true(as.numeric(the_md$file_size) - 7424 < 11)
  the_md$file_size <- NULL
  the_md |>
    expect_identical(
      list(
        name = "mtcars", version = "0.1.0",
        title = "Motor Trend Road Car Tests",
        homepage = "https://github.com/cole-brokamp/dpkg",
        description = "This is a data set all about characteristics of different cars",
        hash = "502c9e71f62f79debf7e769e071df0b4",
        num_rows = 32L,
        num_cols = 11L,
        fields = c(
          "mpg", "cyl", "disp", "hp", "drat",
          "wt", "qsec", "vs", "am", "gear", "carb"
        )
      )
    )
})
