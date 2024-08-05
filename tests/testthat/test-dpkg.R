test_that("as_dpkg() works", {
  x <- as_dpkg(mtcars, name = "mtcars", title = "Motor Trend Road Car Tests")
  x@description <- "This is a data set all about characteristics of different cars"
  x@homepage <- "https://github.com/cole-brokamp/dpkg"

  expect_s3_class(x, "dpkg::dpkg")

  S7::props(x) |>
    expect_identical(
      list(
        name = "mtcars", version = "0.0.0.9000", title = "Motor Trend Road Car Tests",
        homepage = "https://github.com/cole-brokamp/dpkg", description = "This is a data set all about characteristics of different cars"
      )
    )

  dpkg_meta(x) |>
    expect_identical(
      list(
        name = "mtcars", version = "0.0.0.9000", title = "Motor Trend Road Car Tests",
        homepage = "https://github.com/cole-brokamp/dpkg", description = "This is a data set all about characteristics of different cars"
      )
    )

  x[1, ] |>
    expect_s3_class("dpkg::dpkg")
  
  expect_snapshot(x[1, ])

})
