test_that("as_dpkg() works", {
  x <- as_dpkg(mtcars, name = "mtcars", title = "Motor Trend Road Car Tests")
  attr(x, "description")<- "This is a data set all about characteristics of different cars"
  attr(x, "homepage") <- "https://github.com/cole-brokamp/dpkg"

  expect_s3_class(x, "dpkg")

  dpkg_meta(x) |>
    expect_identical(
      list(
        name = "mtcars", version = "0.0.0.9000", title = "Motor Trend Road Car Tests",
        homepage = "https://github.com/cole-brokamp/dpkg", description = "This is a data set all about characteristics of different cars"
      )
    )

  x[1, ] |>
    expect_s3_class("dpkg")

  x[1, ] |>
    dpkg_meta() |>
    expect_identical(list(
      name = "mtcars", version = "0.0.0.9000", title = "Motor Trend Road Car Tests",
      homepage = "https://github.com/cole-brokamp/dpkg", description = "This is a data set all about characteristics of different cars"
    ))

  expect_snapshot(x[1, ])
})
