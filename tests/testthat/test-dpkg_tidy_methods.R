test_that("extract dpkg", {
  x <- as_dpkg(mtcars, name = "mtcars", title = "Motor Trend Road Car Tests")
  attr(x, "description") <- "This is a data set all about characteristics of different cars"
  attr(x, "homepage") <- "https://github.com/cole-brokamp/dpkg"
  x[1] |>
    expect_s3_class("dpkg")
})

test_that("using dplyr verbs with dpkg objects do not error", {
  x <- as_dpkg(mtcars, name = "mtcars", title = "Motor Trend Road Car Tests")
  attr(x, "description")<- "This is a data set all about characteristics of different cars"
  attr(x, "homepage")<- "https://github.com/cole-brokamp/dpkg"
  dplyr::left_join(x, mtcars, by = "vs", relationship = "many-to-many") |>
    expect_no_error()
  dplyr::left_join(mtcars, x, by = "vs", relationship = "many-to-many") |>
    expect_no_error()
  dplyr::left_join(x, x, by = "vs", relationship = "many-to-many") |>
    expect_no_error()
  dplyr::summarize(x, n = dplyr::n()) |>
    expect_no_error()

  dplyr::filter(x, mpg < 20) |>
    expect_no_error()
  dplyr::mutate(x, year = 2022) |>
    expect_no_error()
  dplyr::rename(x, new_name = mpg) |>
    expect_no_error()
  dplyr::select(x, mpg) |>
    expect_no_error()
  dplyr::arrange(x, mpg) |>
    expect_no_error()
  dplyr::slice(x, 1) |>
    expect_no_error()

})
