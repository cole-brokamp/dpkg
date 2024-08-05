test_that("check_name works", {
  expect_no_condition(check_name("codec_traffic"))
  check_name("CoDEC_traffic") |>
    expect_equal("`name` must be all lowercase")
  check_name(1) |>
    expect_equal("`name` must be a character string")
  check_name("census data") |>
    expect_equal("`name` must not contain spaces")
  check_name("census+data") |>
    expect_equal("`name` must only contain a-z, 0-9, -, _, .")
})
