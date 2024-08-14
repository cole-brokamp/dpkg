test_that("checking name property works", {

  as_dpkg(mtcars, name = "mtcars", version = "0.a1.2") |>
    expect_error("invalid version specification")

  as_dpkg(mtcars, name = c("my", "cars")) |>
    expect_error("`name` must be length 1")

  as_dpkg(mtcars, name = 1) |>
    expect_error("`name` must be <character>, not <numeric>")

  as_dpkg(mtcars, name = "Mtcars") |>
    expect_error("name must be all lowercase")

  as_dpkg(mtcars, name = "my mtcars") |>
    expect_error("name must only contain alphanumeric")

  as_dpkg(mtcars, name = "<foofy>") |>
    expect_error("name must only contain alphanumeric")
  
  check_label("my_value", "the_name") |>
    expect_identical("my_value")

  check_label(character(), "the_name", required = TRUE) |>
    expect_error("`the_name` must be length 1")

  check_label(c("a", "b"), "the_thing", required = TRUE) |>
    expect_error("`the_thing` must be length 1")

})
