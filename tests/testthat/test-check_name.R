test_that("checking name property works", {
  skip("until checker functions are back in")

  as_dpkg(mtcars, name = "mtcars", version = "0.a1.2") |>
    expect_error("invalid version specification")

  as_dpkg(mtcars, name = c("my", "cars")) |>
    expect_error("@name must be length 1")

  as_dpkg(mtcars, name = 1) |>
    expect_error("@name must be <character>")

  as_dpkg(mtcars, name = "Mtcars") |>
    expect_error("@name must be all lowercase")

  as_dpkg(mtcars, name = "my mtcars") |>
    expect_error("@name must only contain alphanumeric")

  as_dpkg(mtcars, name = "<foofy>") |>
    expect_error("@name must only contain alphanumeric")
  
  prop_label$validator("the_name") |>
    expect_null()

  prop_label$validator(c("a", "b")) |>
    expect_identical("must be length 1")

  prop_label_maybe$validator("the_name") |>
    expect_null()

  prop_label_maybe$validator(character()) |>
    expect_null()

  prop_label_maybe$validator(c("a", "b")) |>
    expect_identical("must be length 1 (or 0)")

})
