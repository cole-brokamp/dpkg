test_that("stow_info() and friends work", {
  withr::local_envvar(
    R_USER_DATA_DIR = tempfile("stow"),
    R_DEFAULT_INTERNET_TIMEOUT = 360
  )

  expect_identical(fs::path_file(stow_path()), "stow")

  expect_true(grepl("Rtmp", stow_path(), fixed = TRUE))

  stow("https://github.com/geomarker-io/appc/releases/download/v0.1.0/nei_2020.rds") |>
    expect_identical(stow_path("nei_2020.rds"))

  stow("ftp://ftp2.census.gov/geo/tiger/TIGER2024/ADDR/tl_2024_39061_addr.zip") |>
    expect_identical(stow_path("tl_2024_39061_addr.zip"))

  stow_size("nei_2020.rds") |>
    expect_identical(structure(2883974, class = c("fs_bytes", "numeric")))

  stow_remove("nei_2020.rds")
  expect_true(!stow_exists("nei_2020.rds"))

  stow_remove(.delete_stow_dir_confirm = TRUE)
  expect_identical(nrow(stow_info()), 0L)

})
