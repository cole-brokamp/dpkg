# get info about stowed files

get info about stowed files

get the path to a stowed file (or the stow directory)

test if a stowed file (or the stow directory) exists

get the size of a stowed file

remove a stowed file (or the stow entire directory)

## Usage

``` r
stow_info(filename = NULL)

stow_path(filename = NULL)

stow_exists(filename = NULL)

stow_size(filename = NULL)

stow_remove(filename = NULL, .delete_stow_dir_confirm = FALSE)
```

## Arguments

- filename:

  character filename of stowed file; if NULL, then information about
  *all* stowed files or the directory where files are stowed is returned

- .delete_stow_dir_confirm:

  set to TRUE in order to delete the entire stow directory without
  interactive user confirmation

## Value

for `stow_info()`, a tibble of file or folder information; for
`stow_path()`, a character path to the stowed file or stow directory;
for `stow_exists()`, a logical; for `stow_size()`, a fs::

## Examples

``` r
Sys.setenv(R_USER_DATA_DIR = tempfile("stow"))

stow_path()
#> [1] "/tmp/RtmpJ51sXR/stow1fee1da4edd6/R/stow"

stow("https://github.com/geomarker-io/appc/releases/download/v0.1.0/nei_2020.rds")
#> [1] "/tmp/RtmpJ51sXR/stow1fee1da4edd6/R/stow/nei_2020.rds"

stow_path("nei_2020.rds")
#> [1] "/tmp/RtmpJ51sXR/stow1fee1da4edd6/R/stow/nei_2020.rds"

stow_exists("nei_2020.rds")
#> [1] TRUE

stow_size("nei_2020.rds")
#> 2.75M

stow("https://github.com/geomarker-io/appc/releases/download/v0.1.0/nei_2017.rds")
#> [1] "/tmp/RtmpJ51sXR/stow1fee1da4edd6/R/stow/nei_2017.rds"

stow_info("nei_2017.rds")
#> # A tibble: 1 × 18
#>   path         type   size permissions modification_time   user  group device_id
#>   <fs::path>   <fct> <fs:> <fs::perms> <dttm>              <chr> <chr>     <dbl>
#> 1 …ei_2017.rds file  2.67M rw-r--r--   2026-01-24 13:25:26 runn… runn…      2049
#> # ℹ 10 more variables: hard_links <dbl>, special_device_id <dbl>, inode <dbl>,
#> #   block_size <dbl>, blocks <dbl>, flags <int>, generation <dbl>,
#> #   access_time <dttm>, change_time <dttm>, birth_time <dttm>

stow_info()
#> # A tibble: 2 × 18
#>   path         type   size permissions modification_time   user  group device_id
#>   <fs::path>   <fct> <fs:> <fs::perms> <dttm>              <chr> <chr>     <dbl>
#> 1 …ei_2017.rds file  2.67M rw-r--r--   2026-01-24 13:25:26 runn… runn…      2049
#> 2 …ei_2020.rds file  2.75M rw-r--r--   2026-01-24 13:25:26 runn… runn…      2049
#> # ℹ 10 more variables: hard_links <dbl>, special_device_id <dbl>, inode <dbl>,
#> #   block_size <dbl>, blocks <dbl>, flags <int>, generation <dbl>,
#> #   access_time <dttm>, change_time <dttm>, birth_time <dttm>

stow_size()
#> 5.42M

stow_remove(.delete_stow_dir_confirm = TRUE)
```
