# download a github release asset to the `stow` R user directory

Use stow to abstract away the process of downloading a file or a GitHub
release asset to a user's data directory, only downloading files that
have not already been downloaded.

## Usage

``` r
stow_gh_release(owner, repo, dpkg, overwrite = FALSE)

stow(uri, overwrite = FALSE)

stow_url(url, overwrite = FALSE)
```

## Arguments

- owner:

  string of repo owner

- repo:

  string of repo name

- dpkg:

  string of gh release tag (will be the same as the filename without the
  `.parquet` extension)

- overwrite:

  logical; re-download the remote file even though a local file with the
  same name exists?

- uri:

  character string universal resource identifier; currently, must begin
  with `http://`, `https://`, `ftp://`, or `gh://`

- url:

  a URL string starting with `http://`, `https://`, or `ftp://`

## Value

path to the stowed file or url to github release

## Details

Supported URI prefixes include:

- `https://`, `http://`: download from a file

- `gh://`: download a github release asset, formatted as
  `gh://owner/repo/name`

Stow downloads files to the users data directory; see
[`?tools::R_user_dir`](https://rdrr.io/r/tools/userdir.html). Specify an
alternative download location by setting the `R_USER_DATA_DIR`
environment variable. The stow cache works by name only; that is, if a
file with the same URI has already been downloaded once, it will not be
re-downloaded again (unless `overwrite = TRUE`).

## Examples

``` r
if (FALSE) { # \dontrun{
Sys.setenv(R_USER_DATA_DIR = tempfile("stow"))
# get by using URL
stow("https://github.com/geomarker-io/appc/releases/download/v0.1.0/nei_2020.rds",
  overwrite = TRUE
) |>
  readRDS()

# will be faster (even in later R sessions) next time
stow("https://github.com/geomarker-io/appc/releases/download/v0.1.0/nei_2020.rds")

# get a data package from a GitHub release
stow("gh://cole-brokamp/dpkg/mtcars-v0.0.0.9000")

# use FTP protocol
stow("ftp://ftp2.census.gov/geo/tiger/TIGER2024/ADDR/tl_2024_39061_addr.zip")
} # }
```
