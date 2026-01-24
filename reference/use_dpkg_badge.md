# Use a markdown badge for a dpkg's latest github release

The badge relies on shields.io for the images, which will always display
to the most recently released version and will link to the releases
specific to the dpkg name.

## Usage

``` r
use_dpkg_badge(x)
```

## Arguments

- x:

  a data package (`dpkg`) object

## Value

character string of markdown

## Details

Note that this relies on the structure of the release created with
[`dpkg_gh_release()`](http://geomarker.io/codec/reference/dpkg_gh_release.md),
but relies on a dpkg object *before* it is released. This will lead to
broken release badges and links until an initial dpkg release is created
with
[`dpkg_gh_release()`](http://geomarker.io/codec/reference/dpkg_gh_release.md).

## Examples

``` r
if (FALSE) { # \dontrun{
as_dpkg(mtcars,
  version = "0.0.0.9000", title = "Foofy Cars",
  homepage = "https://github.com/cole-brokamp/dpkg",
  description =
    paste("# Foofy Cars\n",
      "This is a test for the [dpkg](https://github.com/cole-brokamp/dpkg) package.",
      collapse = "\n"
    )
) |>
  use_dpkg_badge()
} # }
```
