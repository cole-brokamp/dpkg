# Use a dpkg to create a github release

A GitHub release will be created based on the current commit, tagged and
named according to the `name` and `version` of the dpkg. The dpkg
`description` is used for the release body.

## Usage

``` r
dpkg_gh_release(x, draft = TRUE)
```

## Arguments

- x:

  a data package (`dpkg`) object

- draft:

  logical; mark release as draft?

## Value

the URL to the release (invisibly)

## Details

The `GITHUB_PAT` environment variable must be set and the working
directory must be inside of a git repository with a GitHub remote.

The GitHub release will *not* be set to the latest release in order to
prevent problems with other automated actions that rely on the latest
release, like R universe or remotes `"*release"` syntax or other GitHub
actions.

Release tags are required to be unique, so this will fail if a release
with the same name and version already exists.

## Examples

``` r
if (FALSE) { # \dontrun{
dpkg_gh_release(
  as_dpkg(mtcars,
    version = "0.0.0.9001", title = "Foofy Cars",
    homepage = "https://github.com/cole-brokamp/dpkg",
    description =
      paste("# Foofy Cars\n",
        "This is a test for the [dpkg](https://github.com/cole-brokamp/dpkg) package.",
        collapse = "\n"
      )
  ),
  draft = FALSE
)
} # }
#> created release at: https://github.com/cole-brokamp/dpkg/releases/tag/mtcars-v0.0.0.9001
```
