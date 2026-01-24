# Use a data.frame and metadata to create a data package

Convert a data frame into a data package (`dpkg`) by providing specific
metadata in the arguments.

## Usage

``` r
as_dpkg(
  x,
  name = deparse(substitute(x)),
  version = "0.0.0.9000",
  title = character(),
  homepage = character(),
  description = character()
)
```

## Arguments

- x:

  a tibble or data frame

- name:

  a lowercase character string consisting of only `a-z`, `0-9`, `-`,
  `_`, or `.` to be used as a data package identifier

- version:

  a character string representing a [semantic
  version](https://datapackage.org/recipes/data-package-version/) (e.g.,
  "0.2.1")

- title:

  a character string that is a title of the data package for humans

- homepage:

  a valid URL that links to a webpage with code or descriptions related
  to creation of the data package

- description:

  a character string (markdown encouraged!) of more details about how
  the data was created, including the data sources, references to code
  or packages used, relevant details for any specific columns, and notes
  about (mis)usage of the data

## Value

a dpkg object

## Details

`name` should be specified, but if is not will be deparsed from code
defining `x`; this might not result in a valid `name` (e.g., when piping
code to create a data frame)

## Examples

``` r
x <- as_dpkg(mtcars, name = "mtcars", title = "Motor Trend Road Car Tests")
attr(x, "description") <- "This is a data set all about characteristics of different cars"
attr(x, "homepage") <- "https://github.com/cole-brokamp/dpkg"
x
#> # [☰] mtcars-v0.0.0.9000
#> # title: "Motor Trend Road Car Tests"
#> # homepage: https://github.com/cole-brokamp/dpkg
#> # ℹ Use `dpkg_meta() to get all metadata
#> # A tibble: 32 × 11
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows
```
