# dpkg

<!-- badges: start -->
[![CRAN
status](https://www.r-pkg.org/badges/version/dpkg)](https://CRAN.R-project.org/package=dpkg)
[![R-CMD-check](https://github.com/cole-brokamp/dpkg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cole-brokamp/dpkg/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

Description: Data frame, tibble, or tbl objects are converted to data package objects using specific metadata labels (name, version, title, homepage, description). A data package object (`dpkg`) can be written to disk as a 'parquet' file or released to a 'GitHub' repository. Data package objects can be read into R from online repositories and downloaded files are cached locally across R sessions.

## Installation

Install the released version of dpkg from CRAN with:

```r
install.packages("dpkg")
```

You can install the development version of dpkg from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cole-brokamp/dpkg")
```
