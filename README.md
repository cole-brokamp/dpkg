# dpkg

<!-- badges: start -->
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/dpkg)](https://CRAN.R-project.org/package=dpkg)
[![R-CMD-check](https://github.com/cole-brokamp/dpkg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cole-brokamp/dpkg/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Data package (`dpkg`) is an R package used to create, stow, and read data packages.
Data packages are data frame, tibble, or tbl objects with specific metadata labels (name, version, title, homepage, description).
A data package object (`dpkg`) can be written to disk as a 'parquet' file and uploaded to a 'Amazon Web Service' (AWS) 'Simple Storage Service' (S3) bucket or to a 'GitHub' release to share with others.
Data package objects can be read into R from online repositories and the downloaded 'parquet' files are cached locally in the R user data directory.

## Installation

You can install the development version of dpkg from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cole-brokamp/dpkg")
```
