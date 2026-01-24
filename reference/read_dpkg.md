# read (meta)data from dpkg on disk

read (meta)data from dpkg on disk

## Usage

``` r
read_dpkg_metadata(x)

read_dpkg(x)
```

## Arguments

- x:

  path to data package (`.parquet` file) on disk

## Value

for `read_dpkg()`, a dpkg object; for `read_dpkg_metadata()`, a list of
metadata

## Examples

``` r
d <- as_dpkg(mtcars, version = "0.1.0", title = "Motor Trend Road Car Tests")
attr(d, "description") <- "This is a data set all about characteristics of different cars"
attr(d, "homepage") <- "https://github.com/cole-brokamp/dpkg"

write_dpkg(d, dir = tempdir()) |>
  read_dpkg()
#> # [☰] mtcars-v0.1.0
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

# geo objects are supported via the `geoarrow_vctr` in the geoarrow package
library(geoarrow)
sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf")) |>
  as_dpkg(name = "nc_data") |>
  write_dpkg(tempdir())
d <- read_dpkg(fs::path_temp("nc_data-v0.0.0.9000.parquet"))
d
#> # [☰] nc_data-v0.0.0.9000
#> # ℹ Use `dpkg_meta() to get all metadata
#> # A tibble: 100 × 15
#>     AREA PERIMETER CNTY_ CNTY_ID NAME  FIPS  FIPSNO CRESS_ID BIR74 SID74 NWBIR74
#>    <dbl>     <dbl> <dbl>   <dbl> <chr> <chr>  <dbl>    <int> <dbl> <dbl>   <dbl>
#>  1 0.114      1.44  1825    1825 Ashe  37009  37009        5  1091     1      10
#>  2 0.061      1.23  1827    1827 Alle… 37005  37005        3   487     0      10
#>  3 0.143      1.63  1828    1828 Surry 37171  37171       86  3188     5     208
#>  4 0.07       2.97  1831    1831 Curr… 37053  37053       27   508     1     123
#>  5 0.153      2.21  1832    1832 Nort… 37131  37131       66  1421     9    1066
#>  6 0.097      1.67  1833    1833 Hert… 37091  37091       46  1452     7     954
#>  7 0.062      1.55  1834    1834 Camd… 37029  37029       15   286     0     115
#>  8 0.091      1.28  1835    1835 Gates 37073  37073       37   420     0     254
#>  9 0.118      1.42  1836    1836 Warr… 37185  37185       93   968     4     748
#> 10 0.124      1.43  1837    1837 Stok… 37169  37169       85  1612     1     160
#> # ℹ 90 more rows
#> # ℹ 4 more variables: BIR79 <dbl>, SID79 <dbl>, NWBIR79 <dbl>, geom <grrw_vct>

# as a simple features collection
d$geom <- sf::st_as_sfc(d$geom)
sf::st_as_sf(d)
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
#> # [☰] nc_data-v0.0.0.9000
#> # ℹ Use `dpkg_meta() to get all metadata
#> # A tibble: 100 × 15
#>     AREA PERIMETER CNTY_ CNTY_ID NAME  FIPS  FIPSNO CRESS_ID BIR74 SID74 NWBIR74
#>    <dbl>     <dbl> <dbl>   <dbl> <chr> <chr>  <dbl>    <int> <dbl> <dbl>   <dbl>
#>  1 0.114      1.44  1825    1825 Ashe  37009  37009        5  1091     1      10
#>  2 0.061      1.23  1827    1827 Alle… 37005  37005        3   487     0      10
#>  3 0.143      1.63  1828    1828 Surry 37171  37171       86  3188     5     208
#>  4 0.07       2.97  1831    1831 Curr… 37053  37053       27   508     1     123
#>  5 0.153      2.21  1832    1832 Nort… 37131  37131       66  1421     9    1066
#>  6 0.097      1.67  1833    1833 Hert… 37091  37091       46  1452     7     954
#>  7 0.062      1.55  1834    1834 Camd… 37029  37029       15   286     0     115
#>  8 0.091      1.28  1835    1835 Gates 37073  37073       37   420     0     254
#>  9 0.118      1.42  1836    1836 Warr… 37185  37185       93   968     4     748
#> 10 0.124      1.43  1837    1837 Stok… 37169  37169       85  1612     1     160
#> # ℹ 90 more rows
#> # ℹ 4 more variables: BIR79 <dbl>, SID79 <dbl>, NWBIR79 <dbl>,
#> #   geom <MULTIPOLYGON [°]>

# read just the metadata
read_dpkg_metadata(fs::path_temp("nc_data-v0.0.0.9000.parquet"))
#> $name
#> [1] "nc_data"
#> 
#> $version
#> [1] "0.0.0.9000"
#> 
#> $title
#> character(0)
#> 
#> $homepage
#> character(0)
#> 
#> $description
#> character(0)
#> 
#> $hash
#> [1] "289449022c576625697e520e221775f5a0074c9f342f41f25614224b99107acc"
#> 
#> $created
#> [1] "2026-01-24 13:25:24 UTC"
#> 
#> $num_rows
#> [1] 100
#> 
#> $num_cols
#> [1] 15
#> 
#> $fields
#>  [1] "AREA"      "PERIMETER" "CNTY_"     "CNTY_ID"   "NAME"      "FIPS"     
#>  [7] "FIPSNO"    "CRESS_ID"  "BIR74"     "SID74"     "NWBIR74"   "BIR79"    
#> [13] "SID79"     "NWBIR79"   "geom"     
#> 
#> $file_size
#> 39K
```
