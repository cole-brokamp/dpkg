# as_dpkg() works

    Code
      x[1, ]
    Message
      # [=] mtcars-v0.0.0.9000
      # i Use `dpkg_meta() to get all metadata
    Output
      # A tibble: 1 x 11
          mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
      1    21     6   160   110   3.9  2.62  16.5     0     1     4     4

