# CellBench

R package for benchmarking single cell analysis methods. Currently under development. Functions and API are liable to change without warning.

# Introduction

This package revolves around one object and one function. The `benchmark_tbl` (benchmark [tibble](https://tibble.tidyverse.org)) and the `apply_methods(x, methods)` function.

We expect data to to be stored in lists, and we apply functions stored in lists to the data. This creates a `benchmark_tbl` where the names of the lists items are stored as columns and the final column contains the result of the computations.

```
library(CellBench)

sample1 <- data.frame(
    x = matrix(runif(25), nrow = 5, ncol = 5)
)

sample2 <- data.frame(
    x = matrix(runif(25), nrow = 5, ncol = 5)
)

datasets <- list(
    sample1 = sample1,
    sample2 = sample2
)

transform <- list(
    correlation = cor,
    covariance = cov
)

datasets %>% apply_methods(transform)

## # A tibble: 4 x 3
##   data    metric      result       
##   <fct>   <fct>       <list>       
## 1 sample1 correlation <dbl [5 × 5]>
## 2 sample1 covariance  <dbl [5 × 5]>
## 3 sample2 correlation <dbl [5 × 5]>
## 4 sample2 covariance  <dbl [5 × 5]>
```

We can additionally chain method applications and this will combinatorially expand our `benchmark_tbl` so that combinations of methods can easily be computed.

```
metric <- list(
    mean = mean,
    median = median
)

datasets %>%
    apply_methods(transform) %>%
    apply_methods(metric)

## # A tibble: 8 x 4
##   data    transform   metric   result
##   <fct>   <fct>       <fct>     <dbl>
## 1 sample1 correlation mean    0.0602 
## 2 sample1 correlation median -0.0520 
## 3 sample1 covariance  mean    0.00823
## 4 sample1 covariance  median -0.00219
## 5 sample2 correlation mean    0.303  
## 6 sample2 correlation median  0.482  
## 7 sample2 covariance  mean    0.0115 
## 8 sample2 covariance  median  0.0132 
```

The result table is essentially a regular `tibble` and works with all `tidyverse` packages.

# Features

TODO...

# License

This package is licensed under GNU General Public License v3.0 (GPL-3.0).
