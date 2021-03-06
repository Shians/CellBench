# CellBench

[![Travis build status](https://travis-ci.org/Shians/CellBench.svg?branch=master)](https://travis-ci.org/Shians/CellBench)
[![Coverage status](https://codecov.io/gh/Shians/CellBench/branch/master/graph/badge.svg)](https://codecov.io/github/Shians/CellBench?branch=master)

<img src="https://github.com/Shians/CellBench/raw/master/CellBench.png" width="150" />

R package for benchmarking single cell analysis methods, primarily inspired by the modelling structure used in [DSC](https://github.com/stephenslab/dsc).

# Installation

```r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("shians/CellBench", ref = "R-3.5", build_opts = c("--no-resave-data", "--no-manual"))
```

# Introduction

This package revolves around one object and one function. The `benchmark_tbl` (benchmark [tibble](https://tibble.tidyverse.org)) and the `apply_methods(x, methods)` function.

We expect data to to be stored in lists, and we apply functions stored in lists to the data. This creates a `benchmark_tbl` where the names of the lists items are stored as columns and the final column contains the result of the computations.

```r
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

```r
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

See

```r
vignette("Introduction", package = "CellBench")
```

for a more detailed introduction and example with biological data.

# Features

* High compatibility with dplyr and rest of tidyverse, fundamental data object can be used with dplyr verbs
* Multithreading, methods can be applied in parallel

# License

This package is licensed under GNU General Public License v3.0 (GPL-3.0).
