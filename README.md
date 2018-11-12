# CellBench

R package for benchmarking single cell analysis methods. Currently under development.

# Introduction

This package revolves around one object and one function. The `benchmark_tbl` (benchmark [tibble](https://tibble.tidyverse.org)) and `apply_methods(x, methods)`.

We expect data to arrive in lists, and we apply lists of functions to the data. This creates a `benchmark_tbl` where the names of the lists are stored as columns and the final column contains the result of the computations.

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

# Development

Fork and clone this repository for development. The data required has been put in the [scBenchData](https://github.com/Shians/scBenchData) repository for cleanliness as they are intended to reside elsewhere in the finished package. Download them into the `inst/extdata/` with the following commands

```
# from the CellBench folder
mkdir -p inst/extdata
wget https://github.com/Shians/scBenchData/raw/master/mix_9cell_data.RData -O inst/extdata/mix_9cell_data.RData
wget https://github.com/Shians/scBenchData/raw/master/mrna_mix_data.RData -O inst/extdata/mrna_mix_data.RData
wget https://github.com/Shians/scBenchData/raw/master/single_cell_data.RData -O inst/extdata/single_cell_data.RData
```

or download and place them manually.
