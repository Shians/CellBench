# CellBench

R package for benchmarking single cell analysis methods. Currently under development.

# Development

Fork and clone this repository for development. The data required has been put in the [scBenchData](https://github.com/Shians/scBenchData) repository for cleanliness as they are intended to reside elsewhere in the finished package. Download them into the `inst/extdata/` with the following commands

```
# from the CellBench folder
wget https://github.com/Shians/scBenchData/raw/master/mix_9cell_data.RData -O inst/extdata/mix_9cell_data.RData
wget https://github.com/Shians/scBenchData/raw/master/mrna_mix_data.RData -O inst/extdata/mrna_mix_data.RData
wget https://github.com/Shians/scBenchData/raw/master/single_cell_data.RData -O inst/extdata/single_cell_data.RData
```

or download and place them manually.
