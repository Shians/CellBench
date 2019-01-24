# CellBench 0.0.6 (24-01-2019)

* Added propagation for errors
* Added task_error class for errors
* Added print method for task_error objects

# CellBench 0.0.5 (23-01-2019)

* Added "Timing" vignette to explain time_methods
* Changed apply_methods() to continue on errors and return error object in result column

# CellBench 0.0.4 (10-01-2019)

* Added time_methods function
* Added set_cellbench_bpparam for more advanced parallelism options
* Changed `.name` arguments in `time_methods()` and `apply_methods()` to `name`

# CellBench 0.0.3 (20-12-2018)

* Implemented parallel application of methods to benchmark_tbl, previously only worked for dataset lists
* Added fn_list constructor
* Added data_list constructor

# CellBench 0.0.2 (19-12-2018)

* Fixed bug in apply_methods() causing it to fail when more than 1 thread is used
* Updated introduction vignette to describe multithreading and function caching

# CellBench 0.0.1 (18-12-2018)

* Minimal functioning package created
* Compliant with BiocCheck::BiocCheck() and goodpractice::goodpractice()
