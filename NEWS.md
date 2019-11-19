# CellBench 1.1.3

## Bug Fixes
* Data loading functions now appear in package index and documentation

# CellBench 1.1.2

## Bug Fixes
* Updated make_combinations to work with tidyr 1.0.0

## Modifications
* Updated the WritingWrappers vignette.
* Added a case study precompiled vignette.

# CellBench 1.1.1

## New Features
* Added any_task_errors() function to check if any tasks failed in benchmark tibble.

# CellBench 0.99.10
* Accepted into Bioconductor.

## New Features
* Added new vignettes for Tidyverse Patterns and Method Wrappers.

## Modifications
* `fn_arg_seq()` now has a `.strict` argument to check if arguments supplied are actually used in the function. Default is `FALSE`, previously this check is always done, but it failed for functions that use methods dispatch.
* `pipeline_collapse()` now has a `data.name` argument for if the name of the dataset should be kept in the pipeline string. Useful if only one dataset is used.
* `arrow_sep()` now uses ascii glyphs (only left and right available) instead of unicode. Unicode arrows fail when ggplots in rmarkdown is compiled into PDF, a common enough use-case for this to be concerning.

# CellBench 0.0.7

## Modifications
* purrr version number requirement set to (>= 0.3.0) because of argument name change in `partial()`
* Documentation reorganised to clean up package documentation index.
* Added landing page for `?CellBench`

# CellBench 0.0.6

## New Features

* Added propagation for errors.
* Added task_error class for errors.
* Added print method for task_error objects.

# CellBench 0.0.5

## New Features
* Added "Timing" vignette to explain time_methods.
* Changed apply_methods() to continue on errors and return error object in result column.

# CellBench 0.0.4

## Breaking Changes
* Changed `.name` arguments in `time_methods()` and `apply_methods()` to `name`.

## New Features
* Added time_methods function.
* Added set_cellbench_bpparam for more advanced parallelism options.

# CellBench 0.0.3

## New Features
* Implemented parallel application of methods to benchmark_tbl, previously only worked for dataset lists.
* Added fn_list constructor.
* Added data_list constructor.

# CellBench 0.0.2

## Bug Fixes
* Fixed bug in apply_methods() causing it to fail when more than 1 thread is used.

## Modifications
* Updated introduction vignette to describe multithreading and function caching.

# CellBench 0.0.1

* Minimal functioning package created.
* Compliant with BiocCheck::BiocCheck() and goodpractice::goodpractice().
