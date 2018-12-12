context("Memoise")

test_that(
    "Memoised functions product the same result", {
    cache_path <- file.path(tempdir(), ".CellBenchCache")
    set_cellbench_cache_path(cache_path)
    fn <- function(x) { x }
    mfn <- cache_method(fn)

    expect_identical(mfn(1), fn(1))
    expect_message(clear_cellbench_cache())
})