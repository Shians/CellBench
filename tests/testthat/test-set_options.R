context("Option setting")

test_that(
    "Threads can be set properly", {
    set_cellbench_threads(1)
    expect_identical(getOption("CellBench.threads"), 1)

    set_cellbench_threads(2)
    expect_identical(getOption("CellBench.threads"), 2)

    expect_error(
        set_cellbench_threads("foo"),
        "is.numeric(nthreads) is not TRUE",
        fixed = TRUE
    )

    expect_error(
        set_cellbench_bpparam(1),
        'is(param, "BiocParallelParam") is not TRUE',
        fixed = TRUE
    )

    expect_silent(set_cellbench_bpparam(BiocParallel::SerialParam()))
})

test_that(
    "Cache path can be set properly", {
    expect_error(
        set_cellbench_cache_path(1),
        "is.character(path) is not TRUE",
        fixed = TRUE
    )

    p <- file.path(tempdir(), ".CellBenchCache")
    set_cellbench_cache_path(p)

    expect_true(dir.exists(p))
    unlink(p)
})
