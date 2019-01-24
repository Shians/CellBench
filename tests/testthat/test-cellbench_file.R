context("Helper for CellBench internal files")

test_that(
    "cellbench_file works properly", {
    expect_is(cellbench_file(), "character")
    expect_gt(length(cellbench_file()), 0)

    expect_is(cellbench_file("10x_sce_sample.rds"), "character")
    expect_length(cellbench_file("10x_sce_sample.rds"), 1)

    expect_error(
        cellbench_file("10x_sce_samples.rds"),
        "file not found, run cellbench_file() to see available files",
        fixed = TRUE
    )
})
