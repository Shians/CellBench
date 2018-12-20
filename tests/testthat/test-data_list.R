context("Data list constructor")

test_that(
    "Data list constructor works properly", {
    expect_is(data_list(a = 1, b = 2), "data_list")
    expect_is(data_list(a = 1, b = 2), "list")
    expect_is(data_list(a = "a", b = "b"), "data_list")
    expect_is(data_list(a = "a", b = "b"), "list")
    expect_error(data_list(1, b = 2))
    expect_error(data_list(a = 1, 2))
    expect_error(data_list(a = 1, b = "b"))
    expect_error(data_list(1))
})