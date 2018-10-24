# take the "head" of matrix/tables as a n by n block
mhead <- function(x, n = 6) {
    stopifnot(!is.null(dim(x)))
    stopifnot(n < ncol(x))
    stopifnot(n < nrow(x))

    x[1:n, 1:n]
}

# left to right function composition
chain <- function(...) {
    fs <- lapply(list(...), match.fun)

    first <- fs[[1]]
    rest <- fs[-1]

    function(...) {
        out <- first(...)
        for (f in rest) {
            out <- f(out)
        }
        out
    }
}

outer_apply <- function(x, fn_list) {
    UseMethod("outer_apply", x)
}

# create data x methods cross product
# returns list of (data_name, method_name, result) values
outer_apply.list <- function(data, fn_list) {
    d_names <- names(data)
    m_names <- names(fn_list)

    output <- list()
    for (d_name in d_names) {
        for (m_name in m_names) {
            result <- list(
                data = d_name,
                .temp = m_name,
                result = fn_list[[m_name]](data[[d_name]])
            )
            output <- append(output, list(result))
        }
    }

    output <- tibble::tibble(
        data = purrr::map_chr(output, function(x) x$data),
        .temp = purrr::map_chr(output, function(x) x$.temp),
        result = purrr::map(output, function(x) x$result)
    )
    names(output)[2] <- deparse(substitute(fn_list))

    if (all_length_one(output$result)) {
        output$result <- unlist(output$result)
    }
    class(output) <- c("benchmark_tbl", class(output))

    output
}

all_length_one <- function(x) {
    stopifnot(is(x, "list"))
    all(sapply(x, function(x) { length(x) == 1 }))
}

outer_apply.tbl_df <- function(tbl_df, fn_list) {
    m_names <- names(fn_list)

    results <- list()
    for (data in tbl_df$result) {
        for (fn in fn_list[order(names(fn_list))]) {
            results <- append(results, list(fn(data)))
        }
    }

    output <- tbl_df %>% select(-result)
    output <- tidyr::crossing(tbl_df, names(fn_list))
    names(output)[ncol(output)] <- deparse(substitute(fn_list))
    output <- output %>%
        dplyr::mutate(result = results) %>%
        dplyr::select(-result, result) # move result column to end

    if (all_length_one(output$result)) {
        output$result <- unlist(output$result)
    }
    class(output) <- c("benchmark_tbl", class(output))

    output
}

# check if object is a list of functions
is_fn_list <- function(x) {
    is(x, "list") && all(purrr::map_lgl(x, is.function))
}

# create outer product by function composition
fn_outer_prod <- function(fn_list1, fn_list2) {
    stopifnot(is_fn_list(fn_list1))
    stopifnot(is_fn_list(fn_list2))

    fnames1 <- names(fn_list1)
    fnames2 <- names(fn_list2)

    output <- list()
    for (fname1 in fnames1) {
        for (fname2 in fnames2) {
            fname <- paste(fname1, fname2, sep = "..")
            output[[fname]] <- chain(fname1, fname2)
        }
    }

    class(output) <- c("fn_list", class(output))
    output
}