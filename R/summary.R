# summarise the pipeline in benchmark_tbl
summary.benchmark_tbl <- function(x) {
    method_names <- names(x)
    method_names <- method_names[-1]
    method_names <- method_names[-length(method_names)]
    print(glue::glue("Pipeline summary:"))
    print(glue::glue_collapse(glue::glue("{method_names}"), sep = " -> "))

    names(method_names) <- method_names
    unique_method_list <- purrr::map(method_names, function(nm) unique(x[, nm]) %>% dplyr::pull(1))

    for (method_name in method_names) {
        unique_methods <- unique_method_list[[method_name]]
        print(glue::glue(""))
        print(glue::glue("{method_name}:"))
        print(glue::glue_collapse(glue::glue("* {unique_methods}"), sep = "\n"))
    }
}
