load_sc_data <- function() {
    load(system.file("extdata", "single_cell_data.RData", package = "CellBench"))
    assign(
        "cellbench_sc_data",
        list(
            "sc_10x" = sc_10x,
            "sc_celseq" = sc_celseq,
            "sc_dropseq" = sc_dropseq
        ),
        envir = parent.frame(n = 1)
    )
}

load_cell_mix_data <- function() {
    load(system.file("extdata", "mix_9cell_data.RData", package = "CellBench"))
    assign(
        "cellbench_cell_mix_data",
        list(
            "cell_mix1" = mix_9cell_07clean_1cell_mat,
            "cell_mix2" = mix_9cell_08clean_1cell_mat,
            "cell_mix3" = mix_9cell_09clean_1cell_mat,
            "cell_mix4" = mix_9cell_07clean_3cell_mat,
            "cell_mix5" = mix_9cell_07clean_90cell_mat
        ),
        envir = parent.frame(n = 1)
    )
}

load_mrna_mix_data <- function() {
    load(system.file("extdata", "mrna_mix_data.RData", package = "CellBench"))
    assign(
        "cellbench_mrna_mix_data",
        list(
            "mrna_mix_celseq" = mrna_mix_celseq,
            "mrna_mix_sortseq" = mrna_mix_sortseq
        ),
        envir = parent.frame(n = 1)
    )
}

load_all_data <- function() {
    load(system.file("extdata", "single_cell_data.RData", package = "CellBench"))
    load(system.file("extdata", "mix_9cell_data.RData", package = "CellBench"))
    load(system.file("extdata", "mrna_mix_data.RData", package = "CellBench"))
    assign(
        "cellbench_data",
        list(
            "sc_10x" = sc_10x,
            "sc_celseq" = sc_celseq,
            "sc_dropseq" = sc_dropseq,
            "cell_mix1" = mix_9cell_07clean_1cell_mat,
            "cell_mix2" = mix_9cell_08clean_1cell_mat,
            "cell_mix3" = mix_9cell_09clean_1cell_mat,
            "cell_mix4" = mix_9cell_07clean_3cell_mat,
            "cell_mix5" = mix_9cell_07clean_90cell_mat,
            "mrna_mix_celseq" = mrna_mix_celseq,
            "mrna_mix_sortseq" = mrna_mix_sortseq
        ),
        envir = parent.frame(n = 1)
    )
}
