run_basics <- function(data) {
    counts <- counts(data) %>% filter_zero_genes()
    tech_features <- stringr::str_detect(rownames(counts), "ERCC")

    with_spikes <- sum(tech_features) > 0
    if (with_spikes) {
        tech_counts <- counts[tech_features, ]
        spike_info <- data.frame("SpikeID" = rownames(counts)[tech_features], "SpikeInput" = row_apply(tech_counts, max))

        data <- BASiCS::newBASiCS_Data(
            counts,
            tech_features,
            spike_info
        )
    } else {
        data <- BASiCS::newBASiCS_Data(
            counts,
            tech_features,
            SpikeInfo = NULL,
            BatchInfo = SingleCellExperiment::colData(data)$cell_line
        )
    }

    chain <- BASiCS::BASiCS_MCMC(
        data,
        N = 4000,
        Thin = 10,
        Burn = 2000,
        Regression = TRUE,
        WithSpikes = with_spikes,
        PrintProgress = FALSE
    )

    BASiCS::BASiCS_DenoisedCounts(Data = data, Chain = chain)
}