# # Wrapper for basics
# run_basics <- function(
#     data,
#     N = 2000,
#     Thin = 10,
#     Burn = 400,
#     Regression = TRUE,
#     ...
# ) {
#     counts <- counts(data) %>% filter_zero_genes()
#     tech_features <- stringr::str_detect(rownames(counts), "ERCC")

#     with_spikes <- sum(tech_features) > 0
#     if (with_spikes) {
#         tech_counts <- counts[tech_features, ]
#         spike_info <- data.frame("SpikeID" = rownames(counts)[tech_features], "SpikeInput" = row_apply(tech_counts, max))

#         data <- BASiCS::newBASiCS_Data(
#             counts,
#             tech_features,
#             spike_info
#         )
#     } else {
#         data <- BASiCS::newBASiCS_Data(
#             counts,
#             tech_features,
#             SpikeInfo = NULL,
#             BatchInfo = SingleCellExperiment::colData(data)$cell_line
#         )
#     }

#     chain <- BASiCS::BASiCS_MCMC(
#         data,
#         WithSpikes = with_spikes,
#         N = N,
#         Thin = Thin,
#         Burn = Burn,
#         Regression = Regression,
#         PrintProgress = FALSE,
#         ...
#     )

#     count_mat <- BASiCS::BASiCS_DenoisedCounts(Data = data, Chain = chain)
#     count_mat
# }
