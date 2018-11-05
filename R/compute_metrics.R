# compute adjusted rand index
adj_rand_index <- function(prediction, truth) {
    mclust::adjustedRandIndex(prediction, truth)
}