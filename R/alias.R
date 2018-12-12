###
# File for storing simple aliases for existing functions
###

# alias for subsetting operator for code clarity elsewhere
subset_inds <- `[`

# apply function across rows
row_apply <- purrr::partial(apply, MARGIN = 1)

# apply function down columns
col_apply <- purrr::partial(apply, MARGIN = 2)

# glue collapse with default sep and last
collapse_with_comma <- purrr::partial(
    glue::glue_collapse,
    sep = ", ",
    last = " and "
)
