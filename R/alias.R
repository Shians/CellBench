###
# File for storing simple aliases for existing functions
###

# alias for subsetting operator for code clarity elsewhere
subset_inds <- `[`

# apply function across rows
row_apply <- purrr::partial(apply, MARGIN = 1)

# apply function down columns
col_apply <- purrr::partial(apply, MARGIN = 2)
