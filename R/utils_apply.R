# apply function across rows
row_apply <- purrr::partial(apply, MARGIN = 1)

# apply function down columns
col_apply <- purrr::partial(apply, MARGIN = 2)