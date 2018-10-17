# check if object is any of a vector classes
is_one_of <- function(x, classes) {
    stopifnot(is(classes, "character"))
    purrr::map_lgl(classes, function(class) is(x, class)) %>% any()
}

# check if object is all of a vector classes
is_all_of <- function(x, classes) {
    stopifnot(is(classes, "character"))
    purrr::map_lgl(classes, function(class) is(x, class)) %>% any()
}