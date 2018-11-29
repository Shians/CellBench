# check if object is any of a vector classes
is_one_of <- function(x, classes) {
    stopifnot(methods::is(classes, "character"))
    purrr::map_lgl(classes, function(class) methods::is(x, class)) %>% any()
}
is_any_of <- is_one_of

# check if object all of a vector classes
is_all_of <- function(x, classes) {
    stopifnot(methods::is(classes, "character"))
    purrr::map_lgl(classes, function(class) methods::is(x, class)) %>% all()
}

all_unique <- function(x) {
    length(x) == length(unique(x))
}
