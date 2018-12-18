#' @name check_class
#' @title Check class of object
#' @description Check an object against a vector of class names. Testing if they
#'   match any or all of the classes. For is_all_of, the object needs to be at
#'   least every class specified, but it can have addition classes and still
#'   pass the check.
#'
#' @param x the object to check
#' @param classes the vector of strings of class names
#'
#' @return boolean value for the result of the check
NULL

#' @rdname check_class
#'
#' @importFrom methods is
#' @export
#'
#' @examples
#' is_one_of(1, c("numeric", "logical")) # TRUE
#' is_one_of(1, c("character", "logical")) # FALSE
#'
#' is_all_of(1, c("numeric", "logical")) # FALSE
#' is_all_of(tibble::tibble(), c("tbl", "data.frame")) # TRUE
is_one_of <- function(x, classes) {
    stopifnot(is(classes, "character"))
    purrr::map_lgl(classes, function(class) is(x, class)) %>% any()
}

#' @rdname check_class
#' @export
is_any_of <- is_one_of

#' @rdname check_class
#'
#' @importFrom methods is
#' @export
is_all_of <- function(x, classes) {
    stopifnot(is(classes, "character"))
    purrr::map_lgl(classes, function(class) is(x, class)) %>% all()
}


#' Check if all values in a vector are unique
#'
#' @param x the vector to check
#'
#' @return TRUE if all values in the vector are unique
#' @export
#'
#' @examples
#' all_unique(c(1, 2, 3)) # TRUE
#' all_unique(c(1, 2, 2)) # FALSE
all_unique <- function(x) {
    stopifnot(is.vector(x))

    length(x) == length(unique(x))
}
