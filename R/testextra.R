## testextra.R
## This file consists of functions used to be included in testextra.
## but as that package is delaying release they are included here.
## This file is to be removed when testextra is released to CRAN.


.nonempty.string.msg <- "does not conform to a non-empty string (a character vector of length 1 without without missing or empty values)."
is_nonempty_string <- function(x){
    is.character(x) &&
    length(x) == 1L &&
    !is.na(x)       &&
    nchar(x) > 0L
}
attr(is_nonempty_string, "fail") <-
function (call, env) {
    sQuote(deparse(call$x)) %<<% .nonempty.string.msg
}


are <- function(lst, class){
    purrr::map_lgl(lst, is, class)
}

all_inherit <- function(lst, class, label=NULL){
    act <- testthat::quasi_label(rlang::enquo(lst), label)
    stopifnot( is.character(class) || is.null(class) )
    if (all(. <- purrr::map_lgl(lst, inherits, what=class, which=FALSE)))
        return(TRUE)
    msg <- if (sum(!.) > 1L) {
        ._("%s has bad elements at %s which do not inherit from %s."
          , act$lab
          , comma_list(which(!.))
          , comma_list(dQuote(class), sep2 = ' or ', sep.last = ' or ')
          ) } else {
        bad.class <- purrr::map_chr(lst[!.], class0)
        ._("%s has bad element at %s which does not inherit from %s. It is a %s"
          , act$lab
          , comma_list(which(!.))
          , comma_list(dQuote(class), sep2 = ' or ', sep.last = ' or ')
          , dQuote(bad.class)
          )
          }
    return(structure(FALSE, msg=msg, bad.elements = which(!.)))
}

is_exactly <- function(object, class){
    any(inherits(object, what=class, which=TRUE)==1)
}
expect_is_exactly <-
function (object, class, info = NULL, label = NULL){
    stopifnot(is.character(class))
    act <- testthat::quasi_label(rlang::enquo(object), label)
    act$class <- collapse(class(object), "/")
    exp_lab <- comma_list(class, sep2 = ' or ', sep.last = ', or a')
    testthat::expect( is_exactly(act$val, class)
                    , sprintf("%s is a %s; should be exactly a `%s`."
                             , act$lab, act$class, exp_lab)
                    , info = info)
    invisible(act$val)
}

all_are_exactly <-
function(lst, class, label=NULL){
    act <- testthat::quasi_label(rlang::enquo(lst), label)
    stopifnot( is.string(class) )
    if (all(. <- purrr::map_lgl(lst, is_exactly, class=class)))
        return(TRUE)
    bad.class <- purrr::map_chr(lst[!.], class0)
    msg <- if (sum(!.) > 1L){
        ._("%s has bad elements at positions %s which are not of class %s."
          , act$lab
          , comma_list(which(!.))
          , dQuote(class)
          )} else {
        ._("%s has bad element at position %s which is not of class %s."
          , act$lab
          , which(!.)
          , dQuote(class)
        )}
    return(structure(FALSE, msg=msg))
}

class0 <- function(x)collapse(class(x), '/')

expect_is_not <-
function (object, class, info = NULL, label = NULL){
    stopifnot(is.character(class))
    act <- testthat::quasi_label(rlang::enquo(object), label)
    act$class <-
    exp_lab <- paste(class, collapse = "/")
    testthat::expect( Negate(is)(act$val, class)
                    , sprintf("%s is a %s; should not inherit from `%s`."
                             , act$lab, act$class, exp_lab)
                    , info = info)
    invisible(act$val)
}

expect_all_inherit <-
function (object, class, info = NULL, label = NULL) {
    act <- testthat::quasi_label(rlang::enquo(object), label)
    test <- all_inherit(object, class, label=act$lab)
    testthat::expect( isTRUE(test)
                    , attr(test, 'msg')
                    , info = info)
    invisible(test)
}
