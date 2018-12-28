#' @include setup-set_old_classes.R
#' @importFrom purrr %||%

#' @name aliases
#' @title Internal Utilities
#'
#' These utilities are used internally and not exported.
#' They are however documented for completeness
#'
#' @param x,obj An object, any object.
#' @param ... passed on to other function(s).
NULL

#' @describeIn aliases Alias for structure, but also adds automatic naming when unnamed,
#'
s <- function( x, ...){
    new.attr <- list(...)
    if (is.null(names(new.attr)))
        names(new.attr) <- as.character(substitute(c(...)))[-1]
    else if(any(. <- is.na(names(new.attr)) | names(new.attr) == ''))
        names(new.attr) <- ifelse(., as.character(substitute(c(...)))[-1], names(new.attr))

    for (a in names(new.attr))
        attr(x, a) <- new.attr[[a]]
    return(x)
}
if(FALSE){#@testing
    msg <- "An failure message"
    val <-s(FALSE, msg)
    expect_identical(attributes(val), list(msg=msg))


    val <- s(FALSE, msg, count = 5)
    expect_identical(attributes(val), list(msg=msg, count=5))

    val <- s(c(a=1, b=2), count=2)
    expect_identical(names(val), c('a','b'))
}

#' @describeIn aliases Specify an additional class for an object, or class when none is set.
#'
#' @param new the new class(es) to append.
cl <- function(x, new){s(x, class=union(new, attr(x, 'class')))}
if(FALSE){#@testing
    x <- cl(TRUE, 'success')
    expect_is(x, 'success')

    y <- cl(x, 'a big success')
    expect_is(y, 'success')
    expect_is(y, 'a big success')

    expect_identical(cl('text', 'class')
                    , structure('text', class='class'))
}

#' @describeIn aliases Remove the `dim` attribute.
undim <- function(x)s(x, dim=NULL)
if(FALSE){#@testing
    x <- matrix(1:6, 2, 3)
    dimnames(x) <- list(rows = c('a', 'b'), cols = c('x', 'y', 'z'))

    expect_identical(undim(x), 1:6)
}

#' @describeIn aliases Create a named list with names inferred if needed.
named <- function(...){
    . <- substitute(c(...))
    inferred <- as.character(.)[-1L]
    val <- list(...)
    if (is.null(n <- names(val)))
        names(val) <- inferred
    else
        names(val) <- ifelse( is.na(n) | n == '', inferred, n)
    val
}
if(FALSE){#@testing
    a <- 1L
    b <- TRUE

    val <- named(a,b)
    expect_identical(val, list(a=a, b=b))

    val <- named(a,b,c='hello')
    expect_identical(val, list(a=a, b=b, c='hello'))
}

#' @describeIn aliases Alias for [tools::toRd.default()]
clean_Rd <- tools:::toRd.default

#' @describeIn aliases Alias for `attr(x, which) %||% default`.
#' @param which name of the attribute to extract.
#' @param default the default value to return if not found.
#' @param exact exact or partial matching?
#' @seealso [purrr::%||%()]
get_attr <- function(x, which, default=NULL, exact=TRUE)
    attr(x, which=which, exact=exact) %||% default
if(FALSE){#@testing
    expect_identical(get_attr(s(list(), test='hello'), 'test'), 'hello')
    expect_null     (get_attr(s(list(), test='hello'), 'test2'))
    expect_identical(get_attr(s(list(), test='hello'), 'test3', 'world'), 'world')
}


#' @describeIn aliases Forward attributes from object to value.
#' @seealso [base::attributes()]
forward_attributes <- function(x, obj){
    mostattributes(x) <- attributes(obj)
    return(x)
}
if(FALSE){#@testing
    a <- s( list(Rd_symb("some"))
          , Rd_tag="\\keyword"
          , class=c("Rd_tag", 'Rd'))
    b <- forward_attributes(list(), a)
    expect_identical(attributes(a), attributes(b))

    a <- s( matrix(1:6, 2, 3)
          , class = 'rectangle'
          , another='shape'
          )
    b <- forward_attributes(list(), a)
    expect_true('dim' %in% names(attributes(a)))
    expect_false('dim' %in% names(attributes(b)))
    expect_identical( attributes(a)[names(attributes(b))]
                    , attributes(b)
                    )
}

#' @describeIn aliases Alias for `forward_attributes()`.
fwd <- forward_attributes

#' @describeIn aliases Check if a string is composed of only whitespace, with [regex][base::grep()] pattern `"^\\s+$"`.
is_whitespace <- function(x){
    grepl("^\\s+$", x)
}
if(FALSE){#@testing
    expect_true(is_whitespace(" "))
    expect_true(is_whitespace("\t"))
    expect_false(is_whitespace("t"))
    expect_false(is_whitespace(""))
}

