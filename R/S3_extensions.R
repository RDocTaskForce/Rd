### Extensions to S3 class #####

#' @name Rd-extraction
#' @title Extract elements from Rd containers
#'
#' These functions can be used to extract or subset elements of
#' Rd lists, either bare `Rd` containers or `Rd_tag` objects.
#' The `[[` operator wraps `Rd_get_element` and `[` wraps
#' `Rd_subset`, respectively.
#'
#' @param x Either an `Rd` or `Rd_tag` object.
#' @param ... Must be empty.
#' @inheritParams base::Extract
#'
#' @return
#' * **`Rd_get_element` and \code{\link[base:Extract]{[[}}:**
#'   the return value is not necessarily an Rd.
#'   The class of the element returned will
#'   inferred if it does not have a class.
#'   Lists will be classed as `Rd` unless the `Rd_tag` attribute
#'   is set in which case a `Rd_tag` will be returned
#' * **`Rd_subset` and \code{\link[base:Extract]{[}}:**
#'   will always return a value of the same class as is given.
#'   Even if what is returned is an empty list, an empty list with
#'   the `Rd_tag` attribute set is valid, and denotes a tag without
#'   any content such as the "\\\\item" tag.
#'
#' @seealso [base::Extract]
NULL

Rd_get_element <- function(x, ..., drop=TRUE){
    if (...length() != 1L)
        pkg_error( "incorrect number of subscripts"
                 , type="invalid_subscripts")
    val <- if (is.character(tag <- ..1)){
        assert_that(is.string(tag))
        tags <- purrr::map_chr(x, get_attr, 'Rd_tag', '')
        i <- which(tags == tag)
        if (length(i) == 0L) pkg_error(._("tag %s not found", sQuote(tag)), type="not_found") else
        if (length(i) >= 2L) pkg_error(._("multiple elements matching tag %s found", sQuote(tag)), type="multiple_found") else
        as.list(x)[[i]]
    } else {
        i <- ..1
        unclass(x)[[...]]
    }
    if (is(val, 'Rd')) return(val)
    if (is.character(val)){
        if (is.null(get_Rd_tag(val))) return(val)
        return(cl(val, 'Rd_string'))
    }
    if (!is_exactly(val, 'list')) return(val)  # nocov  should never happen just for completeness
    tag <- get_Rd_tag(val)
    if (is.null(tag)) return(s(val, class='Rd'))
    if (!is.null(tag))return(cl(val, 'Rd_tag'))
}
if(FALSE){#@testing Rd_get_element by the numbers
    x <- .Rd( Rd_comment("% a comment")
            , Rd_text("\n")
            , Rd_tag("\\name", Rd_symb("testing"))
            , Rd_text("\n")
            , Rd_tag("\\item", .Rd(Rd_symb("name"))
                             , .Rd(Rd_text("A description")))
            )
    expect_is(Rd_get_element(Rd_unclass(x), 1), 'Rd_string')
    expect_true(is_Rd_string(Rd_get_element(Rd_unclass(x), 1), 'COMMENT', strict = TRUE))
    expect_true(is_Rd_string(Rd_get_element(Rd_unclass(x), 2), 'TEXT', strict = TRUE))
    expect_true(is_Rd_tag(Rd_get_element(Rd_unclass(x), 3), '\\name', strict = TRUE))
    expect_true(is_Rd_string(Rd_get_element(Rd_unclass(x), c(3,1)), 'VERB', strict = TRUE))
    expect_true(is_Rd_string(Rd_get_element(Rd_unclass(x), 4), 'TEXT', strict = TRUE))
    expect_true(is_Rd_tag(Rd_get_element(Rd_unclass(x), 5), '\\item', strict = TRUE))
    expect_true(is_Rd(Rd_get_element(Rd_unclass(x), c(5, 1)), strict = TRUE))
    expect_true(is_Rd(Rd_get_element(Rd_unclass(x), c(5, 2)), strict = TRUE))
    expect_true(is_Rd(Rd_get_element(2, x=
                      Rd_get_element(Rd_unclass(x), 5))
                     , strict = TRUE))
    expect_true(is_Rd_string(Rd_get_element(Rd_unclass(x), c(5,2,1)), strict = TRUE))
    expect_is_exactly(Rd_get_element(Rd_unclass(x), c(5,2,1,1)), 'character')
}
if(FALSE){#@testing Rd_get_element by the tag
    test.file <- system.file("examples", "Normal.Rd", package = 'Rd')
    txt <- tools::parse_Rd(test.file)
    txt <- Rd_rm_srcref(txt)
    txt <- Rd_unclass(txt)

    expect_is_exactly(txt, 'list')

    expect_error( Rd_get_element(txt, 1,2,3)
                , class="Rd::Rd_get_element-error-invalid_subscripts")

    expect_Rd_tag(Rd_get_element(txt, '\\arguments'), '\\arguments')
    expect_Rd_tag(Rd_get_element(txt, '\\details'), '\\details')

    expect_error( Rd_get_element(txt, 'bibidy')
                , class = "Rd::Rd_get_element-error-not_found"
                )
    expect_error( Rd_get_element(txt, 'COMMENT')
                , class = "Rd::Rd_get_element-error-multiple_found"
                )
}
if(FALSE){#@testing Rd_get_element fringe cases
    bad.rd <- s(list( a <- Rd_comment("% This is not a valid Rd")
                    , b <- Rd_text('\n')
                    , c <- s(FALSE, Rd_tag='bad logical', class='flag')
                    ))
    expect_false(is_valid_Rd_list(bad.rd))
    expect_identical(Rd_get_element(bad.rd, 'COMMENT'), a)
    expect_identical(Rd_get_element(bad.rd, 2), b)
    expect_identical(Rd_get_element(bad.rd, 3), c)
    expect_identical(Rd_get_element(bad.rd, 'bad logical'), c)
}

#' @rdname Rd-extraction
#' @export
`[[.Rd_tag` <- function(x, ...) Rd_get_element(x, ...)
#' @rdname Rd-extraction
#' @export
`[[.Rd` <- function(x, ...) Rd_get_element(x, ...)
if(FALSE){#@testing [[.Rd & [.Rd
    test.file <- system.file("examples", "Normal.Rd", package = 'Rd')
    txt <- tools::parse_Rd(test.file)
    txt <- Rd_rm_srcref(txt)

    expect_is_exactly(txt, 'Rd')

    expect_is_exactly(txt[['\\arguments']], 'Rd_tag')
    expect_Rd_tag(txt[['\\arguments']], '\\arguments')
    expect_Rd_string(txt[['\\arguments']][[1]], 'TEXT')
    expect_Rd_string(txt[['\\arguments']][[2]], 'TEXT')
    expect_Rd_tag(txt[['\\arguments']][[3]], '\\item')
    expect_Rd_bare(txt[['\\arguments']][[c(3,1)]])
    expect_Rd_string(txt[['\\arguments']][[c(3,1,1)]], 'TEXT')
    expect_Rd_string(txt[['\\arguments']][[c(3,2,1)]], 'TEXT')

    expect_Rd_tag(txt[['\\arguments']][[3L]], '\\item')
    expect_Rd_bare(txt[['\\arguments']][[3L]][[1L]])

    expect_Rd_string(txt[[2]], "TEXT")
    expect_Rd_string(txt[[c(48, 11)]], "TEXT")
}

#' @rdname Rd-extraction
#' @export
Rd_subset <- function(x, i,... , drop=FALSE){
    if (...length())
        pkg_error( "incorrect number of subscripts"
                 , type="invalid_subscripts")
    if (is.character(i)) {
        tags <- purrr::map_chr(x, get_attr, 'Rd_tag', '')
        i <- which(tags %in% i)
        return(fwd(unclass(x)[i, drop=drop], x))
    }
    fwd(unclass(x)[i, drop=drop], x)
}
Rd_subset <- skip_scope(Rd_subset)
if(FALSE){#@testing Rd_subset
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    expect_true(is_valid_Rd_list(txt))
    expect_Rd_bare(txt)

    expect_Rd_bare(Rd_subset(txt, 1:6))
    expect_Rd_bare(Rd_subset(txt, 1))
    expect_Rd_bare(Rd_subset(txt, '\\arguments'))
    expect_length(Rd_subset(txt, 1:6), 6)
    expect_length(Rd_subset(txt, 1), 1)
    expect_length(Rd_subset(txt, '\\arguments'), 1)

    args <- Rd_get_element(txt, '\\arguments')
    expect_Rd_tag(args, '\\arguments')
    expect_Rd_tag(Rd_subset(args, 1:7), '\\arguments')
    expect_Rd_tag(Rd_subset(args, "\\item"), '\\arguments')
    expect_length(Rd_subset(args, "\\item"), 7)

    expect_true(all(purrr::map_chr(Rd_subset(args, "\\item"), get_Rd_tag) == '\\item'))
}

#' @rdname Rd-extraction
#' @export
`[.Rd` <- function(x, ...) Rd_subset(x, ...)
if(FALSE){#@testing `[.Rd` and `[.Rd_tag`
    txt <- Rd_rm_srcref(tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd')))
    expect_Rd_bare(txt)

    expect_error(txt[1,2,3], class="Rd::[.Rd-error-invalid_subscripts")

    expect_Rd_bare(val <- txt[1:2])
    expect_true(is_valid_Rd_list(val))

    expect_identical(txt['bibidy'], Rd())
    expect_identical(txt[logical(0)], Rd())
    expect_identical(txt[character(0)], Rd())
    expect_identical(txt[integer(0)], Rd())
}

#' @rdname Rd-extraction
#' @export
`[.Rd_tag` <- function(x, ...) Rd_subset(x, ...)
if(FALSE){#@testing `[.Rd` and `[.Rd_tag`
    txt <- Rd_rm_srcref(tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd')))
    expect_Rd_bare(txt)

    args <- txt[['\\arguments']]
    expect_Rd_tag(args, '\\arguments')
    expect_Rd_tag(args[1:7], '\\arguments')
    expect_length(args[1:7], 7)

    expect_Rd_tag(args["\\item"], '\\arguments')
    expect_length(args["\\item"], 7)

    expect_true(all(purrr::map_chr(Rd_subset(args, "\\item"), get_Rd_tag) == '\\item'))

    expect_equal(args[    'bibidy'], Rd_tag("\\arguments"))
    expect_equal(args[  logical(0)], Rd_tag("\\arguments"))
    expect_equal(args[character(0)], Rd_tag("\\arguments"))
    expect_equal(args[  integer(0)], Rd_tag("\\arguments"))
}

#' Combine Rd elements
#'
#' @note
#' The special considerations for Rd elements necessitate a
#' special method for combining the traditional generic [c][base::c()]
#' cannot be used due to problems that occur, particularly in the
#' [RStudio](http://rstudio.com) GUI.
#' @param ... [Rd] elements.
#' @export
Rd_c  <- function(...){
    l <- list(...)
    needs.wrap <- sapply(l, Negate(is), 'Rd')
    l[needs.wrap] <- lapply(l[needs.wrap], .Rd)
    return(fwd(unlist(l, recursive = FALSE), l[[1]]))
}
if(FALSE){#@testing
    x <- .Rd(Rd_text('testing'))

    expect_identical( Rd_c(Rd(Rd_text('  ')), Rd_alias('name'))
                    , Rd(Rd_text('  '), Rd_alias('name'))
                    )
    expect_identical( Rd_c(Rd(Rd_text('  ')), 'name')
                    , .Rd(Rd_text('  '), 'name')
                    )
    expect_identical( Rd_c(Rd(Rd_text('  ')), Rd_alias('name'), Rd_text('\n'))
                    , .Rd(Rd_text('  '), Rd_alias('name'), Rd_text('\n'))
                    )
    expect_identical( Rd_c(Rd(Rd_text('  ')), Rd_text('name'), Rd_text('\n'))
                    , .Rd(Rd_text('  '), Rd_text('name'), Rd_text('\n'))
                    )
}

#'@export
c.Rd_string <- function(...){
    l <- list(...)
    if( all(sapply(l, is.character))
     && all(unlist(sapply(l, get_Rd_tag)) %in% get_Rd_tag(..1))
      ) fwd(collapse0(l), ..1)
    else NextMethod()
}
if(FALSE){#@testing
    expect_true('c.Rd_string' %in% as.character(methods('c')))
    expect_identical(c(Rd_text("hello"), ' ', Rd_text("world"))
                    , Rd_text("hello world"))
    expect_identical(c(Rd_text("hello"), ' ', Rd_rcode("world"))
                    , c("hello", ' ', "world"))
}

#' @export
format.Rd <- function(x, ...)collapse0(as.character(x, ...))

# #' @export
# as.character.Rd_tag <- function(x, ...)as.character(.Rd(x), ...)

#' @export
format.Rd_tag <- function(x, ...)collapse0(as.character(.Rd(x), ...))

#' @export
print.Rd_tag <- function(x, ...) cat(as.character(.Rd(x), ...), sep='', collapse='')

if(FALSE){#@testing as.character, format, and print.
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    desc <- txt[["\\description"]]

    expect_Rd_tag(desc, '\\description')
    expect_is_exactly(desc, 'Rd_tag')


    expect_identical( format(.Rd(desc))
                    , "\\description{" %\%
                      "  Density, distribution function, quantile function and random" %\%
                      "  generation for the normal distribution with mean equal to \\code{mean}" %\%
                      "  and standard deviation equal to \\code{sd}." %\%
                      "}"
                    )
    expect_identical( format(desc)
                    , "\\description{" %\%
                      "  Density, distribution function, quantile function and random" %\%
                      "  generation for the normal distribution with mean equal to \\code{mean}" %\%
                      "  and standard deviation equal to \\code{sd}." %\%
                      "}"
                    )

    expect_output(print(desc)
                 , "\\description{" %\%
                   "  Density, distribution function, quantile function and random" %\%
                   "  generation for the normal distribution with mean equal to \\code{mean}" %\%
                   "  and standard deviation equal to \\code{sd}." %\%
                   "}"
                 , fixed=TRUE)
}
