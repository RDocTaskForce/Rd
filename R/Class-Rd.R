#' @include util-aliases.R

# Classes Rd and Rd_tag are set in the file setup-set_old_classes.R

### Testing Types #####
#' @name testing-Rd
#' @title
#' Testing Rd types
#'
#' These provide methods for testing if an object is valid.
#'
#' @param x object to test
#' @param strict if the class must be set. A value of NA indicates that the
#'               first level need not be classed but all subsequent must be.
#' @param tags the type of tag(s) allowed in the `Rd_tag` attribute.
#' @param reason should the reason for failure be included.
#'               Used in [assertthat::assert_that()] and related functions.
NULL

#' @describeIn testing-Rd test if object is a valid Rd string type.
#' @export
is_Rd_string <-
function(x, tags=NULL, strict=FALSE, reason=TRUE){
    tags <- if (is.null(tags)) .Rd.string.tags else
        match.arg(tags, .Rd.string.tags, several.ok = TRUE)

    if (strict && !inherits(x, 'Rd_string'))
        return(s(FALSE, msg=if(reason)"not of class 'Rd_string'"))
    if (!is.character(x))
        return(s(FALSE, msg=if(reason)"not of mode character"))
    if (length(x)!= 1 || is.na(x))
        return(s(FALSE, msg=if(reason)"not a non-empty string"))
    tag <- attr(x, 'Rd_tag')
    if (is.null(tag))
        return(s(FALSE, msg=if(reason)"does not have an 'Rd_tag' attribute"))
    if (tag %!in% tags)
        return(s(FALSE, msg=if(reason)"'Rd_tag' attribute is not in allowed tags"))
    return(TRUE)
}
if(FALSE){#@testing
    expect_error(is_Rd_string('x', tags = "\\tag"))

    expect_identical(is_Rd_string(list(), strict=TRUE, reason=FALSE), FALSE)
    expect_identical(is_Rd_string(list(), strict=TRUE)
                    , s(FALSE, msg="not of class 'Rd_string'"))
    expect_identical(is_Rd_string(list(), strict=FALSE, reason=FALSE), FALSE)
    expect_identical(is_Rd_string(list(), strict=FALSE)
                    , s(FALSE, msg="not of mode character"))
    expect_identical(is_Rd_string(c('a','b'), strict=FALSE, reason=FALSE), FALSE)
    expect_identical(is_Rd_string(c('a','b'), strict=FALSE)
                    , s(FALSE, msg="not a non-empty string"))
    expect_identical(is_Rd_string('a', strict=FALSE, tags='TEXT', reason=FALSE), FALSE)
    expect_identical(is_Rd_string('a', strict=FALSE)
                    , s(FALSE, msg="does not have an 'Rd_tag' attribute"))
    x <- s("test", Rd_tag = 'VERB')
    expect_true(is_Rd_string(x, strict=FALSE))
    expect_identical(is_Rd_string(x, strict=FALSE, tags='TEXT', reason=FALSE), FALSE)
    expect_identical(is_Rd_string(x, strict=FALSE, tags='TEXT')
                    , s(FALSE, msg="'Rd_tag' attribute is not in allowed tags"))

    class(x) <- 'Rd_string'
    expect_true(is_Rd_string(x, strict=TRUE))
}
#' @describeIn testing-Rd Explicitly vectorized version of is_Rd_string
#' @export
are_Rd_strings <-
function(x, tags=NULL, strict=FALSE){
    purrr::map_lgl(x, is_Rd_string, tags=tags, strict=strict, reason=FALSE)
}
if(FALSE){#@testing
    x <- list( s(list(), Rd_tag='\\item')
             , s(list(), Rd_tag='\\dots', class='Rd_tag')
             , s('test', Rd_tag='VERB')
             , s('this', Rd_tag='TEXT', class='Rd_string')
             )
    expect_identical( are_Rd_strings(x)
                    , c(FALSE, FALSE, TRUE, TRUE)
                    )
    expect_identical( are_Rd_strings(x, tags="VERB")
                    , c(FALSE, FALSE, TRUE, FALSE)
                    )
    expect_identical( are_Rd_strings(x, strict=TRUE)
                    , c(FALSE, FALSE, FALSE, TRUE)
                    )
}


#' @describeIn testing-Rd Check if a list is a valid `Rd_tag` object.
#' @export
is_Rd_tag <-
function(x, tags = NULL, strict=FALSE, reason=TRUE){
    assert_that( is.null(tags)
              || all(grepl("^\\\\", tags))
               , msg = "All tags must start with '\\'")
    if (strict && !inherits(x, 'Rd_tag'))
        return(s(FALSE, msg = if(reason)"is not of class 'Rd_tag'"))
    if (!is.list(x))
        return(s(FALSE, msg = if(reason)"is not a list"))
    x.tag <- attr(x, 'Rd_tag')
    if (is.null(x.tag))
        return(s(FALSE, msg = if(reason)"does not have an 'Rd_tag' attribute"))
    if (!grepl("^\\\\", x.tag))
        return(s(FALSE, msg = if(reason)"'Rd_tag' attribute is invalid"))
    if (!is.null(tags) && x.tag %!in% tags)
        return(s(FALSE, msg = if(reason)"'Rd_tag' attribute is not in allowed tags"))
    return(TRUE)
}
if(FALSE){#@testing
    expect_error( is_Rd_tag(list(), tag='TEXT')
                , class = "Rd-error-assertion failure"
                )
    expect_identical(is_Rd_tag(list(), strict=TRUE, reason=FALSE), FALSE)
    expect_identical(is_Rd_tag(list(), strict=TRUE)
                    , s(FALSE, msg="is not of class 'Rd_tag'"))
    expect_identical(is_Rd_tag(list(), strict=FALSE, reason=FALSE), FALSE)
    expect_identical(is_Rd_tag(list(), strict=FALSE)
                    , s(FALSE, msg="does not have an 'Rd_tag' attribute"))
    expect_identical(is_Rd_tag(character(), strict=FALSE, reason=FALSE), FALSE)
    expect_identical(is_Rd_tag(character(), strict=FALSE)
                    , s(FALSE, msg="is not a list"))
    x <- s(list(), Rd_tag = "item")
    expect_identical(is_Rd_tag(x, strict=TRUE, reason=FALSE), FALSE)
    expect_identical(is_Rd_tag(x, strict=TRUE)
                    , s(FALSE, msg="is not of class 'Rd_tag'"))
    expect_identical( is_Rd_tag(x, strict=FALSE, reason=FALSE), FALSE)
    expect_identical( is_Rd_tag(x, strict=FALSE)
                    , s(FALSE, msg="'Rd_tag' attribute is invalid"))

    x <- s(list(), Rd_tag = "\\item")
    expect_true( is_Rd_tag(x) )
    expect_identical( is_Rd_tag(x, strict=FALSE, tag = c("\\details", "\\usage"), reason=FALSE), FALSE)
    expect_identical( is_Rd_tag(x, strict=FALSE, tag = c("\\details", "\\usage"))
                    , s(FALSE, msg="'Rd_tag' attribute is not in allowed tags"))
}
#' @describeIn testing-Rd Explicitly vectorized version of is_Rd_tag
#' @export
are_Rd_tags <- function(x, tags=NULL, strict=FALSE){
    purrr::map_lgl(x, is_Rd_tag, tags=tags, strict=strict, reason=FALSE)
}
if(FALSE){#@testing
    x <- list( s(list(), Rd_tag='\\item')
             , s(list(), Rd_tag='\\dots', class='Rd_tag')
             , s('test', Rd_tag='VERB')
             , s('this', Rd_tag='TEXT', class='Rd_string')
             )
    expect_identical( are_Rd_tags(x)
                    , c(TRUE, TRUE, FALSE, FALSE)
                    )
    expect_identical( are_Rd_tags(x, tags="\\dots")
                    , c(FALSE, TRUE, FALSE, FALSE)
                    )
    expect_identical( are_Rd_tags(x, strict=TRUE)
                    , c(FALSE, TRUE, FALSE, FALSE)
                    )
}
#' @describeIn testing-Rd check if a list is an Rd container object.
#'
is_Rd <- function(x, strict=FALSE){
    see_if( !strict || inherits(x, 'Rd')
          , is.list(x)
          , is.null(attr(x, 'Rd_tag'))
          )
}
if(FALSE){#@testing
    x <- s(list( s("% comment", Rd_tag="COMMENT")
               , s(list( s("TEST", Rd_tag="VERB"))
                  , Rd_tag="\\name")
               ), class='Rd')
    expect_true(is_Rd(x))
    expect_true(is_Rd(unclass(x)))
    expect_true(is_Rd(x, strict=TRUE))
    expect_identical(is_Rd(list(), strict=TRUE)
                    , s(FALSE, msg="!strict is not TRUE or x does not inherit from class Rd"))
    expect_identical(is_Rd(unclass(x), strict=TRUE)
                    , s(FALSE, msg="!strict is not TRUE or x does not inherit from class Rd"))
    expect_identical(is_Rd(character(0), strict=FALSE)
                    , s(FALSE, msg="x is not a list"))
    expect_true(is_Rd(list()))

    expect_identical(is_Rd(unclass(x)[[2]])
                    , s(FALSE, msg='attr(x, "Rd_tag") is not NULL'))

}

#' @describeIn testing-Rd Check that an object is a valid Rd list,
#'                        an `Rd_tag` or `Rd`, but not an `Rd_string`
is_valid_Rd_list <- function(x, tags=NULL, strict=FALSE, deep=!isTRUE(strict) || !missing(tags)){
    if (is.character(x)) return(FALSE) else
    valid <- if (is.list(x) && !is.null(attr(x, 'Rd_tag'))){
        is_Rd_tag(x, tags=setdiff(tags, .Rd.string.tags), strict=isTRUE(strict), reason=FALSE)
    } else {
        is_Rd(x, strict=isTRUE(strict))
    }
    if(!valid || !deep) return(valid)
    elements.are.valid <- purrr::map_lgl(x, is_valid_Rd_object, tags=tags, strict=!isFALSE(strict), deep=deep)
    see_if(all(elements.are.valid))
}

#' @describeIn testing-Rd Check that an object is valid
is_valid_Rd_object <- function(x, tags=NULL, strict=FALSE, deep=!isTRUE(strict) || !missing(tags)){
    if (is.character(x)){
        return(is_Rd_string(x, tags=intersect(.Rd.string.tags, tags), strict=isTRUE(strict), reason=FALSE))
    } else
    see_if(is_valid_Rd_list(x, tags=tags, strict=strict, deep=deep))
}
if(FALSE){#@testing
    expect_true(is_valid_Rd_object(list(), strict=FALSE))
    expect_false(is_valid_Rd_object(list(), strict=TRUE))

    x <- list( s("% comment", Rd_tag="COMMENT")
             , s(list( s("TEST", Rd_tag="VERB"))
                , Rd_tag="\\name")
             )
    expect_true(is_valid_Rd_object(x[[1]], strict=FALSE))
    expect_true(is_valid_Rd_object(x[[2]], strict=FALSE))
    expect_true(is_valid_Rd_object(x     , strict=FALSE))

    expect_false(is_valid_Rd_object(x[[1]], strict=TRUE))
    expect_false(is_valid_Rd_object(x[[2]], strict=TRUE))
    expect_false(is_valid_Rd_object(x     , strict=TRUE))

    expect_identical( validate_that(is_valid_Rd_object(c(x, TRUE), strict=FALSE))
                    , "Elements 3 of elements.are.valid are not true")
    expect_identical( validate_that(is_valid_Rd_object(c(x, TRUE), strict=NA))
                    , "Elements 1, 2, 3 of elements.are.valid are not true")
}
if(FALSE){#@testing is_valid_Rd_object against parse_Rd results
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    expect_true(is_valid_Rd_object(txt))
    expect_true(is_valid_Rd_object(txt, strict=FALSE, deep=TRUE))
}


# is_Rd_newline <- function(x, ...){
#     is_Rd_string(x, ...) && x == '\n'
# }
# if(FALSE){#@testing
#     expect_true(is_Rd_newline(.Rd.newline))
#     expect_true(is_Rd_newline(.Rd.newline[[1]]))
#     expect_false(is_Rd_newline(.Rd.newline[[1]][[1]]))
#     expect_true(is_Rd_newline(.Rd.code.newline))
#     expect_true(is_Rd_newline(.Rd.code.newline[[1]]))
#     expect_true(is_Rd_newline(.Rd.code.newline[[1]]))
#     expect_false(is_Rd_newline(.Rd(.Rd.newline)))
# }

## Text Testing

Rd_is_all_text <- function(x, label=NULL){
    label <- label %||% deparse(substitute(x))
    if(!is_Rd(x) && !is_Rd_tag(x))
        return(s(FALSE, msg="x is not an Rd container"))
    is.text <- sapply(x, inherits, .Rd.text.classes)
    if (all(is.text)) return(TRUE)
    bad.elements = which(!is.text)
    msg <- if (length(bad.elements) > 1L) {
        ._("`%s` has bad elements at positions %s which are not a `TEXT` type for Rd"
          , label
          , comma_list(bad.elements)
          )
    } else {
        bad.class <- purrr::map_chr(x[bad.elements], class0)
        ._("`%s` has a bad element at position %s which is not a `TEXT` type for Rd. It is a %s"
          , label
          , comma_list(bad.elements)
          , dQuote(bad.class)
          )
    }
    return(s(FALSE, msg, bad.elements))
}
if(FALSE){#@testing
    x <- c("Lorem ipsum", stringi::stri_rand_lipsum(3, start_lipsum = FALSE))
    x <- .Rd_strwrap(collapse(x, '\n\n'), wrap.lines = TRUE, wrap.at = 50)
    expect_is(x, "Rd")
    expect_is_not(x, "Rd_TEXT")

    expect_true(Rd_is_all_text(x))
    expect_identical( Rd_is_all_text(x[[1]])
                    , s(FALSE, msg="x is not an Rd container")
                    )

    y <- s(list(x), Rd_tag='\\test', class=c('Rd_tag'))

    expect_false(Rd_is_all_text(y))
    expect_identical( validate_that(Rd_is_all_text(y))
                    , "`y` has a bad element at position 1 which is not a `TEXT`" %<<%
                      "type for Rd. It is a" %<<%
                      dQuote('Rd')
                    )
    y <- s(list( Rd_rcode('some(code)')
               , s(list(Rd_symb("some"))
                  , Rd_tag="\\keyword"
                  , class=c("Rd_tag"))
               ), class='Rd')
    expect_identical( validate_that(Rd_is_all_text(y))
                    , "`y` has bad elements at positions 1 and 2 which are not a `TEXT`" %<<%
                      "type for Rd"
                    )
}

Rd_spans_multiple_lines <- function(x){
    grepl('\\n(?!$)', collapse0(as.character(x)), perl=TRUE)
}
if(FALSE){#@testing
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    expect_true(Rd_spans_multiple_lines(txt))
    expect_true(Rd_spans_multiple_lines(txt[['\\arguments']]))
    expect_false(Rd_spans_multiple_lines(txt[['\\arguments']][[3L]]))

    expect_false(Rd_spans_multiple_lines(Rd_text("hello world\n")))
    expect_true(Rd_spans_multiple_lines(Rd_text("hello\nworld\n")))

    x <- txt[[38]][2]
    expect_true(Rd_spans_multiple_lines(x))
    expect_false(Rd_spans_multiple_lines(unclass(x)))

    x <- c(.Rd.code.newline
          , Rd_rcode('value \\%if\\% proposition')
          , .Rd.code.newline)
    expect_true(Rd_spans_multiple_lines(x))
}

Rd_ends_with_newline <- function(x, keep.class=FALSE){
    grepl('\\n$', collapse0(as.character(if (keep.class) x else unclass(x))))
}
if(FALSE){#@testing
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    expect_true(Rd_ends_with_newline(txt))

    # ends.with.newline <- purrr::map_lgl(txt, Rd_ends_with_newline)
    # spans.multiple.lines <- purrr::map_lgl(txt, Rd_spans_multiple_lines)
    # expect_false(any(ends.with.newline & !spans.multiple.lines))

    x <- txt[[38]]

    expect_true(Rd_ends_with_newline(x))
    expect_false(Rd_ends_with_newline(x, TRUE))
}

Rd_starts_with_newline <- function(x, keep.class=FALSE){
    grepl('^\\n', collapse0(as.character(if (keep.class) x else unclass(x))))
}
if(FALSE){#@testing
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    expect_false(Rd_starts_with_newline(txt))
    expect_true(Rd_starts_with_newline(txt[['\\arguments']]))
    expect_false(Rd_starts_with_newline(txt[['\\arguments']], TRUE))
}



