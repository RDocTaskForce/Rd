#' @include util-aliases.R

# Classes Rd and Rd_tag are set in the file setup-set_old_classes.R

### Global Variables #####
.Rd.text.tags <- c("TEXT", "RCODE", "VERB", "COMMENT", "UNKNOWN", "LIST")
.Rd.newline <- s(list(s( "\n"
                       , Rd_tag="TEXT"
                       , class=c('Rd_newline', 'Rd_TEXT', 'Rd_tag', 'Rd')
                       )), class= 'Rd')
.Rd.code.newline <- s(list(s( "\n"
                       , Rd_tag="RCODE"
                       , class=c('Rd_newline', 'Rd_RCODE', 'Rd_tag', 'Rd')
                       )), class= 'Rd')
.Rd.break <- s(list(s('\n\n'
                     , Rd_tag = 'TEXT'
                     , class = c('Rd_break', 'Rd_newline', 'Rd_tag', 'Rd')
                     )), class='Rd')
.Rd.text.classes <- c('Rd_TEXT', 'Rd_indent', 'Rd_newline', 'NULL')

### Extensions to S3 class #####
Rd_get_element <- function(x, ..., drop=TRUE){
    if (...length() != 1L)
        pkg_error( "incorrect number of subscripts"
                 , type="invalid_subscripts")
    val <- if (is.character(tag <- ..1)){
        assert_that(is.string(tag) == 1)
        tags <- purrr::map_chr(x, get_attr, 'Rd_tag', '')
        i <- which(tags == tag)
        if (length(i) == 0L) pkg_error(._("tag %s not found", sQuote(tag)), type="not_found") else
        if (length(i) >= 2L) pkg_error(._("multiple elements matching tag %s found", sQuote(tag)), type="multiple_found") else
        as.list(x)[[i]]
    } else {
        i <- ..1
        unclass(x)[[...]]
    }
    if (is.null(val)) # This may be an impossible case.
        return(NULL) # nocov
    if (is(val, 'Rd'))
        return(val)
    tag <- attr(val, 'Rd_tag')
    if (is.null(tag)){
        if (is.list(val))
            return(s(val, class='Rd'))
        if (is_Rd_tag(x))
            return(val)
    }
    if (!is.null(tag)) {
        if (is.character(val) && tag %in% .Rd.text.tags) {
            class = c('Rd_' %<<<% tag, 'Rd_tag', 'Rd')
            if (val == '\n')
                return(s(val, class=c( 'Rd_newline', class)))
            else if ( is_whitespace(val)
                   && ( tail(i,1) == 1
                     || Rd_ends_with_newline(x[[c(head(i, -1), tail(i, 1)-1L)]])
                      ))
                return(s(val, class=c( 'Rd_indent', class)))
            else return(s(val, class))
        } else
            return(cl(val, c('Rd_tag', 'Rd')))
    }
    pkg_error("malformed Rd", type="malformed_Rd") # nocov
}
#' @export
`[[.Rd` <- function(...){Rd_get_element(...)}
if(FALSE){#@testing [[.Rd & [.Rd
    test.file <- system.file("examples", "Normal.Rd", package = 'Rd')
    txt <- tools::parse_Rd(test.file)
    txt <- Rd_rm_srcref(txt)
    txt <- Rd_unclass(txt)
    class(txt) <- 'Rd'

    expect_is_exactly(txt, 'Rd')

    expect_error(txt[[1,2,3]], class="Rd::Rd_get_element-error-invalid_subscripts")

    expect_is_exactly(txt[['\\arguments']], 'Rd_tag')
    expect_is_exactly(txt[['\\arguments']][[1]], 'Rd_newline')
    expect_is_exactly(txt[['\\arguments']][[2]], 'Rd_indent')
    expect_is_exactly(txt[['\\arguments']][[3]], 'Rd_tag')
    expect_is_exactly(txt[['\\arguments']][[c(3,1)]], 'Rd')
    expect_is_exactly(txt[['\\arguments']][[c(3,1,1)]], 'Rd_TEXT')
    expect_is_exactly(txt[['\\arguments']][[c(3,2,1)]], 'Rd_TEXT')

    expect_is_exactly(txt[['\\arguments']][[3L]], 'Rd_tag')
    expect_is_exactly(txt[['\\arguments']][[3L]][[1L]], 'Rd')

    expect_is_exactly(txt[[2]], "Rd_newline")
    expect_is_exactly(txt[[c(48, 11)]], "Rd_TEXT")
}

#' @export
`[.Rd` <- function(x, ..., drop=FALSE){
    if (!(...length() <= length(dim(x) %||% 1L) ))
        pkg_error( "incorrect number of subscripts"
                 , type="invalid_subscripts")
    if (is.character(tag <- ..1)) {
        tags <- purrr::map_chr(x, get_attr, 'Rd_tag', '')
        i <- which(tags %in% tag)
        return(Recall(x, i, drop=drop))
    }
    cl(NextMethod('['), 'Rd')
}
if(FALSE){#@testing [[.Rd & [.Rd
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    expect_valid(txt)
    expect_true(is.list(txt))

    expect_is(txt[[10]], "Rd_tag")
    expect_is(txt[[10]], "Rd")
    expect_valid(txt[[10]])

    expect_error(txt[1,2,3], class="Rd::[.Rd-error-invalid_subscripts")

    val <- txt[1]

    expect_is_exactly(val, 'Rd')
    expect_length(val, 1L)
    expect_true(is.list(val))
    expect_false(is.list(txt[[1]]))
    expect_valid(is.list(txt[1]))
    expect_valid(is.list(txt[[1]]))

    char <- as.character(val)
    expect_identical(char, "% File src/library/stats/man/Normal.Rd")

    expect_identical( as.character(txt[[10]])
                    , c("\\name", "{", "Normal", "}"))
    expect_identical( as.character(txt[[10]])
                    , as.character(txt[ 10 ])
                    )


    expect_is_exactly(txt[[2L]], 'Rd_newline')
    expect_is_exactly(txt[['\\arguments']], 'Rd_tag')
    expect_is_exactly(txt[['\\arguments']][[2L]], 'Rd_indent')

    expect_identical( txt[['\\seealso']]
                    , txt[[46L]]
                    )
    expect_error(txt[['bibbidy']], ._("tag %s not found", sQuote("bibbidy")))
    expect_error( txt[['\\alias']]
                , "multiple elements matching tag")

    expect_identical( txt['\\alias']
                    , txt[c(12, 14, 16, 18, 20)]
                    )
    expect_identical( class(.Rd.newline[[1]])
                    , c('Rd_newline', 'Rd_TEXT', 'Rd_tag', 'Rd')
                    )
}

`[.Rd_tag` <- function(x, ..., drop=FALSE){
    s(cl(NextMethod('['), 'Rd_tag')
     , Rd_tag=attr(x, 'Rd_tag'))
}
if(FALSE){#@testing
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))

    x <- txt[['\\arguments']]
    expect_is(x, 'Rd_tag')
    expect_identical(attr(x, 'Rd_tag'), '\\arguments')
    y <- x[1:6]
    expect_is(y, 'Rd_tag')
    expect_identical(attr(y, 'Rd_tag'), '\\arguments')
}

#' @export
c.Rd  <- function(...){
    l <- list(...)
    assert_that(all_inherit(l, 'Rd', '...'))
    is.rd <- purrr::map_lgl(l, is_exactly, 'Rd')
    # if (recursive) for(i in which(is.rd))
    #     l[[i]] <- Recall(l[[i]], recursive=recursive)
    lengths <- purrr::map_int(l, length)
    val <- vector('list', sum(ifelse(is.rd, lengths, 1L)))
    j <- 1L
    for (i in seq_along(l))
        if (is.list(l[[i]]) && is_exactly(l[[i]], 'Rd')) {
            val[seq(j, j+lengths[[i]]-1L)] <- l[[i]]
            j <- j + lengths[[i]]
        } else {
            val[j] <- l[i]
            j <- j + 1L
        }
    return(s(val, class='Rd'))
}
if(FALSE){
    x <- Rd('testing')
    l <- list(.Rd.newline, x, .Rd.newline)
    y <- c(.Rd.newline, x, .Rd.newline)

    a <- Rd('hello')
    b <- Rd(space(1))
    c <- Rd('world')
    x <- s(list(a, b, c), class='Rd')
    lst <- c(.Rd.newline, x, .Rd.newline)
    expect_is(lst, 'Rd')
    expect_length(lst, 5)
    expect_is(lst[[1]], 'Rd_newline')
    expect_is(lst[[2]], 'Rd_TEXT')
    expect_is(lst[[4]], 'Rd_TEXT')
    expect_is(lst[[5]], 'Rd_newline')

    content <- c(.Rd.code.newline
                , Rd_rcode("require(graphics)\n")
                , .Rd.code.newline
                , Rd_rcode("dnorm(0) == 1/sqrt(2*pi)\n")
                , Rd_rcode("dnorm(1) == exp(-1/2)/sqrt(2*pi)\n")
                , Rd_rcode("dnorm(1) == 1/sqrt(2*pi*exp(1))\n")
                )
    expect_is_exactly(content, 'Rd')
    expect_length(content, 6L)

    expect_is_exactly(unclass(content)[[1]],'Rd_newline')
    expect_is_exactly(unclass(content)[[3]],'Rd_newline')

    expect_is_exactly(unclass(content)[[2]],'Rd_RCODE')
    expect_is_exactly(unclass(content)[[4]],'Rd_RCODE')
    expect_is_exactly(unclass(content)[[5]],'Rd_RCODE')
    expect_is_exactly(unclass(content)[[6]],'Rd_RCODE')
}


### Testing Types #####
is_Rd_tag <-
function(x, tag = NULL){
    !is.null(. <- attr(x, 'Rd_tag')) &&
        ( is.null(tag) || . %in% tag)
}
all_are_tag <- function(x, tag=NULL){
    all(purrr::map_lgl(x, is_Rd_tag, tag=tag))
}
is_Rd_newline <- function(x){
    is(x, 'Rd_newline') ||
    ( is_exactly(x, 'Rd') && length(x) == 1L && is(x[[1L]], 'Rd_newline'))
}
if(FALSE){#@testing
    expect_true(is_Rd_newline(.Rd.newline))
    expect_true(is_Rd_newline(.Rd.newline[[1]]))
    expect_false(is_Rd_newline(.Rd.newline[[1]][[1]]))
    expect_true(is_Rd_newline(.Rd.code.newline))
    expect_true(is_Rd_newline(.Rd.code.newline[[1]]))
    expect_true(is_Rd_newline(.Rd.code.newline[[1]]))
    expect_false(is_Rd_newline(.Rd(.Rd.newline)))
}

Rd_is_all_text <- function(x, label=NULL){
    label <- label %||% deparse(substitute(x))
    assert_that(inherits(x, 'Rd'))
    if ( inherits(x, .Rd.text.classes)
      && assert_that(is.character(x))
       ) return(TRUE)
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
    expect_true(Rd_is_all_text(x[[1]]))

    y <- s(list(x), Rd_tag='test', class=c('Rd_tag', 'Rd'))

    expect_false(Rd_is_all_text(y))
    expect_identical( validate_that(Rd_is_all_text(y))
                    , "`y` has a bad element at position 1 which is not a `TEXT`" %<<%
                      "type for Rd. It is a" %<<%
                      dQuote('Rd')
                    )
    y <- s(list( Rd_rcode('some(code)')
               , s( list(Rd_symb("some"))
                  , Rd_tag="\\keyword"
                  , class=c("Rd_tag", 'Rd'))
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


### Construction Functions #####

#' Split a rd object into relevant lines.
Rd_split <-
function(x){

    assert_that(is(x, 'Rd'))
    has.newline <- purrr::map_lgl(x, Rd_ends_with_newline)
    group <- rev(cumsum(rev(has.newline)))
    group <- max(group)-group

    parts <- unname(split(x, group))
    parts <- lapply(parts, function(x)
        if (is.list(x) && length(x) == 1L) x[[1]] else x
    )
    parts
}
if(FALSE){#@testing
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))

    val <- Rd_split(txt)
    expect_is(val, 'list')
    expect_is_not(val, 'Rd')

    expect_all_inherit(val, 'Rd')
    expect_false(all_are_exactly(val, 'Rd'))


    x <- txt['\\examples'][[1]]
    y <- Rd_split(x)
    expect_identical(y[[1]], x[[1]])
    expect_length(y, 29L)
}

compact_Rd <- function(l, recurse=FALSE){
    assert_that( is(l, 'Rd')
              || ( (is.null(attr(l, 'class')) && mode(l) == 'list')
                  && all_inherit(l, c('Rd', 'NULL')))
              )
    i <- 1L
    while (i <= length(l)) {
        if (is_exactly(l[[i]], 'Rd')) {
            if (length(l[[i]]) == 1L)
                l[[i]] <- l[[i]][[1]]
            else
                l <- forward_attributes(append(l, l[[i]], after=i)[-i], l)
        } else if(recurse && is.list(l[[i]])) {
            l[[i]] <- Recall(l[[i]])
            i <- i+1L
        } else {
            i <- i+1L
        }
    }
    return(l)
}
if(FALSE){#@testing
    l <- s(list( .Rd.newline
               , Rd_text('text')
               , .Rd.newline
               , s(list(s(list(Rd_symb('symb')), Rd_tag='tag'
                         , class=c('Rd_tag', 'Rd'))
                       , .Rd.newline
                       ), class='Rd')
               ), class='Rd')
    m <-  compact_Rd(l, recurse=TRUE)

    expect_length(l, 4L)
    expect_length(m, 5L)

    expect_is(m, 'Rd')
    expect_is(m[[1L]], 'Rd_newline')
    expect_is(m[[2L]], 'Rd_TEXT')
    expect_is(m[[3L]], 'Rd_newline')
    expect_is(m[[4L]], 'Rd_tag')
    expect_is(m[[5L]], 'Rd_newline')
}

.Rd <- function(...)s(list(...), class='Rd')

#' @export
Rd <-
function( ... #< elements of Rd.
        , wrap.lines     = FALSE
        , wrap.at        = 72L
        ){
    if (...length() == 0L) return(invisible(cl(list(), 'Rd'))) else
    if (...length() == 1L) {
        if (is_exactly(..1, 'Rd')) return(..1)
        if (inherits(..1, 'character')) {
            val <- .Rd_strwrap(..1, wrap.lines=wrap.lines, wrap.at=wrap.at)
            return(val %if% (is_exactly(val, 'Rd'))
                   %otherwise% s( list(val), class= 'Rd'))
        }
    }
    l <- list(...)

    for (i in seq_along(l)) if (is_exactly(l[[i]], 'character'))
        l[[i]] <- Rd_text(l[[i]])

    assert_that(all_inherit(l, 'Rd', '`...`'))
    compact_Rd(s(l, class='Rd'))
}
if(FALSE){#@testing
    a <- "test"
    b <- Rd(a)
    expect_is_exactly(b, 'Rd')
    expect_is(b[[1]], 'Rd_TEXT')

    a <- stringi::stri_rand_lipsum(3)
    b <- Rd(collapse(a, '\n\n'), wrap.lines=TRUE)
    expect_is_exactly(b, 'Rd')
    expect_identical(mode(b), 'list')
    expect_true(length(b) > 5)

    c <- Rd(a, wrap.lines=FALSE)
    expect_is_exactly(c, 'Rd')
    d <- Rd(c, wrap.lines=TRUE)
    expect_is_exactly(d, 'Rd')
    expect_identical(c, d)

    expect_error(Rd(NULL))

    expect_is(Rd(), 'Rd')
    expect_length(Rd(), 0L)

    x <- Rd(collapse(stringi::stri_rand_lipsum(3), '\n\n'), wrap.lines=TRUE)
    expect_is_exactly(x, 'Rd')
    expect_is_exactly(x[[1L]], 'Rd_TEXT')
    expect_true(all_inherit(x, c('Rd_TEXT', 'Rd_newline')))

    x <- Rd(Rd_text('text'))
    expect_is_exactly(x, 'Rd')
    expect_is_exactly(x[[1]], 'Rd_TEXT')

    x <- Rd("Multiple ", "Character strings", " to convert")
    expect_is_exactly(x, 'Rd')
    expect_all_inherit(x, "Rd_TEXT")
    expect_true(all_are_tag(x, 'TEXT'))
}
if(FALSE){#@testing Class-Rd
    x <- cl('text', 'Rd')
    expect_is(x, 'Rd')

    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    expect_is(txt, 'Rd')
    expect_true(validObject(txt))
}

setValidity('Rd', function(object){
    if (identical(class(object), 'Rd'))
        validate_that( is.list(object)
                     , is.null(attr(object, 'Rd_tag'))
                     , all_inherit(object, c( 'list', 'character'
                                            , 'Rd', 'Rd_tag'))
                     )
    else
        validate_that( is.list(object) || is.character(object) )
})
setValidity('Rd_tag', function(object){
    validate_that( is.list(object) || is.character(object)
                 , !is.null(tag <- attr(object, 'Rd_tag'))
                 , grepl('^\\\\', tag) || tag %in% .Rd.text.classes
                 )
})

Rd_text <-
function( content
        , type = c("TEXT", "RCODE", "VERB", "COMMENT", "UNKNOWN", "LIST")
        , opt = NULL
        ){
    type <- match.arg(type)
    assert_that( is.character(content)
               , length(opt) == 0L || inherits(opt, 'Rd')
               )
    s( content
     , Rd_tag=type
     , class = c(paste0('Rd_', type), 'Rd_tag', 'Rd')
     )
}
if(FALSE){#@testing
    val <- Rd_text('testing')
    expect_is(val, 'Rd')
    expect_is(val, 'Rd_tag')
    expect_is(val, 'Rd_TEXT')

    expect_true(is.character(val))
    expect_false(is.list(val))

    val <- Rd_text('some(code)', 'RCODE')
    expect_is(val, 'Rd_RCODE')
    val <- Rd_text('some(code)', 'R')
    expect_is(val, 'Rd_RCODE')

    x <- Rd_text(collapse(stringi::stri_rand_lipsum(3), '\n\n'))
    expect_is(x, 'Rd')
    expect_is(x, 'Rd_tag')
    expect_is(x, 'Rd_TEXT')

    x <- Rd_text(c( 'hello', '\n', ' big', '\n', '  wide', '\n', '   world'))
    expect_is(x, 'Rd')
    expect_is(x, 'Rd_TEXT')

    expect_length(x, 7L)
    # expect_all_inherit(x[c(3,6,9,12)], 'Rd_newline')
    # expect_all_inherit(x[c(4,7,10)], 'Rd_indent')
    # expect_all_inherit(x[-1], c('Rd_TEXT', 'Rd_newline'))

    x <- Rd_text("     hello world")
    expect_is(x, 'Rd')
    expect_is(x, 'Rd_TEXT')
    expect_length(x, 1L)
    # expect_is(x[[1]], 'Rd_indent')
    # expect_is(x[[2]], 'Rd_TEXT')
    # expect_is(x[[3]], 'Rd_newline')
}
Rd_rcode <- function(content, opt=NULL){
    assert_that( is.character(content)
               , length(opt) == 0L || inherits(opt, 'Rd')
               )
    s( content
     , Rd_tag='RCODE'
     , class = c('Rd_RCODE', 'Rd_tag', 'Rd')
     )
}
Rd_symb <- function(...)Rd_text(..., type='VERB')
Rd_comment <- function(...){
    assert_that(all(purrr::map_lgl(list(...), grepl, pattern='^%'))
               , msg = "Ill-formed comments")
    Rd_text(..., type="COMMENT")
}
if(FALSE){#@testing Rd_rcode, Rd_symb, and Rd_comment
    expect_error(Rd_comment("testing"))
    expect_is(Rd_comment("% comment"), "Rd_COMMENT")
    expect_equal(attr(Rd_comment("% comment"), 'Rd_tag'), "COMMENT")
    expect_is(Rd_rcode("some(code)"), "Rd_RCODE")
    expect_equal(attr(Rd_rcode("some(code)"), 'Rd_tag'), "RCODE")
    expect_is(Rd_symb("name"), "Rd_VERB")
    expect_equal(attr(Rd_symb("name"), 'Rd_tag'), "VERB")

    a <- Rd_rcode("require(graphics)\n")
    expect_is_exactly(a, 'Rd_RCODE')
    expect_length(a, 1L)
}

Rd_tag  <-
function( tag
        , ...
        , content=.Rd(...)
        , opt = Rd()
        , indent         = FALSE
        , indent.with    = .Rd.default.indent
        , wrap.lines     = FALSE
        , wrap.at        = 72L
        ){
    assert_that( is.string(tag)
               , length(opt) == 0L || inherits(opt, 'Rd')
               )
    for (i in seq_along(content))
        if (is_exactly(content[[i]], c('character', 'Rd_TEXT')))
            content[[i]] <- .Rd_strwrap(content[[i]], wrap.lines=wrap.lines, wrap.at=wrap.at)
    assert_that(all_inherit(content, 'Rd', '`...`'))
    if (Rd_spans_multiple_lines(content)) {
        if (!Rd_starts_with_newline(content)) content <- c(.Rd.newline, content)
        if (!Rd_ends_with_newline(content)) content <- c(content, .Rd.newline)
        if (indent)
            content <- .Rd_indent(content, indent=indent, indent.with = indent.with)
    }
    return(s( content
            , Rd_tag = "\\" %<<<% tag
            , class  = c('Rd_tag', 'Rd')
            , Rd_option = opt %if% length(opt)
            ))
}
if(FALSE){#! @testing
    expect_error(Rd_tag(NULL, 'test'), "tag is not a string")
    expect_error(Rd_tag(c('a', 'b'), 'test'), "tag is not a string")
    expect_error(Rd_tag(1, 'test'), "tag is not a string")
    expect_is(Rd_tag('name', Rd_text('my name')), "Rd_tag")
    expect_is(Rd_tag('name', Rd_text('my name')), "Rd")
    expect_identical( Rd_tag('name', Rd_text('my name'))
                    , s( list(Rd_text("my name"))
                       , Rd_tag = "\\name"
                       , class  = c('Rd_tag', 'Rd')
                       ))

    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    e <- txt[['\\arguments']][[3L]]
    attr(e, 'srcref') <- NULL
    for (i in seq_along(e)) attr(e[[i]][[1L]], 'srcref') <- NULL

    expect_identical(mode(e), 'list')
    expect_length(e, 2)

    x <- Rd_tag('item', Rd(Rd_text('arg')), Rd(Rd_text("an agrument")))
    expect_length(x, 2L)

    expect_equal( as.character(x <- Rd_tag('name', Rd_text(c('line1', 'line2'))))
                , c('\\name', '{', 'line1', 'line2', '}')
                )

    val <- Rd_tag('link', Rd_text('dest'), opt=Rd_text('pkg'))
    expect_is(val, 'Rd')
    expect_identical(collapse0(as.character(val)), "\\link[pkg]{dest}")


    content <- Rd_canonize(Rd(collapse(stringi::stri_rand_lipsum(3), '\n\n')))
    tag <- Rd_tag( 'description', content=content
                 , wrap.lines = TRUE, wrap.at = 72
                 , indent=TRUE, indent.with = ' '
                 )
    expect_true(is_Rd_tag(tag, '\\description'))
    expect_true(length(tag) > 5L)
    expect_equal(substr(tag[[2]], 1, 13)[[1]], '  Lorem ipsum')
}


