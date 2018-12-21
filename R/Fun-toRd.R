#' @include Class-Rd.R
#' @importFrom tools toRd



### toRd #######################################################################
#' @export
setGeneric('toRd',
function(obj, ...){
    ans <- standardGeneric("toRd")
    if (is_exactly(ans, 'character')) ans <- Rd_canonize_text(Rd_text(ans))
    if (is.character(ans)) ans <- Rd_canonize(Rd(ans))
    if (is_exactly(ans, 'Rd')) return(ans)
    if (is(ans, 'Rd')) return(s(list(ans), class='Rd'))
    else
        pkg_error(._( "Method of generic function %1$s for class %2$s" %<<%
                      "returned a %4$s." %<<%
                      "Methods of %1$s are expected to retun a %3$s vector."
                    , sQuote("toRd"), dQuote(class(obj))
                    , 'character', dQuote(class(ans))
                    ))
})
toRd@valueClass <- 'Rd'
if(FALSE){# testing INACTIVE
    val <- toRd('character')
    expect_identical(val, Rd("character"))

    val <- toRd(c( "use the \\backslash to escape."
                 , "and '{}' to group."
                 ))
    expect_is(val, 'Rd')
    expect_identical( val
                    , Rd(Rd_text("use the \\\\backslash to escape." %<<<%
                                 "and '\\{\\}' to group."
                                 )))
    obj <-
        c( person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                                  , person('Drew'  , 'Blue')
                                  )
    expect_identical( toRd(obj)
                    , Rd(Rd_text("Andrew Redd \\email{andrew.redd@hsc.utah.edu} and Drew Blue"))
                    )
}
if(FALSE){# testing INACTIVE
    toRd.test_class <- function(obj, ...)obj
    expect_error( toRd(cl(1L, 'test_class'))
                , class = "Rd-error")
}
# set_option_documentation( "documentation::Rd::indent"
#    , description = "Determines if code should be indented when formatted.  Should default to FALSE when unset."
#    , default = FALSE
#    , constraints = list(is.logical)
#    )
# set_option_documentation( "documentation::Rd::indent.with"
#    , description = "Determines what to indent with, when getOption('documentation::toRd::indent') is TRUE."
#    , default = "  "
#    , constraints = list(~is.string(.))
#    )

# Helpers ============================================
.Rd.default.indent <- s(list(s( "    "
                              , Rd_tag="TEXT"
                              , class=c('Rd_indent', 'Rd_TEXT', 'Rd_tag', 'Rd')
                              )), class= 'Rd')


Rd_clean_indent <-
function(indent.with){
    if (is_exactly(indent.with, 'character'))
        indent.with <- Rd(cl(Rd_text(indent.with), 'Rd_indent'))
    if (is_exactly(indent.with, c('Rd_text', 'Rd_rcode')))
        indent.with <- cl(indent.with, 'Rd_indent')
    if (is(indent.with, 'Rd_indent'))
        indent.with <- Rd(indent.with)
    if (is(indent.with, 'Rd')){
        assert_that( length(indent.with) == 1L)
        if (is_exactly(indent.with[[1L]], 'Rd_text'))
            indent.with[[1L]] <- cl(indent.with[[1L]], 'Rd_indent')
        assert_that(is(indent.with[[1L]], 'Rd_indent'))
        if (grepl('\\t', indent.with))
            pkg_warning( type='guidelines_violation'
                         , ._("Tabs are discouraged from being used for indentation" %<<%
                              "as they may not be rendered properly on all possible pagers." %<<%
                              "See https://developer.r-project.org/Rds.html for reference.")
            )
        if (any(grepl('\\n', as.character(indent.with))))
            pkg_error( ._("Newlines are not allowed in indent.with"))
        return(indent.with)
    }
    pkg_error("bad indent")
}

.Rd_indent <-
function( x   #< Rd object.
        , ... #< Ignored.
        , indent
        , indent.with
        , indent.first = !is(x, 'Rd_tag')
        ){
    if (!indent) return(x)
    indent.with <- Rd_clean_indent(indent.with)
    if (is.character(x)) {
        if (is_exactly(x, 'Rd_TEXT') || is_exactly(x, 'Rd_RCODE') || is_exactly(x, 'Rd_indent'))
            return(forward_attributes(paste0(indent.with, x),x))
        else if (is_exactly(x, 'Rd_newline')) return(x)
        else
            pkg_error("Unknown Rd type" %<<% sQuote(collapse(class(x), '/')))
    }
    assert_that(is.list(x))
    if (!Rd_spans_multiple_lines(x)) return(x)
    parts <- Rd_split(Rd_untag(x))
    if (length(parts) == 1L){
        parts <- lapply( if (indent.first) Rd_untag(x)
                         else tail(Rd_untag(x), -1L)
                       , .Rd_indent
                       , indent=indent
                       , indent.with=indent.with
                       , indent.first = indent.first && !is(x, 'Rd_tag'))
        if (!indent.first)
            parts <- c(unclass(x)[1], parts)
        return(forward_attributes(parts, x))
    }
    for (i in seq_along(parts)) if (Rd_spans_multiple_lines(parts[[i]]))
        parts[[i]] <- .Rd_indent(parts[[i]]
                                , indent=indent
                                , indent.with=indent.with
                                , indent.first = indent.first && !is(x, 'Rd_tag'))
    is.nl <- purrr::map_lgl(parts, is_Rd_newline)
    is.code <- purrr::map_lgl(parts, is, 'Rd_RCODE')
    indent.code <- Rd(cl(Rd_rcode(as.character(indent.with)), 'Rd_indent'))
    indents <- ifelse(is.nl, Rd(), ifelse(is.code, indent.code, indent.with))
    if (!indent.first)
        indents[[1]] <- Rd()
    val <- rbind(indents, parts)
    val <- purrr::compact(undim(val))
    val <- cl(val, 'Rd')
    val <- forward_attributes(val, x)
    val <- Rd_canonize_text(val)
    val <- Rd_canonize_code(val)
    return(val)
}
if(FALSE){# testing INACTIVE
    x <- .Rd_strwrap( collapse(stringi::stri_rand_lipsum(3), '\n\n')
                    , wrap.lines = TRUE, wrap.at = 72)
    expect_is(x, 'Rd')
    expect_true(length(x)> 5)

    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    txt <- Rd_rm_srcref(txt)
    x <- txt[['\\examples']]

    expect_identical(.Rd_indent(x, indent=FALSE), x)

    val <- .Rd_indent(x, indent=TRUE, indent.with = "    ")
    expect_is(val, 'Rd')
    expect_is(val, 'Rd_tag')
    expect_identical( stringi::stri_split_lines1(collapse0(as.character(val)))
                    , stringi::stri_split_lines1(collapse0(as.character(x))) %>%
                        ifelse(. %in% c('\\examples{', '}', ''), ., paste0("    ", .))
                    )
    expect_equal(length(val), length(x))

    x <- txt[['\\arguments']][[9]]
    val <- .Rd_indent(x, indent=TRUE, indent.with='  ')
    exp <- Rd_item('n', Rd( x[[2]][1:3]
                          , Rd_text(paste0('  ', x[[2]][[4]]))
                          ))
    expect_identical(val, exp)

    x <- Rd_untag(txt[['\\arguments']][8:10])
    val <- .Rd_indent(x, indent=TRUE, indent.with='  ', indent.first=FALSE)
    expect_equal(collapse0(val)
                , "  \\item{n}{number of observations. If \\code{length(n) > 1}, the length" %\%
                  "      is taken to be the number required.}\n" )

    x <- txt[['\\arguments']]
    val <- .Rd_indent(x, indent=TRUE, indent.with='  ')
    expect_equal( collapse0(val)
                , "\\arguments{" %\%
                  "    \\item{x, q}{vector of quantiles.}" %\%
                  "    \\item{p}{vector of probabilities.}" %\%
                  "    \\item{n}{number of observations. If \\code{length(n) > 1}, the length" %\%
                  "      is taken to be the number required.}" %\%
                  "    \\item{mean}{vector of means.}" %\%
                  "    \\item{sd}{vector of standard deviations.}" %\%
                  "    \\item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}" %\%
                  "    \\item{lower.tail}{logical; if TRUE (default), probabilities are" %\%
                  "      \\eqn{P[X \\le x]} otherwise, \\eqn{P[X > x]}.}" %\%
                  "}"
                )
}
if(FALSE){# deprecated testing .Rd_indent specification by options
    x <- Rd_canonize(Rd(Rd_text(c("test strings\n", "second line"))))
    withr::with_options(list( "documentation::Rd::indent" = TRUE
                            , "documentation::Rd::indent.with" = NULL), {
        expect_identical( Rd_canonize(x)
                        , Rd(Rd_text( .Rd.default.indent %<<<% "test strings\n")
                            ,Rd_text( .Rd.default.indent %<<<% "second line")
                            ))
    })
    withr::with_options(list( "documentation::indent" = TRUE), {
        expect_identical( Rd_canonize(x)
                        , Rd( Rd_text( .Rd.default.indent %<<<% "test strings\n")
                            , Rd_text( .Rd.default.indent %<<<% "second line")
                            ))
    })
    withr::with_options(list( "indent" = TRUE), {
        expect_identical( Rd_canonize(x)
                        , Rd( Rd_text( .Rd.default.indent %<<<% "test strings\n")
                            , Rd_text( .Rd.default.indent %<<<% "second line")
                            ))
    })
    withr::with_options(list( "documentation::Rd::indent" = TRUE
                            , "documentation::Rd::indent.with" = "   "), {
        expect_identical( Rd_canonize(x)
                        , Rd( Rd_text( "   test strings\n")
                            , Rd_text( "   second line")
                            ))
    })
    withr::with_options(list( "documentation::Rd::indent" = TRUE
                            , "documentation::Rd::indent.with" = "\t"), {
        expect_warning( Rd_canonize(x)
                      , class= "documentation-warning-guidelines_violation"
                      )
    })
}
if(FALSE){# testing INACTIVE indenting first lines
    expect_error(.Rd_indent(c('test strings')))

    x <- Rd_usage( .Rd.code.newline
                 , Rd_rcode('value \\%if\\% proposition\n')
                 , Rd_rcode('value \\%if\\% proposition \\%otherwise\\% alternate\n')
                 )
    val<- .Rd_indent(x=x, indent=TRUE, indent.with = '  ')
    expect_identical( val
                    , Rd_usage( .Rd.code.newline
                              , Rd_rcode('  value \\%if\\% proposition\n')
                              , Rd_rcode('  value \\%if\\% proposition \\%otherwise\\% alternate\n')
                              ))
}


# set_option_documentation("documentation::Rd::collapse.lines"
#    , description = "should documentation functions return a single " %<<%
#                    "string (TRUE) or a array of strings (FALSE) " %<<%
#                    "representing the lines of documentation."
#    , default = FALSE
#    , constraints = list(is.logical)
#    )
# set_option_documentation("documentation::Rd::collapse.with"
#    , description = "when \\code{getOption(documentation::toRd::collapse.lines)}" %<<%
#                    "is \\code{TRUE} what the lines should be separated with."
#    , default = "\n"
#    , constraints = list(~is.string(.))
#    )

.Rd_strwrap <-
function( x
        , wrap.lines
        , wrap.at
        ){
    assert_that(is.flag(wrap.lines))
    if (identical(class(x), 'character')) x <- Rd_text(x)
    else assert_that(is(x, 'Rd'))
    if (!wrap.lines) {
        return(x)
    } else
    if (is.character(x) && attr(x, 'Rd_tag') =="TEXT") {
        assert_that( is.count(wrap.at)
                   , is.character(x)
                   , is.null(attr(x, 'Rd_option'))
                   )

        lines <- base::strwrap(x, width=wrap.at, simplify = TRUE)
        if (length(lines) <=1) return(x)
        lines <- lapply(lines, Rd_text, type='TEXT')
        y <-

        tag <- attr(x, 'Rd_tag') %unless% (attr(x, 'Rd_tag')=='TEXT') %then% 'Rd'
        class <- attr(x, 'class') %unless% (attr(x, 'Rd_tag')=='TEXT') %then% 'Rd'
        return(s( unname(rbind(lines, .Rd.newline))
                , class = 'Rd'
                ))
    } else
    if (is.list(x) && are_Rd_strings(x, 'TEXT')) {
        lines <- base::strwrap( collapse0(unlist(x))
                              , width=wrap.at
                              , simplify = TRUE)
        if (length(lines) <=1) return(x)
        lines <- lapply(lines, Rd_text, type='TEXT')
        if (length(lines) > 1L && !is.null(attr(x, 'Rd_tag'))) {
            lines <- unname(rbind(lines, .Rd.newline))
            lines <- c(unclass(.Rd.newline), as.vector(lines))
            return(s( lines
                    , class = attr(x, 'class')
                    , Rd_tag = attr(x, 'Rd_tag')
                    , Rd_option = attr(x, 'Rd_option')
                    ))
        } else {
            return(s( unname(rbind(lines, .Rd.newline))
                    , class = attr(x, 'class')
                    , Rd_tag = attr(x, 'Rd_tag')
                    , Rd_option = attr(x, 'Rd_option')
                    ))
        }
    } else
    if (is.list(x)) {
        val <- lapply(x, .Rd_strwrap, wrap.lines=wrap.lines, wrap.at=wrap.at)
        return(fwd(val, x))
    } else {
        return(x)
    }
}
if(FALSE){# testing INACTIVE
    x <- stringi::stri_rand_lipsum(1)

    expect_identical(.Rd_strwrap(x, wrap.lines=FALSE, wrap.at=72L)
                    , Rd_text(x))
    val <- .Rd_strwrap(x, wrap.lines=TRUE , wrap.at=72L)
    expect_is(val, 'Rd')
    expect_true(all_inherit(val, c('Rd_TEXT', 'Rd_newline')))
    expect_equal( unlist(val[1,])
                , base::strwrap(x, 72L)
                )
    expect_true( all(unlist(val[2,])=='\n'))
    val <- .Rd_strwrap(x, wrap.lines=TRUE, wrap.at=50)
    expect_identical( unlist(val[1,])
                    , base::strwrap(x, 50)
                    )
}
if(FALSE){# deprecated testing .Rd_strwrap specification by options
    withr::with_options(list( "documentation::Rd::wrap.lines" = TRUE
                            , "documentation::Rd::wrap.at"    = 50), {
        x <- Rd_text(stringi::stri_rand_lipsum(1))
        expect_identical( as.character(Rd_canonize(x))
                        , paste0(base::strwrap(x, 50), '\n')
                        )
    })
}
if(FALSE){# testing INACTIVE .Rd_strwrap preservation of line breaks.
    x <- c("Lorem ipsum", stringi::stri_rand_lipsum(3, start_lipsum = FALSE))
    x <- Rd_text(collapse(x, '\n\n'))

    expect_identical( as.character(Rd_canonize(x, control=list(wrap.lines=TRUE, wrap.at=72L)))
                    , paste0(unlist(base::strwrap(x, 72)), '\n')
                    )
    expect_equal(sum(.Rd_strwrap(x, wrap.lines=TRUE, wrap.at=72L) == ''), 3L)

    expect_identical( unlist(.Rd_strwrap("   hello\n\nworld", wrap.lines=TRUE, wrap.at=72L)[1,])
                    , c("hello", "", "world")
                    )
}



# S3 Methods ----------------------------------------------------------

if (FALSE){# testing INACTIVE toRd,character
    expect_identical( toRd(c("\\hello\n", "%world"))
                    , Rd(Rd_text("\\\\hello\n"), Rd_text("\\%world")))
}

toRd.NULL <- function(obj, ...){Rd()}
if(FALSE){# testing INACTIVE
    expect_identical(toRd(NULL), Rd())
}

#' @export
toRd.list <- function(obj, ...){
    val <- lapply(obj, toRd, ...)
    assert_that( is_valid_Rd_list(val))
    cl(val, 'Rd')
}
if(FALSE){# testing INACTIVE
    l <- list('\\hello ', '%world')
    expect_identical( toRd(l)
                    , s( list( Rd_text("\\\\hello ")
                             , Rd_text("\\%world"))
                       , class='Rd')
                    )

    l <- list( first  = Rd("first text")
             , second = Rd(c("second", "text"))
             , third = NULL
             )

    val <- toRd(l)

    expect_is(val, 'Rd')
    expect_is(val[[1]], 'Rd_TEXT')
}



#' @export
toRd.Rd <- function(obj, ...){
    assert_that( is(obj, 'Rd')
               , is.character(obj)  %if% is(obj, c('Rd_TEXT', 'Rd_RCODE'))
                 %otherwise% is.list(obj)
               )
    obj
}
if(FALSE){# testing INACTIVE
    obj <- Rd("test")
    expect_identical(toRd(obj), obj)
    expect_error(toRd(cl(TRUE, 'Rd')))

    obj <- Rd("\\rd")
    expect_identical(toRd(obj), obj)
}



#' @export
toRd.person<-
function( obj
        , ...
        , name.parts = c('given', 'family', 'email')
        ){
    comma_list(format( obj, include = name.parts
                     , braces  = list(email = c('\\email{', '}'))
                     ))
}
if (FALSE) {# testing toRd,author
    obj <- list(author = c( person('Andrew', 'Redd'
                                  , email='andrew.redd@hsc.utah.edu')
                          , person('Drew'  , 'Blue')
                          ))
    val <- toRd(obj)
    expect_is_exactly(val, 'Rd')
    expect_length(val, 1L)
    expect_is_exactly(val[[1]], 'Rd_TEXT')
    expect_length(val[[1]], 1L)
    expect_identical( as.character(toRd(obj))
                    , "Andrew Redd \\email{andrew.redd@hsc.utah.edu} and Drew Blue"
                    )
}
if (FALSE) {# testing INACTIVE
    object <-c( person('First' , 'Author', email='me1@email.com')
              , person('Second', 'Author', email='me2@email.com')
              )
    val <- toRd(object)
    expect_is_exactly(val, 'Rd')
    expect_length(val, 1L)
    expect_is_exactly(val[[1L]], 'Rd_TEXT')
    expect_length(val[[1]], 1L)

    expect_equal( as.character(val)
                , 'First Author \\email{me1@email.com}' %<<% 'and' %<<%
                  'Second Author \\email{me2@email.com}'
                  )
    expect_equal( toRd( c( person('Andrew', 'Redd', email='andrew.redd@hsc.utah.edu')
                         , person('Drew'  , 'Blue')
                         ) )
                , s( list( s('Andrew Redd \\email{andrew.redd@hsc.utah.edu}' %<<%
                             'and Drew Blue'
                            , Rd_tag = 'TEXT'
                            , class = c('Rd_TEXT', 'Rd_tag', 'Rd')))
                           , class='Rd')
                )
}

toRd.name <- function(obj, ...)Rd_text(as.character(obj), 'VERB')
if(FALSE){# testing INACTIVE
    obj <- as.name('test.name')
    val <- toRd(obj)
    expect_is(val[[1]], 'Rd_VERB')
    expect_identical(toRd(obj), Rd(Rd_text('test.name', 'VERB')))
}
