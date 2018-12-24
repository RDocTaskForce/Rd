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
