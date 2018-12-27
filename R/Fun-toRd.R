#' @include Class-Rd.R
#' @importFrom tools toRd



### toRd #######################################################################
#' @export
setGeneric('toRd',
function(obj, ...){
    ans <- standardGeneric("toRd")
    if (is(ans, 'Rd_object')) return(ans) else
    if (is(ans, 'character')){
        ans <- Rd_canonize_text(.Rd(Rd_text(collapse0(ans))))
        if (length(ans)==1) return(ans[[1]])
        return(ans)
    } else
        pkg_error(._( "Method of generic function %1$s for class %2$s" %<<%
                      "returned a %4$s." %<<%
                      "Methods of %1$s are expected to retun a %3$s vector."
                    , sQuote("toRd"), dQuote(class(obj))
                    , 'character', dQuote(class(ans))
                    ))
})
toRd@valueClass <- 'Rd_object'
if(FALSE){#@testing toRd with character return vectors
    expect_Rd_string(val <- toRd('character'), NULL)
    expect_identical(val, Rd_text("character"))

    val <- toRd(c( "use the \\backslash to escape.\n"
                 , "and '{}' to group."
                 ))
    expect_is(val, 'Rd')
    expect_identical( val
                    , Rd(Rd_text("use the \\\\backslash to escape." %\%
                                 "and '\\{\\}' to group."
                                 )))
}
if(FALSE){#@testing INACTIVE
    toRd.test_class <- function(obj, ...)obj
    expect_error( toRd(cl(1L, 'test_class'))
                , class = "Rd-error")
}

# S3 Methods ----------------------------------------------------------

if (FALSE){# testing INACTIVE toRd,character
    expect_identical( toRd(c("\\hello\n", "%world"))
                    , Rd(Rd_text("\\\\hello\n"), Rd_text("\\%world")))
}

toRd.NULL <- function(obj, ...){Rd()}
if(FALSE){#@testing
    expect_identical(toRd(NULL), Rd())
}

Rd_unnest <- function(x){
    needs.wrap <- sapply(x, Negate(is), 'Rd')
    x[needs.wrap] <- lapply(x[needs.wrap], .Rd)
    return(unlist(x, recursive = FALSE))
}

#' @describeIn toRd
#' @param unnest Should the results be [unlist][base::unlist()]ed to remove nesting?
#'        A value of FALSE indicates that unnesting should never happen,
#'        TRUE implies allways unnest elements with class [Rd], and when
#'        NA, the default, unnesting will occure only if all elements
#'        are Rd.
#' @export
toRd.list <- function(obj, ..., unnest=NA){
    val <- lapply(obj, toRd, ...)
    if (isTRUE(unnest) || (is.na(unnest) && all(are(val, 'Rd'))))
        val <- Rd_unnest(val)
    assert_that( is_valid_Rd_list(val))
    cl(val, 'Rd')
}
if(FALSE){#@testing
    l <- list('\\hello ', '%world')
    expect_identical( toRd(l)
                    , .Rd( Rd_text("\\\\hello ")
                         , Rd_text("\\%world")
                         )
                    )

    l <- list( first  = Rd("first text")
             , second = Rd("second text")
             , third = NULL
             )
    val <- toRd(l)
    expect_is(val, 'Rd')
    expect_is(val[[1]], 'Rd_string')
    expect_length(val, 2)

    m <- list( first  = Rd("first text")
             , second = Rd_text("second text")
             , third = NULL
             )
    val2 <- toRd(m, unnest=TRUE)
    expect_identical(val, val2)

    val3 <- toRd(m)
    expect_is(val3, 'Rd')
    expect_is(val3[[1]], 'Rd')
    expect_is(val3[[1]][[1]], 'Rd_string')
    expect_length(val3, 3)
}

#' @export
toRd.Rd_string <- function(obj, ...)obj
#' @export
toRd.Rd_tag <- function(obj, ...)obj
#' @export
toRd.Rd <- function(obj, ...)obj
if(FALSE){#@testing toRd.(Rd|Rd_tag|Rd_string)
    obj <- Rd("test")
    expect_identical(toRd(obj), obj)

    obj <- obj[[1]]
    expect_Rd_string(obj, 'TEXT')
    expect_identical(toRd(obj), obj)

    obj <- Rd_tag("\\rd")
    expect_Rd_tag(obj, '\\rd')
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
    obj <- c( person('Andrew', 'Redd'
                    , email='andrew.redd@hsc.utah.edu')
            , person('Drew'  , 'Blue')
            )
    val <- toRd(obj)
    expect_Rd_string(val, 'TEXT')
    expect_length(val, 1L)
    expect_identical( format(val)
                    , "Andrew Redd \\email{andrew.redd@hsc.utah.edu} and Drew Blue"
                    )
}
if (FALSE) {#@testing
    object <-c( person('First' , 'Author', email='me1@email.com')
              , person('Second', 'Author', email='me2@email.com')
              )
    val <- toRd(object)
    expect_Rd_string(val, 'TEXT')
    expect_length(val, 1L)

    expect_equal( as.character(val)
                , 'First Author \\email{me1@email.com}' %<<% 'and' %<<%
                  'Second Author \\email{me2@email.com}'
                  )
}

toRd.name <- function(obj, ...)Rd_symb(as.character(obj))
if(FALSE){#@testing
    obj <- as.name('test.name')
    val <- toRd(obj)
    expect_Rd_string(val, 'VERB')
    expect_identical(toRd(obj), Rd_symb('test.name'))
}
