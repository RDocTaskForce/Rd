### Tag Convenience Functions #####

#' @name shortcuts
#' @title Convenience Construction Shortcuts.
#' @description
#' These functions are provided to construct Rd structure.
#' In some cases additional compliance checks are included.
#' @param ...,content [Rd] elements to be contained in the tag.
NULL

#' @describeIn shortcuts Create an alias tag.
#' @param alias an alias as a plain string.
#' @export
Rd_alias <- function(alias){Rd_tag('\\alias', Rd_symb(alias))}
if(FALSE){#@testing
    expect_Rd_tag(Rd_alias("x"), '\\alias')
    expect_identical(format(Rd_alias("x")), "\\alias{x}")
}

#' @describeIn shortcuts Create multiple aliases
#' @param aliases a character vector, each element a separate alias.
#' @export
Rd_aliases <- function(aliases){Rd_lines( lapply(aliases, Rd_alias)
                                        , tags = c("\\alias", "VERB", "TEXT"))}
if(FALSE){#@testing
    expect_Rd_bare(Rd_aliases(head(letters)))
    expect_identical(format(Rd_aliases(head(letters)))
                    , collapse(paste0("\\alias{", head(letters), "}"), '\n'))
}

#' @describeIn shortcuts Create author tags
#' @param author a string, a person object, or another Rd object.
#' @export
Rd_author <- function(author){
    if (is(author, 'person'))
        author <- toRd(author)
    else if (is(author, 'character'))
        author <- Rd_text(author)
    else {
        assert_that( is_valid_Rd_object(author)
                   , msg="author must be a string or person object," %<<%
                         "if not already valid Rd.")
        if (is(author, 'Rd'))
            return(Rd_tag('\\author', content=author))
    }
    Rd_tag('\\author', author)
}
if(FALSE){#@testing
    expect_Rd_tag(Rd_author('joe blow'), "\\author")
    expect_identical(format(Rd_author('joe blow')), "\\author{joe blow}")

    suppress_messages(author <- Rd("My Name ", Rd_tag("\\email", "my.name@email")))
    expect_Rd_tag(. <- Rd_author(author), "\\author")
    expect_identical(format(.), "\\author{My Name \\email{my.name@email}}")

    expect_Rd_tag(. <- Rd_author(person("Given", "Family")), '\\author')
    expect_identical(format(.), "\\author{Given Family}")
}

#' @describeIn shortcuts create an arguments tag
#' @param items arguments each should be an 'item' tag,
#'                  newlines to separate items are added automatically.
#' @param indent indent content
#' @param indent.with string to use for indent.
#' @export
Rd_arguments <-
function( ...
        , items = list(...)
        , indent = getOption("Rd::indent", TRUE)
        , indent.with = getOption("Rd::indent.with", '  ')
        ){
    indent.with <- Rd_clean_indent(indent.with)
    assert_that( all(are_Rd_tags(items, '\\item') ))
    content <- undim(rbind( indent.with %if% indent
                          , items
                          , Rd.newline
                          ))
    Rd_tag(tag='\\arguments', content=content)
}
if(FALSE){#@testing
    items <- .Rd( Rd_item('a', 'first')
                , Rd_item('b', 'second'))
    expect_Rd_tag(Rd_arguments(items=items, indent=FALSE), '\\arguments')
    expect_length(Rd_arguments(items=items, indent=FALSE), 5L)
    expect_identical(format(Rd_arguments(items=items, indent=TRUE, indent.with='  '))
                    , "\\arguments{" %\%
                      "  \\item{a}{first}" %\%
                      "  \\item{b}{second}" %\%
                      "}")

}


#' @describeIn shortcuts Create a code tag.
#' @param code a string to be converted to `RCODE` then wrapped in the 'code' tag.
#' @export
Rd_code <- function(code){Rd_tag('\\code', Rd_rcode(code))}
if(FALSE){#@testing
    expect_Rd_tag(Rd_code('code'), '\\code')
    expect_Rd_string(Rd_code('code')[[1]], 'RCODE')
    expect_identical( format(Rd_code('code'))
                    , "\\code{code}")

    expect_identical( Rd_code(Rd_text("hello_world"))
                    , Rd_tag("\\code", Rd_rcode("hello_world")))
}

#' @describeIn shortcuts Create a 'concept' tag.
#' @param concept the concept name, as a plain string.
#' @export
Rd_concept <- function(concept){Rd_tag('\\concept', Rd_text(concept))}
if(FALSE){#@testing
    expect_Rd_tag(Rd_concept('testing'), '\\concept')
    expect_identical( format(Rd_concept('testing'))
                    , "\\concept{testing}")
    expect_Rd_string(Rd_concept('testing')[[1]], 'TEXT')
}


#' @describeIn shortcuts Create multiple concepts.
#' @param concepts a vector of concepts each to be put in a concept tag.
#' @export
Rd_concepts <- function(concepts){
    assert_that(is.character(concepts))
    Rd_lines(lapply(concepts, Rd_concept))
}
if(FALSE){#@testing
    val <- Rd_concepts(c('test1', 'test2'))
    expect_Rd_bare(val)
    expect_length(val, 3L)
    expect_Rd_tag(val[[3]], '\\concept')
    expect_Rd_string(val[[2]], 'TEXT')
    expect_identical( format(val)
                    , "\\concept{test1}\n\\concept{test2}")
    expect_Rd_string(val[[1]][[1]], 'TEXT')

    expect_error(Rd_concepts(TRUE))
}

#' @describeIn shortcuts Create a description tag.
#' @export
Rd_description <- function(..., content=Rd(...)) {
    Rd_tag("\\description", content=content)
}
if(FALSE){#@testing
    x <- strwrap(collapse(stringi::stri_rand_lipsum(3), '\n\n'), 72)
    val <- Rd_description(Rd_text(collapse(x, '\n')))
    expect_Rd_tag(val, '\\description')
    expect_true(length(val) > 5L)
    expect_true(is_Rd_newline(val[[1]]))
    expect_Rd_string(val[[2]], 'TEXT')
}

#' @describeIn shortcuts Create an examples tag.
#' @export
Rd_examples <- function(..., content=list(...)) {
    . <- are(content, 'character')
    content[.] <- lapply(content[.], Rd_rcode)
    assert_that(all(are_Rd_strings(content, 'RCODE')))
    Rd_tag('\\examples', content=content)
}
if(FALSE){#@testing
    expect_error(Rd_examples(Rd_text('example'))
                , class = "Rd-error-assertion failure")
    val <- Rd_examples( "Rd_alias('alias')\n"
                      , "Rd_concept('testing')"
                      )
    expect_Rd_tag(val, '\\examples')
    expect_length(val, 3L)
    expect_true(is_Rd_newline(val[[1]]))
    expect_Rd_string(val[[2]], 'RCODE')
}

#' @describeIn shortcuts Create an item tag.
#' @param item the item text
#' @param description an optional description that if provided
#'            changes the 'item' tag into a two parameter, rather
#'            than a single item tag followed by the item text.
#' @export
#' @examples
#' Rd_item("an item")
#' Rd_item('a', 'the first letter of the alphabet.')
Rd_item <- function(item, description=NULL) {
    if (is(item, 'character')) item <- Rd_text(item)
    else assert_that(is_valid_Rd_object(item))

    if (is.null(description)){
        if (!is(item, 'Rd')) item <- .Rd(item)
        if (grepl('^(?!= )', format(item), perl=TRUE))
            item <- Rd_canonize_text(Rd_c(Rd_text(" "), item))
        Rd_c(.Rd(Rd_tag("\\item")), item)
    } else {
        if (is(description, 'character')) description <- Rd_text(description)
        else assert_that(is_valid_Rd_object(description))
        if (!is(item, 'Rd')) item <- .Rd(item)
        if (!is(description, 'Rd')) description <- .Rd(description)
        Rd_tag("\\item", content=.Rd( item, description))
    }
}
if(FALSE){#@testing
    expect_Rd_bare(Rd_item('an item'))
    expect_length(Rd_item('an item'), 2)
    expect_identical(format(Rd_item('an item')), "\\item an item")

    expect_Rd_tag(val <- Rd_item('a', 'the first letter of the alphabet'), '\\item')
    expect_identical(format(val), "\\item{a}{the first letter of the alphabet}")

    suppress_messages(item <- Rd(Rd_code('a'), ': the first letter of the alphabet'))
    expect_Rd_bare(val <- Rd_item(item))
    expect_Rd_tag(val[[1]], '\\item')
    expect_length(val[[1]], 0)
    expect_identical(format(val), "\\item \\code{a}: the first letter of the alphabet")

    val <- Rd_item( .Rd(Rd_code('a'), Rd_text(':'))
                  , Rd_text('the first letter of the alphabet')
                  )
    expect_Rd_tag(val, '\\item')
    expect_length(val, 2L)
    expect_Rd_bare(val[[1L]])
    expect_Rd_bare(val[[2L]])
    expect_identical(format(val), "\\item{\\code{a}:}{the first letter of the alphabet}")
}

#' @describeIn shortcuts Create a keyword tag.
#' @param key A string denoting a valid Rd keyword.
#' @param .check perform validity checks?
#' @export
#' @examples
#' Rd_keyword('documentation')
Rd_keyword <- function(key, .check = TRUE){
    if (.check) assert_that( is.string(key)
                           , key %in% keyword.db$KEYWORD
                           )
    Rd_tag('\\keyword', Rd_text(key))
}
if(FALSE){#@testing
    expect_error(Rd_keyword(TRUE), class="Rd-error-assertion failure")
    expect_error(Rd_keyword("hibbidty"), class="Rd-error-assertion failure")

    expect_Rd_tag(Rd_keyword('documentation'), '\\keyword')
    expect_identical(format(Rd_keyword('documentation')), "\\keyword{documentation}")
}

#' @describeIn shortcuts Create multiple keyword tags.
#' @param keys A character vector denoting valid Rd keywords.
#' @export
Rd_keywords <- function(keys, .check=TRUE){
    if (.check) assert_that( is.character(keys)
                           , all(keys %in% keyword.db$KEYWORD)
                           )
    Rd_lines(lapply(keys, Rd_keyword, .check=FALSE))
}
if(FALSE){#@testing
    expect_error(Rd_keywords(TRUE), class="Rd-error-assertion failure")
    expect_error(Rd_keywords("hibbidty"), class="Rd-error-assertion failure")

    expect_Rd_bare(. <-Rd_keywords(c('documentation', 'utilities')))
    expect_length(., 3)
    expect_Rd_tag(.[[1]], '\\keyword')
    expect_identical( format(.)
                    , "\\keyword{documentation}\n\\keyword{utilities}")
}

#' @describeIn shortcuts Create a name tag.
#' @param name A string for a name of the Rd document.
#' @export
Rd_name <- function(name){
    assert_that(is.string(name))
    Rd_tag('\\name', Rd_symb(name))
}
if(FALSE){#@testing
    expect_Rd_tag(Rd_name('bob'), '\\name')
    expect_identical(format(Rd_name('bob')), '\\name{bob}')
}


#' @describeIn shortcuts Create a title tag.
#' @param title A string giving the title.
#' @export
Rd_title <- function(title){Rd_tag('\\title', Rd_text(title))}
if(FALSE){#@testing
    expect_Rd_tag(Rd_title("A Title String"), '\\title')
    expect_identical( format(Rd_title("A Title String"))
                    , '\\title{A Title String}')
}

#' @describeIn shortcuts Create a usage tag.
#' @param usages lines of usage, all should be bare strings
#'                   or [`RCODE`][Rd_string()] strings.
#' @export
Rd_usage <- function(..., usages=list(...)){
    . <- are(usages, 'character')
    usages[.] <- lapply(usages[.], Rd_rcode)
    if(length(usages) > 1) usages <- Rd_lines(usages)
    val <- Rd_tag('\\usage', content=usages)
    assert_that(is_valid_Rd_object(val, tags=c('\\usage', 'RCODE', '\\S3method', '\\S4method')))
    return(val)
}
if(FALSE){#@testing
    . <- Rd_usage(Rd_rcode("Rd_usage(..., content=Rd(...))"))
    expect_Rd_tag(., '\\usage')
    expect_identical( format(.)
                    , '\\usage{Rd_usage(..., content=Rd(...))}')

    . <- Rd_usage( Rd_rcode("Rd_usage(..., content=Rd(...))")
                 , "Rd_alias(alias)"
                 )
    expect_Rd_tag(., '\\usage')
    expect_identical( format(.)
                    , '\\usage{' %\%
                      'Rd_usage(..., content=Rd(...))' %\%
                      'Rd_alias(alias)' %\%
                      '}')
}

#' @describeIn shortcuts Create a value tag section.
#' @param value The return value, must be a correctly formatted [Rd] object.
#' @export
Rd_value <- function(value){Rd_tag('\\value', content=value)}
if(FALSE){#@testing
    expect_Rd_tag(. <- Rd_value(Rd(Rd_text("A strings describing the return value.")))
                 , '\\value')
    expect_identical( format(.)
                    , '\\value{A strings describing the return value.}')

    value <- Rd( Rd_text("A value ")
               , Rd_tag('\\link', Rd_text("tag"), opt=Rd_text('=Rd_tag'))
               , Rd_text("."))
    val <- Rd_value(value)
    expect_Rd_tag(val, '\\value')
    expect_identical(format(val), '\\value{A value \\link[=Rd_tag]{tag}.}')
}


if(FALSE){# INACTIVE testing Rd_* tags
    rd <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    txt <- Rd_rm_srcref(rd)

    expect_identical(Rd_alias('Normal'), txt[[12L]])

    expect_identical(Rd_author(Rd('My Name')), Rd_author(Rd_text('My Name')))

    expect_equal( val <- Rd_arguments( Rd_item("x, q", "vector of quantiles.")
                              , Rd_item('p', "vector of probabilities.")
                              , indent = TRUE
                              , indent.with = "  "
                              ), x <- txt[['\\arguments']][1:7])

    desc <- Rd_description( Rd.newline
                          , Rd_text("  Density, distribution function, quantile function and random\n")
                          , Rd_text("  generation for the normal distribution with mean equal to ")
                            ,  Rd_tag('code', Rd_rcode('mean')), Rd.newline
                          , Rd_text("  and standard deviation equal to ")
                            , Rd_tag('code', Rd_rcode('sd'))
                          , Rd_text(".\n")
                          )
    expect_identical( collapse0(as.character(desc))
                    , collapse0(as.character(txt[['\\description']])))

    expect_identical( Rd_examples( Rd.code.newline
                                 , Rd_rcode("require(graphics)\n")
                                 , Rd.code.newline
                                 , Rd_rcode("dnorm(0) == 1/sqrt(2*pi)\n")
                                 , Rd_rcode("dnorm(1) == exp(-1/2)/sqrt(2*pi)\n")
                                 , Rd_rcode("dnorm(1) == 1/sqrt(2*pi*exp(1))\n")
                                 )
                      , txt[[52]][1:6] )
    expect_identical( Rd_examples( content=Rd( Rd.code.newline
                                             , Rd_rcode("require(graphics)\n")
                                             , Rd.code.newline
                                             , Rd_rcode("dnorm(0) == 1/sqrt(2*pi)\n")
                                             , Rd_rcode("dnorm(1) == exp(-1/2)/sqrt(2*pi)\n")
                                             , Rd_rcode("dnorm(1) == 1/sqrt(2*pi*exp(1))\n")
                                             ))
                      , txt[[52]][1:6] )

    expect_identical( Rd_item("x, q", "vector of quantiles.")
                    , txt[['\\arguments']][[3L]]
                    )

    expect_identical( Rd_keyword('distribution'), txt[['\\keyword']])
    expect_identical( Rd_name('Normal'), txt[['\\name']])
    expect_identical(Rd_title('The Normal Distribution'), txt[['\\title']])

    expect_identical(Rd_usage( Rd.code.newline
                             , Rd_rcode("dnorm(x, mean = 0, sd = 1, log = FALSE)\n")
                             , Rd_rcode("pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)\n")
                             , Rd_rcode("qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)\n")
                             , Rd_rcode("rnorm(n, mean = 0, sd = 1)\n")
                             ), txt[['\\usage']])
}

