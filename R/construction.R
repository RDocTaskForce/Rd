### Construction Functions #####

#' Split a rd object into relevant lines.
#'
#' @param rd an rd list.
Rd_split <-
function(rd){
    assert_that(is_valid_Rd_list(rd))
    has.newline <- purrr::map_lgl(rd, Rd_ends_with_newline)
    group <- rev(cumsum(rev(has.newline)))
    group <- max(group)-group

    parts <-unname(split(Rd_untag(rd), group))
    s(fwd(parts, unclass(rd)), split = TRUE)
}
if(FALSE){#@testing
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))

    val <- Rd_split(txt)
    expect_is(val, 'list')
    expect_is_not(val, 'Rd')

    expect_all_inherit(val, 'Rd')
    expect_true(all_are_exactly(val, 'Rd'))

    x <- txt['\\examples'][[1]]
    y <- Rd_split(x)
    expect_is_not(y, 'Rd_tag')
    expect_is_not(y, 'Rd')
    expect_equal(get_Rd_tag(y), "\\examples")
    expect_true(attr(y, "split"))

    expect_is_exactly(y[[1]], 'Rd')
    expect_identical(y[[1]][[1]], x[[1]])
    expect_length(y, 29L)

    expect_equal( fwd(unlist(y, recursive = FALSE), x), x)
}

check_content <- function( content, .check=NA, label= NULL
                         , verbose = getOption("Rd::verbose"
                                    , getOption("verbose", FALSE))
                         ){
    label = label %||% deparse(substitute(content))
    if (isFALSE(.check)) {
        return(content)
    } else
    if (is.na(.check)) {
        untagged.text <- are(content, 'character')
        if (any(untagged.text)) {
            n <- sum(untagged.text)
            for (i in which(untagged.text)){
                if (length(content[[i]]) > 1L) {
                    pkg_warning(._("Strings are expected." %<<%
                                   "Collapsing character vector to string" %<<%
                                   "at position %d.", i)
                               , type="collapsing_character_vector" )
                    content[[i]] <- collapse0(content[[i]])
                }
                content[[i]] <- Rd_text(content[[i]])
            }
            pkg_message(if(n == 1) ._("There was 1 character string at position %d" %<<%
                                      "of %s that was converted to" %<<%
                                      "an Rd_string/TEXT.", which(untagged.text), label)
                             else  ._("There were %d character strings in %s" %<<%
                                      "that were converted to" %<<%
                                      "Rd_string/TEXT.", n, label)
                       , type="class inference") %if% verbose
        }
        untagged.lists <- are(content, 'list')
        if (any(untagged.lists)) {
            n <- sum(untagged.lists)
            for (i in which(untagged.lists)) {
                assert_that( is_valid_Rd_object(content[[i]])
                           , msg = "list given at position" %<<% i %<<%
                                   "of" %<<% label %<<%
                                   "cannot be converted to a valid Rd object."
                           )
                if (is.null(attr(content[[i]], 'Rd_tag')))
                    content[[i]] <- cl(content[[i]], 'Rd')
                else
                    content[[i]] <- cl(content[[i]], 'Rd_tag')

            }
            pkg_message(if(n == 1) ._("There was 1 list at position %d" %<<%
                                      "of %s that was converted to" %<<%
                                      "an %s.", i, label, class(content[[i]]))
                             else  ._("There were %d lists in %s" %<<%
                                      "that were converted to" %<<%
                                      "Rd objects.", n, label)
                       , type="class inference") %if% verbose
        }
    } else
    if (isTRUE(.check)) {
        assert_that( is.null(get_Rd_tag(content))
                   , is_valid_Rd_object(content, strict=NA, deep=TRUE)
                   )
    }
    return(Rd_canonize(content, .check=FALSE))
}
check_content <- skip_scope(check_content)
test_check_content <- function(..., content=list(...), .check=NA, verbose=TRUE){
    check_content(content=content, .check=.check, verbose=verbose)
}
if(FALSE){#@testing check_content(., .check=NA)
    expect_identical(test_check_content(content=list()), .Rd())
    expect_message( val <- test_check_content("string")
                  , class = "Rd::test_check_content-message-class inference")
    expect_message( val <- test_check_content("string")
                  , regexp = "There was 1 character string at position 1" %<<%
                             "of content that was converted to an Rd_string/TEXT\\.")
    expect_identical(val, .Rd(Rd_text('string')))

    expect_message( val <- test_check_content("hello", "world")
                  , regexp = "There were 2 character strings in content" %<<%
                             "that were converted to Rd_string/TEXT\\.")

    expect_warning( val <- test_check_content(c("hello", "world"), verbose=FALSE)
                  , class = "Rd::test_check_content-warning-collapsing_character_vector")

    expect_message( val <- test_check_content(Rd_text("hello"), list(Rd_text("world")))
                  , "There was 1 list at position 2 of content that was converted to an Rd\\."
                  )
    expect_message( val <- test_check_content( list(Rd_text("hello"))
                                             , s(list(Rd_rcode("world")), Rd_tag="\\code")
                                             )
                  , "There were 2 lists in content that were converted to Rd objects\\."
                  )
    expect_identical(val, .Rd( .Rd(Rd_text("hello"))
                              , Rd_tag("\\code", Rd_rcode("world"))
                              ))

}
if(FALSE){#@testing check_content(., .check=FALSE)
    expect_identical(test_check_content(content=iris, .check=FALSE), iris)
}
if(FALSE){#@testing check_content(., .check=TRUE)
    expect_identical(test_check_content(content=list(), .check=TRUE), .Rd())
    expect_error( test_check_content("string", .check=TRUE)
                , "Elements 1 of elements.are.valid are not true")
    expect_error( test_check_content("string", .check=TRUE)
                , class = "Rd::test_check_content-error-assertion failure")

    expect_error( test_check_content("hello", "world", .check=TRUE)
                , "Elements 1, 2 of elements.are.valid are not true")
    expect_error( test_check_content("hello", "world", .check=TRUE)
                , class = "Rd::test_check_content-error-assertion failure")

    content <- list( list(Rd_text("hello"))
                   , s(list(Rd_rcode("world")), Rd_tag="\\code")
                   )
    expect_error( test_check_content( content =content
                                    , .check=TRUE)
                , "Elements 1, 2 of elements.are.valid are not true")
}

add_newlines <- function(content){
    if (!is_Rd(content)) class(content) <- 'Rd'
    if (Rd_spans_multiple_lines(content)){
        if(all_are_exactly(content, 'Rd')){
            for (i in seq_along(content))
                content[[i]] <- Recall(content[[i]])
        } else {
            type <- if (get_Rd_tag(content[[1]]) == 'RCODE') 'RCODE' else 'TEXT'
            nl <- .Rd(Rd_string('\n', type))
            if (!Rd_starts_with_newline(content)) content <- Rd_canonize_text(Rd_c(nl, content))
            if (!Rd_ends_with_newline(content)) content <- Rd_canonize_text(Rd_c(content, nl))
        }
    }
    return(content)
}


# String Construction -----------------------------------------------------------------
#' @name Rd_string_creation
#' @title Rd String Construction
#'
#' @description
#' Construct a Rd string type.
#'
#' @param content a string to type as an Rd string type
#' @param type the type of text is is.
#'
#' @family construction
NULL

#' @rdname Rd_string_creation
#' @export
Rd_string <-
function( content
        , type = c("TEXT", "RCODE", "VERB", "COMMENT", "UNKNOWN", "LIST")
        ){
    type <- match.arg(type)
    assert_that( is.string(content))
    s( content
     , Rd_tag = type
     , class  = 'Rd_string'
     )
}

#' @describeIn Rd_string_creation Type as plain text.
#' @export
#' @examples
#' ## Plain text
#' Rd_text("Plain text")
Rd_text <-  function(content){
    Rd_string(content, type='TEXT')
}
if(FALSE){#@testing
    val <- Rd_text('testing')
    expect_is_exactly(val, 'Rd_string')
    expect_is_not(val, 'Rd')
    expect_is_not(val, 'Rd_tag')
    expect_is_not(val, 'Rd_TEXT')

    expect_true(is.character(val))
    expect_false(is.list(val))

    val <- Rd_string('some(code)', 'RCODE')
    expect_equal(attr(val, 'Rd_tag'), 'RCODE')
    val <- Rd_string('some(code)', 'R')
    expect_is(val, 'Rd_string')
    expect_equal(attr(val, 'Rd_tag'), 'RCODE')

    x <- Rd_text(collapse(stringi::stri_rand_lipsum(3), '\n\n'))
    expect_is(x, 'Rd_string')
    expect_true(is_Rd_string(x, 'TEXT'))
    expect_is_not(x, 'Rd')
    expect_is_not(x, 'Rd_tag')
    expect_is_not(x, 'Rd_TEXT')

    expect_error(x <- Rd_text(c( 'hello', '\n', ' big', '\n', '  wide', '\n', '   world'))
                , class="Rd-error-assertion failure")

    x <- Rd_text("     hello world")
    expect_is(x, 'Rd_string')
    expect_length(x, 1L)
}

#' @describeIn Rd_string_creation Type as R Code.
#' @export
#' @examples
#' ## R Code
#' Rd_rcode("code()")
Rd_rcode <- function(content){
    Rd_string(content, type='RCODE')
}
if(FALSE){ #@testing
    val <- Rd_rcode('1+1==2')
    expect_identical(val, s('1+1==2', Rd_tag='RCODE', class='Rd_string'))
}

#' @describeIn Rd_string_creation Type as R symbol or 'verb'.
#' @export
#' @examples
#' ## Symbols, i.e. verbs
#' Rd_verb("verb")
#' Rd_symb("symbol")
Rd_verb <- function(content){
    Rd_string(content, type='VERB')
}
#' @describeIn Rd_string_creation Type as R symbol or 'verb'.
#' @export
Rd_symb <- Rd_verb
if(FALSE){ #@testing
    val <- Rd_symb('name')
    expect_identical(val, s('name', Rd_tag='VERB', class='Rd_string'))
}


#' @describeIn Rd_string_creation type as Rd comment. Comments must start with the comment character '%'.
#' @export
#' @examples
#' ## Rd Comments
#' Rd_comment("% Rd/LaTeX comment")
Rd_comment <- function(content){
    assert_that( grepl(pattern='^%', content)
               , msg = "Ill-formed comment")
    assert_that(!grepl(pattern='\n', content, fixed = TRUE)
               , msg = "comments cannot contain newlines.")
    Rd_string(content, type='COMMENT')
}
if(FALSE){#@testing Rd_rcode, Rd_symb, and Rd_comment
    expect_error(Rd_comment("testing"), "Ill-formed comment", class = "Rd-error-assertion failure")
    expect_true(is_Rd_string(Rd_comment("% comment"), "COMMENT", strict = TRUE))
}

# List Constructions ---------------------------------------------------------------

# Rd Container =====================================================================

.Rd <- function(...)s(list(...), class='Rd')


#' Construct an Rd container
#'
#' An Rd container is a list which contains other Rd objects.
#'
#' @param ...,content Content specified in individual or list form.
#'                    Either may be used but only one.
#'                    All elements should be unnamed.
#' @param .check Should the content be checked for valid Rd and if options are valid?
#'               A value of FALSE indicates no checking,
#'               TRUE strict checking and
#'               NA convert where possible, with messages and warnings.
#' @param verbose Print informational messages.
#' @export
#' @family construction
#' @examples
#' ## Recreating the first few lines of the `example` help file.
#' R <- Rd_tag("\\R")
#' Rd( Rd_name('example'), '\n'
#'   , Rd_alias('example'), '\n'
#'   , Rd_title("Run an Examples Section from the Online Help"), '\n'
#'   , Rd_description( "Run all ", R, " code from the "
#'                   , Rd_tag("\\bold", "Examples"), " part of \n"
#'                   , R, "'s online help topic ", Rd_code("topic")
#'                   , " with possible exceptions \n"
#'                   , Rd_code("dontrun"), ", "
#'                   , Rd_code("dontshow"), ", and "
#'                   , Rd_code("donttest"), ",\n see "
#'                   , Rd_tag("\\sQuote", "Details"), " below."
#'                   )
#'   )
Rd <-
function(..., content = list(...), .check=NA
        , verbose = getOption("Rd::verbose", getOption("verbose", FALSE))
        ){
    assert_that(missing(content) || ...length() == 0L)
    #' @details
    #' An empty Rd vector can be created with a `Rd()` call.
    if (length(content) == 0L) return(invisible(cl(list(), 'Rd'))) else
    #' A call to Rd with only one argument will not create a double nested list,
    #' but will encapsulate tags and strings.  For example,
    #' calling `Rd(Rd('test'))` has the same effect as `Rd('test')`
    #'
    if (...length() == 1L && is_exactly(..1, 'Rd'))
        return(Rd_canonize(..1))
    #' A character vector may be passed to Rd where it is
    #' collapsed  and normalized to a `Rd` container of Rd string.
    #'
    #' @return Will always return valid Rd in canonical form.
    content <- check_content(content, .check=.check, verbose=verbose)
    cl(content, 'Rd')
}
if(FALSE){#@testing
    a <- "test"
    expect_message( b <- Rd(a, verbose=TRUE)
                  , "There was 1 character string at position 1" %<<%
                    "of content that was converted to an Rd_string/TEXT." )
    expect_is_exactly(b, 'Rd')
    expect_true(is_Rd_string(b[[1]], 'TEXT'))

    # a <- stringi::stri_rand_lipsum(3)
    # b <- Rd(collapse(a, '\n\n'), wrap.lines=TRUE)
    # expect_is_exactly(b, 'Rd')
    # expect_identical(mode(b), 'list')
    # expect_true(length(b) > 5)

    c <- Rd(Rd_text(a))
    expect_is_exactly(c, 'Rd')
    d <- Rd(c)
    expect_is_exactly(d, 'Rd')
    expect_identical(c, d)

    expect_error(Rd(NULL))

    expect_is(Rd(), 'Rd')
    expect_length(Rd(), 0L)

    expect_message(x <- Rd(collapse(stringi::stri_rand_lipsum(3), '\n\n'), verbose=TRUE))
    expect_Rd_bare(x)
    expect_Rd_string(x[[1L]], 'TEXT')
    expect_true(all(are_Rd_strings(x, 'TEXT')))

    x <- Rd(Rd_text('text'))
    expect_Rd_bare(x)
    expect_Rd_string(x[[1]], 'TEXT')

    x <- Rd("Multiple ", "Character strings", " to convert")
    expect_Rd_bare(x)
    expect_Rd_string(x[[1]], "TEXT")
    expect_length(x, 1)
    expect_true(all(are_Rd_strings(x, 'TEXT')))
}
setValidity('Rd', function(object){
    validate_that( is.list(object)
                 , is.null(attr(object, 'Rd_tag'))
                 , is_valid_Rd_list(object, strict = TRUE, deep=TRUE)
                 )
})
if(FALSE){#@testing Class-Rd
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    expect_is(txt, 'Rd')
    expect_true(is_valid_Rd_object(txt))
    expect_true(validObject(txt, TRUE, complete = FALSE))
}



# Tag Construction ----------------------------------------------------------------------------------

#' Create an Rd tag container
#'
#' Rd_tag containers can contain, [strings][Rd_string()],
#' [Rd containers][Rd()] and other tags.
#'
#' @inheritParams Rd
#' @param tag     The Rd/LaTeX tag to use.  Must start with the backslash.
#' @param ...,content The content specified in individual form or list form.
#'                    Either may be used but not both.
#' @param opt Options for the tag which as placed in square brackets.
#' @param indent Should content inside the tag be indented?
#' @param indent.with  Amount to indent with, defaults to four spaces.
# @param wrap.lines Should lines of content inside the tag be wrapped for nicer formatting?
# @param wrap.at The number of characters to wrap at, defaults to 72.
#' @param .check Should the content be checked for valid Rd and if options are valid?
#'               A value of FALSE indicates no checking,
#'               TRUE strict checking and
#'               NA convert where possible, with messages and warnings.
#' @export
#' @family construction
#' @examples
#' Rd_tag("\\bold", Rd_text("Bolded text."))
Rd_tag  <-
function( tag
        , ...
        , content = Rd(...)
        , opt = NULL
        , indent         = getOption("Rd::indent", FALSE)
        , indent.with    = getOption("Rd::indent.with", '  ')
        , .check = NA
        , verbose = getOption("Rd::verbose", getOption("verbose", FALSE))
        ){
    assert_that( is.flag(.check)
               , is.string(tag)
               , is.flag(indent)
               , missing(content) || ...length() == 0L
               )
    if (!grepl("^\\\\", tag)) {
        if(isTRUE(.check)) pkg_error("tag is expected to start with a backslash"
                                    , type = "invalid Rd tag")
        assert_that( grepl("^[a-zA-Z]+$", tag)
                   , type = "invalid Rd tag")
        tag <- "\\" %<<<% tag
        if(is.na(.check)) pkg_warning( ._( "`tag` is expected to start with a backslash." %<<%
                                           "Converted to %s.", sQuote(tag))
                                     , type = "invalid Rd tag"
                                     )
    }
    content <- check_content(content, .check=.check, verbose=verbose)
    if (length(opt))
        assert_that(is_valid_Rd_object(opt))
    content <- cl(content, 'Rd')
    content <- add_newlines(content)
    if (indent)
        content <- Rd_indent(content, indent.with = indent.with)
    val <- s( content
            , Rd_tag = tag
            , class  = 'Rd_tag'
            , Rd_option = opt %if% length(opt)
            )
    if (!isFALSE(.check)) val <- Rd_canonize(val)
    return(val)
}
if(FALSE){#! @testing
    expect_error(Rd_tag(NULL, 'test'), "tag is not a string")
    expect_error(Rd_tag(c('a', 'b'), 'test'), "tag is not a string")
    expect_error(Rd_tag(1, 'test'), "tag is not a string")
    expect_warning( val <- Rd_tag('name', Rd_text('my name'))
                  , class = "Rd::Rd_tag-warning-invalid Rd tag"
                  )
    expect_warning( val <- Rd_tag('name', Rd_text('my name'))
                  , regexp = "`tag` is expected to start with a backslash\\." %<<%
                             "Converted to .{1,4}name.{1,4}\\."
                  )
    expect_is(val, "Rd_tag")
    expect_identical( Rd_tag('name', Rd_text('my name'), .check = FALSE)
                    , s( list(Rd_text("my name"))
                       , Rd_tag = "\\name"
                       , class  = 'Rd_tag'
                       ))

    expect_error(Rd_tag('name', Rd_text('my name'), .check = TRUE)
                , class="Rd-error-invalid Rd tag" )
}
if(FALSE){#@testing
    x <- Rd_tag('\\item', Rd(Rd_text('arg')), Rd(Rd_text("an agrument")))
    expect_length(x, 2L)

    val <- Rd_tag('\\link', Rd_text('dest'), opt=Rd_text('pkg'))
    expect_Rd_string(attr(val, 'Rd_option'), 'TEXT')

    expect_is(val, 'Rd_tag')
    expect_identical(format(val), "\\link[pkg]{dest}")
}
if(FALSE){#@testing Rd_tag(indent=TRUE)
    val <- Rd_tag('\\description'
                 , Rd_text('line 1\n')
                 , Rd_text('line 2\n')
                 , indent=TRUE
                 , indent.with='    '
                 )
    exp <- Rd_tag('\\description'
                 , Rd_text('\n')
                 , Rd_text('    line 1\n')
                 , Rd_text('    line 2\n')
                 , .check=FALSE
                 )
    expect_Rd_tag(val, '\\description')
    expect_length(val, 3L)
    expect_identical(val, exp)
    expect_identical( format(val)
                    , "\\description{" %\%
                      "    line 1" %\%
                      "    line 2" %\%
                      "}"
                    )
}
setValidity('Rd_tag', function(object){
    validate_that( is.list(object) || is.character(object)
                 , !is.null(tag <- attr(object, 'Rd_tag'))
                 , grepl('^\\\\', tag)
                 , is_valid_Rd_list(object, strict = TRUE, deep=TRUE)
                 )
})
if(FALSE){#@testing validObject(Rd_tag)
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    expect_is(txt, 'Rd')

    desc <- Rd_get_element(txt, '\\description')
    expect_Rd_tag(desc, '\\description')
    expect_true(is_valid_Rd_object(desc))
    expect_true(validObject(desc, TRUE))
}
if(FALSE){#@testing Rd_tag edge case all elements are Rd.
    content <- collapse(strwrap(stringi::stri_rand_lipsum(1), 72), '\n')
    rd <- Rd_tag( "\\section"
                , content = list( Rd("Title")
                                , Rd(content)
                                )
                )
    expect_Rd_tag(rd, '\\section')
    expect_Rd_bare(rd[[1]])
    expect_Rd_bare(rd[[2]])
    expect_true(head(rd[[2]], 1) == '\n')
    expect_true(tail(rd[[2]], 1) != '\n')
}
