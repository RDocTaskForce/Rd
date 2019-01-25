.Rd.default.indent <- .Rd(Rd_text("  "))
setHook("onLoad", function(...){
        options( 'Rd::indent' = FALSE
               , 'Rd::indent.with' = .Rd.default.indent)
})

#' Check and clean an indent string
#'
#' @param indent.with The string to use for indentation.
#' @param type The type of string the indent is to be.
#'
#' @details
#' Indents must have the following characteristics:
#' * They must be all whitespace, i.e. spaces.
#' * Newlines are not allowed.
#' * Tabs, are allowed with a warning as
#'   they violate the guidelines.
#' * May only be Rd_strings of tag type `TEXT` or `RCODE`.
#'   Code type should only be used to indent other text.
#'
#'
#' @note When put in [canonical][Rd_canonize()] form the indents element
#' will often be merged with other elements.
#' @seealso <https://developer.r-project.org/Rds.html>
#' @return An [Rd_string], of type 'TEXT' or 'CODE', see details,
#'   wrapped in an [Rd] container.
Rd_clean_indent <-
function(indent.with, type=get_Rd_tag(indent.with)){
    if(is.null(type)) type = 'TEXT'
    if(is.character(indent.with)){
        assert_that(is_nonempty_string(indent.with))
        if (is(indent.with, 'character')) {
            indent.with <- .Rd(Rd_string(indent.with, type))
        } else
        if (is(indent.with, 'Rd_string')){
            assert_that(is_Rd_string(indent.with, c('TEXT', 'RCODE')))
            indent.with <- .Rd(Rd_string(indent.with, type))
        }
    }
    if (is(indent.with, 'Rd')){
        assert_that( length(indent.with) == 1
                   , is_Rd_string(indent.with[[1]], c('TEXT', 'RCODE'))
                   , msg = "unacceptable Rd: If passed as an Rd container" %<<%
                           "indent.with must contain a single Rd_string" %<<%
                           "that is acceptable as an indent." )
        assert_that( is_whitespace(indent.with[[1]])
                   , !grepl('\\n', indent.with[[1]])
                   , msg = "indents must be whitespace with no newlines")
        if (grepl('\\t', indent.with[[1]]))
            pkg_warning( type='guidelines_violation'
                       , ._("Tabs are discouraged from being used for indentation" %<<%
                            "as they may not be rendered properly on all possible pagers." %<<%
                            "See https://developer.r-project.org/Rds.html for reference.")
            )
        return(indent.with)
    }
    else pkg_error("bad indent")
}
if(FALSE){#@testing Rd_clean_indent expected errors
    expect_error( Rd_clean_indent('a')
                , "indents must be whitespace with no newlines"
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent('  \n')
                , "indents must be whitespace with no newlines"
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent(c(' ', ' '))
                , ".+indent\\.with.+ does not conform to a non-empty string"
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent(s(c(' ', ' '), Rd_tag='TEXT', class='Rd_string'))
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent(Rd_verb('  '))
                , "'Rd_tag' attribute is not in allowed tags"
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent(.Rd(Rd_text("  "), Rd_alias('test')))
                , "unacceptable Rd"
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent(.Rd(Rd_verb('  ')))
                , "unacceptable Rd"
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent(.Rd(.Rd(Rd_text("  "))))
                , "unacceptable Rd"
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent(.Rd(Rd_text("...")))
                , "indents must be whitespace with no newlines"
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent(.Rd(Rd_text("  \n")))
                , "indents must be whitespace with no newlines"
                , class="Rd::Rd_clean_indent-error-assertion failure")
    expect_error( Rd_clean_indent(TRUE)
                , "bad indent"
                , class="Rd::Rd_clean_indent-error")
}
if(FALSE){#@testing Rd_clean_indent warning
    expect_warning( Rd_clean_indent(.Rd(Rd_text("\t")))
                  , "Tabs are discouraged from being used for indentation")
    expect_warning( Rd_clean_indent(.Rd(Rd_text("\t")))
                  , class="Rd::Rd_clean_indent-warning-guidelines_violation")
}
if(FALSE){#@testing Rd_clean_indent expected output
    expect_identical(Rd_clean_indent('  '), .Rd(Rd_text('  ')))
    expect_identical(Rd_clean_indent(Rd_text('  ')), .Rd(Rd_text('  ')))
    expect_identical(Rd_clean_indent(Rd(Rd_text('  '))), .Rd(Rd_text('  ')))
    expect_identical(Rd_clean_indent(Rd(Rd_rcode('  '))), .Rd(Rd_rcode('  ')))
    expect_identical(Rd_clean_indent(Rd_rcode('  ')), .Rd(Rd_rcode('  ')))
}


#' Indent Rd
#'
#' @param rd an [Rd] container or an [Rd tag][Rd_tag()].
#' @param indent.with What to indent with. See [Rd_clean_indent()].
#' @param recursive Indent recursively?
#' @param ... Ignored but included for forward compatibility
#'            and to force full names on subsequent parameters.
#' @param no.first if the first element should be indented.
#' @param .check check for valid Rd?
#'
#' @export
#' @examples
#' (x <- Rd_description("line 1\n", "line 2\n"))
#' Rd_indent(x)
Rd_indent <-
function( rd   #< Rd object.
        , indent.with = getOption("Rd::indent.with", "  ")
        , recursive = TRUE
        , ...
        , no.first = is(rd, 'Rd_tag')
        , .check=TRUE
        ){
    if (.check) assert_that(is_valid_Rd_list(rd))
    if (recursive) for (i in seq_len(length(rd))) {
        if ( is.list(rd[[i]]) && ( Rd_spans_multiple_lines(rd[[i]]))
           ) {
            lists <- sapply(rd, inherits, c('Rd_tag', 'Rd')) &
                     sapply(rd, Rd_spans_multiple_lines)
            rd[[i]] <- Recall( rd[[i]]
                             , Rd_indent
                             , indent.with  = indent.with
                             , recursive = recursive
                             , ...
                             , no.first = ( i > 1 && !tail(Rd_ends_with_newline(rd[[i-1]]), 1))
                             , .check=FALSE
                             )
        }
    }
    if (Rd_spans_multiple_lines(rd)) rd
    parts <- Rd_split(rd)
    type <- if (length(rd)>0 && is_Rd_string(rd[[1]], 'RCODE')) 'RCODE'
            else 'TEXT'
    indent.with <- Rd_clean_indent(indent.with, type)
    indents <- ifelse( sapply(parts, is_Rd_newline)
                     , list(Rd()), list(indent.with))
    if(no.first) indents[[1]] <- Rd()
    val <- undim(rbind( indents, parts))
    val <- fwd(unlist(val, recursive = FALSE), rd)
    Rd_canonize(val)
}
if(FALSE){#@ testing for Rd
    rd <- Rd(Rd_text("hello"))
    expect_identical(Rd_indent(rd, '  '), .Rd(Rd_text('  hello')))
}
if(FALSE){#@testing Rd_indent for Rd_tag
    rd <- Rd_alias('jane doe')
    expect_identical(Rd_indent(rd, .Rd.default.indent), rd)

    rd <- Rd_tag('\\description'
                , Rd_text("a description without\n")
                , Rd_text("a leading newline")
                , .check=FALSE
                )[2:3]
    expect_identical( format(Rd_indent(rd, '             ', indent.first = FALSE))
                    , "\\description{a description without" %\%
                      "             a leading newline" %\%
                      "}")
    expect_identical( Rd_indent(rd, .Rd.default.indent, no.first = FALSE)
                    , Rd_tag('\\description'
                            , Rd_text("  a description without\n")
                            , Rd_text("  a leading newline")
                            , .check=FALSE
                            )[2:3] )
}
if(FALSE){#@testing Rd_indent errors
    expect_error(Rd_indent('hello'), class="Rd-error-assertion failure")
    expect_error(Rd_indent(Rd_text('hello')), class="Rd-error-assertion failure")
}
if(FALSE){#@testing Rd_indent expected output.
    val <- Rd_indent(Rd(Rd_text('hello')), .Rd.default.indent)
    expect_identical(val, .Rd(Rd_text('  hello')))

    expect_warning( val <- Rd_indent(Rd(Rd_text('hello')), '\t')
                  , class = 'Rd-warning-guidelines_violation')
    expect_identical(val, .Rd(Rd_text('\thello')))

    x <- strwrap( collapse(stringi::stri_rand_lipsum(3), '\n\n')
                , width = 72)

    y <- collapse(x, '\n')
    val <- Rd_indent(Rd_tag('\\description', Rd_text(y)))
    expect_Rd_tag(val, '\\description')
    expect_true(length(val)> 5)
    expect_equal( tail(head(as.character(.Rd(val)), -1), -3)
                , paste0(ifelse(x == '', '', '  '), x, '\n'))
}
if(FALSE){#@testing with parsed Rd
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    txt <- Rd_rm_srcref(txt)
    x <- Rd_get_element(txt, '\\examples')

    val <- Rd_indent(x, indent.with = Rd_rcode("    "))
    expect_Rd_tag(val, '\\examples')
    expect_identical( stringi::stri_split_lines1(format(val))
                    , stringi::stri_split_lines1(format(x)) %>%
                        ifelse(. %in% c('\\examples{', '}', ''), ., paste0("    ", .))
                    )
    expect_equal(length(val), length(x))

    x <- Rd_get_element(txt, "\\arguments")[[9]]
    val <- Rd_indent(x, indent.with='  ')
    expect_Rd_tag(val, '\\item')
    exp <- s(.Rd( Rd(Rd_text('n'))
                , .Rd( x[[2]][[1]], x[[2]][[2]], x[[2]][[3]]
                     , Rd_text('  '), x[[2]][[4]]))
            , Rd_tag = "\\item", class='Rd_tag')
    exp <- Rd_canonize_text(exp)
    expect_identical(val, exp)

    x <- Rd_untag(Rd_get_element(txt, "\\arguments")[8:10])
    val <- Rd_indent(x, indent=TRUE, indent.with='  ', no.first=TRUE)
    expect_equal(format(val)
                , "  \\item{n}{number of observations. If \\code{length(n) > 1}, the length" %\%
                  "      is taken to be the number required.}\n" )

    x <- Rd_get_element(txt, "\\arguments")
    val <- Rd_indent(x, indent=TRUE, indent.with='  ')
    expect_equal( format(val)
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
