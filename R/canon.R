#' @title Rd Canonical Form
#'
#' @description
#' Canonical form is simply described as that which would come out from reading
#' an Rd file via, [tools::parse_Rd()].
#'
#' @details
#'
#' **Canonical Rd Text has:**
#'
#' * One line per element, with `attr(., 'Rd_tag')=='TEXT'`
#' * The indents are merged with content if the first content is text.
#' * Newlines are contained with the content provided the content is 'TEXT',
#'   but the newline must be the last character in the string and cannot appear anywhere else.
#' * Comments are a separate class and do not include the newline.
#'
#' **Canonical R code follows the following rules:**
#'
#' * One element per line of code.
#' * newline is included at the end of the line string,
#'   not as a separate element.
#' * if there are multiple lines they are bound together in an Rd or Rd_tag list.
#'
#' @param rd the Rd container object to put in canonical form.
#' @param .check Perform input checks?
#' @inheritDotParams is_valid_Rd_object
#' @export
#' @examples
#' ## Rd_c does not guarantee canonical code.
#' x <- Rd_c(Rd('Testing'), Rd('\n'))
#' str(x)
#' str(Rd_canonize(x))
Rd_canonize <- function(rd, ..., .check=TRUE){
    if (!isFALSE(.check))
        assert_that( is_valid_Rd_object(rd, ...)
                   , msg = "Rd must be valid before it can be put in cannonical form."
                   )
    if (is.character(rd))
        rd <- .Rd(cl(rd, 'Rd_string'))
    if (is(rd, 'list'))
        if (is.null(get_Rd_tag(rd)))
            rd <- cl(rd, 'Rd')
        else
            rd <- cl(rd, 'Rd_tag')
    rd <- Rd_canonize_text(rd)
    rd <- Rd_canonize_code(rd)
    return(rd)
}
if(FALSE){#@testing
    rd <- Rd_text("a\nb\nc\n")
    expect_is(rd, 'Rd_string')
    expect_true(is.character(rd))
    expect_length(rd, 1)

    expect_error(Rd_canonize_text(rd))

    val <- Rd_canonize_text(Rd(rd))
    expected <- Rd_unclass(.Rd( Rd_text('a\n')
                              , Rd_text('b\n')
                              , Rd_text('c\n')
                              ))
    expect_identical( Rd_unclass(val), expected)
    expect_identical(Rd_canonize(val), val)

    rd <- Rd_tag( "\\examples"
                , Rd_rcode("\n")
                , Rd_rcode("x<- rnorm(100)\n")
                , Rd_rcode("plot(x)\n"))
    expect_true(is_valid_Rd_list(rd))

    expect_identical(Rd_canonize_text(rd), rd)
    expect_identical(Rd_canonize_code(rd), rd)
    expect_identical(Rd_canonize(rd), rd)
    expect_identical(Rd_unclass(Rd_canonize(Rd_unclass(rd))), Rd_unclass(rd))

    expect_identical(Rd_canonize_code(Rd_tag('\\examples'
                                            , Rd_rcode("\nx<- rnorm(100)\nplot(x)\n")))
                    , rd)

    rd <- .Rd(Rd_text("use the \\backslash to escape.")
             , Rd_text("and '{}' to group.")
             )
    val <- Rd_canonize_text(rd)
    expect_is_exactly(val, 'Rd')
    expect_length(val, 1L)
    expect_true(is_Rd_string(val[[1]], 'TEXT'))
    expect_length(val[[1]], 1L)
}
if(FALSE){#@testing Rd_canonize with output from parse_Rd
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    txt <- Rd_rm_srcref(txt)
    expect_identical( Rd_unclass(val <- Rd_canonize_code(rd <- Rd_get_element(txt, '\\examples')))
                    , Rd_unclass(rd)
                    )

    desc <- Rd_get_element(txt, '\\description')
    canonical <- Rd_canonize_text(desc)
    expect_identical( as.character(desc)
                    , as.character(canonical)
                    )

    expect_identical(Rd_unclass(Rd_canonize_text(txt)), Rd_unclass(txt))
    expect_identical(Rd_unclass(Rd_canonize_code(txt)), Rd_unclass(txt))
    expect_identical(Rd_unclass(Rd_canonize     (txt)), Rd_unclass(txt))

    x <- Rd_text("test strings\nsecond line")
    val <- Rd_canonize(x)
    expected <- Rd(Rd_text("test strings\n"), Rd_text("second line"))
    expect_identical(val, expected)

    expect_identical(Rd_canonize_text(Rd.newline), Rd.newline)

    x <- .Rd( Rd_tag("\\item"), Rd_text(" "), Rd_text("content"))
    expect_identical(Rd_canonize_text(x)[[1]], Rd_tag('\\item'))
    expect_identical(Rd_canonize_text(x)[[2]], Rd_text(' content'))
}
if(FALSE){#@testing Rd_canonize with unclassed arguments
    x <- unclass(Rd_text('test'))
    expect_is(x, 'character')
    val <- Rd_canonize(x)
    expect_Rd_bare(val)
    expect_Rd_string(unclass(val)[[1]], 'TEXT')

    x <- unclass(Rd_tag('\\bold', 'text'))
    expect_is(x, 'list')
    expect_equal(get_Rd_tag(x), '\\bold')
    val <- Rd_canonize(x)
    expect_is(val, 'Rd_tag')
    expect_Rd_tag(val, '\\bold')
    expect_Rd_string(unclass(val)[[1]], 'TEXT')

    y <- unclass(Rd(x))
    y[[1]] <- unclass(y[[1]])
    expect_is(y, 'list')
    expect_is(y[[1]], 'list')
    val.y <- Rd_canonize(y)
    expect_is(val.y, 'Rd')
    expect_is(unclass(val.y)[[1]], 'Rd_tag')
    expect_Rd_bare(val.y)
    expect_Rd_tag(unclass(val.y)[[1]], '\\bold')
    expect_Rd_string(unclass(val.y)[[1]][[1]], 'TEXT')
}

#' @describeIn Rd_canonize Put text in canonical form.
#' @export
Rd_canonize_text <- function(rd, .check = TRUE, ...){
    if (!isFALSE(.check))
        assert_that(is_valid_Rd_list(rd, ...))
    if (length(rd)==0 || is_Rd_string(rd)) return(rd)
    are.strings <- are_Rd_strings(rd, 'TEXT')
    if (all(are.strings)) {
        lines <- unlist(stringi::stri_split(collapse0(unlist(rd)), regex="(?<=\\n)"))
        lines <- lines[nchar(lines) > 0L]

        if (length(lines) == 1){
            if (lines =='\n') return(Rd.newline)
            return(forward_attributes(list(Rd_text(lines)), rd))
        }
        # lines <- ifelse( lines == '\n'
        #                , .Rd.newline
        #                , ifelse( is_whitespace(lines)
        #                        , lapply(lines, structure
        #                                , Rd_tag="TEXT"
        #                                , class='Rd_string')
        #                        , lapply(lines, Rd_text))
        #                        )
        lines <- lapply(lines, structure, Rd_tag="TEXT", class='Rd_string')
        return(fwd(lines, rd))
    } else if (!any(purrr::map_lgl(rd, is.list))){
        return(rd)
    } else if (!any(are.strings)) {
        return(fwd(lapply(rd, Rd_canonize_text), rd))
    } else {
        group <- cumsum(abs(!are.strings | c(FALSE, head(!are.strings, -1))))
        is.text <- purrr::map_lgl(split(are.strings, group), all)
        parts <- split(rd, group)

        for (j in seq_along(parts))
            parts[[j]] <- Recall(parts[[j]], .check=FALSE)

        return(fwd( unlist(parts, FALSE, FALSE), rd))
    }
}
if(FALSE){#@testing
    expect_error(Rd_canonize_text(Rd_text('\n')))
    expect_identical(Rd_canonize_text(Rd_text('\n'), .check=FALSE), Rd_text('\n'))

    x <- .Rd( Rd_text("    ")
            , Rd_text("hello")
            , Rd_text("\n    ")
            , Rd_text("world")
            )
    val <- Rd_canonize_text(x)
    expect_is(val, 'Rd')
    expect_length(val, 2)
    expect_identical(val, .Rd( Rd_text("    hello\n")
                             , Rd_text("    world")
                             ))

    reclaimed <- Rd_rm_srcref(s(tools::parse_Rd(textConnection(collapse0(x))), macros=NULL))
    val <- Rd_canonize_text(cl(c(x, .Rd(Rd_text('\n'))), 'Rd'))
    expect_identical(Rd_unclass(val), Rd_unclass(reclaimed))
}

#' @describeIn Rd_canonize Put R code in canonical form.
#' @export
Rd_canonize_code <- function(rd, .check=TRUE, ...){
    if (!isFALSE(.check))
        assert_that(is_valid_Rd_list(rd, ...))
    if (length(rd)==0 || is_Rd_string(rd)) return(rd)
    are.code <- are_Rd_strings(rd, 'RCODE')
    if (any(are.code)) {
        type <- unique(purrr::map_chr(rd, get_attr, 'Rd_tag', ''))
        assert_that(length(type)==1L
                   , msg = "RCODE type strings may not appear in a " %<<%
                           "container with any other type." )

        lines <- stringi::stri_split(collapse0(unlist(rd)), regex="(?<=\\n)")[[1]]
        lines <- lines[nchar(lines)>0L]
        lines <- lapply(lines, Rd_rcode)
        return(forward_attributes(lines, rd))

    } else {
        return(forward_attributes( lapply(rd, Rd_canonize_code, .check=FALSE), rd))
    }
}
if(FALSE){#@testing
    x <- Rd_tag( "\\usage"
               , Rd_rcode("\n")
               , Rd_rcode('value \\%if\\% proposition'), Rd_rcode("\n")
               , Rd_rcode('value \\%if\\% proposition \\%otherwise\\% alternate'), Rd_rcode("\n")
               )
    expected <- Rd_tag( "\\usage"
               , Rd_rcode("\n")
               , Rd_rcode('value \\%if\\% proposition\n')
               , Rd_rcode('value \\%if\\% proposition \\%otherwise\\% alternate\n')
               )
    expect_identical( Rd_canonize_code(x), expected)
    expect_identical( Rd_canonize(x), expected)
    expect_identical( Rd_canonize_text(x), x)


    bad <- .Rd( Rd_text("\n")
              , Rd_rcode('value \\%if\\% proposition'), Rd_rcode("\n")
              , Rd_rcode('value \\%if\\% proposition \\%otherwise\\% alternate'), Rd_rcode("\n")
              )
    expect_error(Rd_canonize_code(bad)
                , "RCODE type strings may not appear in a  container with any other type\\.")
}
