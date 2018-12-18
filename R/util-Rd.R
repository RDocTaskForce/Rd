### Rd Utilities #####
Rd_rm_srcref <- function(rd){
    attr(rd, 'srcref') <- NULL
    if (is.list(rd)) for(i in seq_along(rd))
        rd[[i]] <- Recall(rd[[i]])
    return(rd)
}
Rd_unclass <- function(rd){
    attr(rd, 'class') <- NULL
    if (is.list(rd)) for(i in seq_along(rd))
        rd[[i]] <- Recall(rd[[i]])
    return(rd)
}
Rd_untag <- function(x)s(x, Rd_tag=NULL, class='Rd')
get_Rd_tag <- function(x, ...)get_attr(x, 'Rd_tag', ...)
if(FALSE){#@testing cleanup utilities.
    txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    expect_is(txt, 'Rd')
    expect_false(is.null(attr(txt, 'srcref')))
    expect_false(is.null(attr(txt[[1]], 'srcref')))

    txt <- Rd_rm_srcref(txt)
    expect_true(is.null(attr(txt, 'srcref')))
    expect_true(is.null(attr(txt[[1]], 'srcref')))

    lst <- Rd_unclass(txt)
    expect_is_exactly(lst, "list")
    expect_is_exactly(lst[[1]], "character")

    expect_equal(get_Rd_tag(txt[[1]]), "COMMENT")
    expect_null(get_Rd_tag(Rd_untag(txt[[1]])))
}

Rd_lines <- function(l, ...){
    assert_that(is.list(l), all_inherit(l, 'Rd'))
    val <- if (all_are_tag(l, 'RCODE'))
            Rd_canonize(cl(undim(rbind(l, .Rd.code.newline)), 'Rd'), ...)
        else
            Rd_canonize(cl(undim(rbind(l, .Rd.newline)), 'Rd'), ...)
    if (tail(val, 1L)=='\n')
        val <- head(val, -1L)
    return(val)
}
if(FALSE){#@testing
    l <- list( Rd_rcode("value \\%if\\% proposition")
             , Rd_rcode("proposition \\%otherwise\\% alternate")
             , Rd_rcode('')
             )
    exp <- Rd( Rd_rcode("value \\%if\\% proposition\n")
             , Rd_rcode("proposition \\%otherwise\\% alternate\n"))
    val <- Rd_lines(l)
    expect_identical(val, exp)
}
