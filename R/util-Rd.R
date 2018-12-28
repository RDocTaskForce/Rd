### Rd Utilities #####


Rd_rm_srcref <- function(rd){
    attr(rd, 'srcref') <- NULL
    attr(rd, 'macros') <- NULL
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
    assert_that(is_valid_Rd_list(l))
    val <- if (all(are_Rd_strings(l, 'RCODE')))
            Rd_canonize(cl(undim(rbind(l, .Rd(Rd_rcode("\n")))), 'Rd'), ...)
        else
            Rd_canonize(cl(undim(rbind(l, .Rd(Rd_text("\n")))), 'Rd'), ...)
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

ensure_ends_with_newline <- function(rd, .check=TRUE){
    if (is.list(rd)){
        if(.check)
            assert_that(all(are_Rd_strings(rd, c('RCODE', 'TEXT'))))
        lapply(rd, ensure_ends_with_newline, .check=FALSE)
    } else {
        if(.check)
            assert_that(is_Rd_string(rd, c('RCODE', 'TEXT')))
        if (Rd_ends_with_newline(rd)) return(rd)
        fwd(paste0(rd, '\n'), rd)
    }
}
if(FALSE){#@testing
    expect_identical( ensure_ends_with_newline(Rd_text("testing"))
                    , Rd_text("testing\n"))
    expect_identical( ensure_ends_with_newline(Rd_rcode("testing"))
                    , Rd_rcode("testing\n"))
    expect_identical( ensure_ends_with_newline(
                        list( Rd_rcode("testing()")
                            , Rd_rcode("test_me()")
                            ))
                    , list( Rd_rcode("testing()\n")
                          , Rd_rcode("test_me()\n")
                          ))
    expect_identical( ensure_ends_with_newline(
                        list( Rd_rcode("testing()\n")
                            , Rd_rcode("test_me()")
                            ))
                    , list( Rd_rcode("testing()\n")
                          , Rd_rcode("test_me()\n")
                          ))
    expect_identical( ensure_ends_with_newline(
                        list( Rd_rcode("testing()\n")
                            , Rd_rcode("test_me()\n")
                            ))
                    , list( Rd_rcode("testing()\n")
                          , Rd_rcode("test_me()\n")
                          ))
}

### Testing Helpers ####
expect_Rd_string <- function(object, tag, info=NULL, label = NULL){
    requireNamespace('testthat')
    act <- testthat::quasi_label(rlang::enquo(object), label)
    val <- see_if(is_Rd_string(object, tag, strict=TRUE, reason = TRUE))
    testthat::expect(val, act$label %<<% attr(val, 'msg'), info)
}
expect_Rd_tag <- function(object, tag, info=NULL, label = NULL){
    requireNamespace('testthat')
    act <- testthat::quasi_label(rlang::enquo(object), label)
    val <- see_if(is_Rd_tag(object, tag, strict=TRUE, reason = TRUE))
    testthat::expect(val, act$label %<<% attr(val, 'msg'), info)
}
expect_Rd_bare <- function(object, info=NULL, label = NULL){
    requireNamespace('testthat')
    act <- testthat::quasi_label(rlang::enquo(object), label)
    val <- see_if(is_Rd(object, strict=TRUE))
    testthat::expect(val, act$label %<<% attr(val, 'msg'), info)
}
