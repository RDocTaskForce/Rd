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



#' Compact a list into an Rd vector
#'
#' When creating Rd from other objects it is common to
#' iterate over a vector or list and convert each [to Rd](toRd())
#' then combine the results. However [toRd()] can return an [Rd_string],
#' [Rd_tag], or an [Rd] container. to combine these together
#' intelligently, use `Rd_compact` which converts each to an Rd
#' container, placing strings and tags in a container, then
#' concatenating all together.
#'
#' @param l list of Rd objects.
#'
#' @export
Rd_compact <- function(l){
    assert_that( is.list(l)
               , all(purrr::map_lgl(l, is_valid_Rd_object))
               )
    if (is(l, 'Rd_tag')) return(.Rd(l))
    for (i in seq_along(l)) if (!is_exactly(l[[i]], 'Rd'))
        l[[i]] <- .Rd(l[[i]])
    cl(unlist(l, recursive = FALSE), 'Rd')
}
if(FALSE){#@testing
    l <- list( Rd_text('testing ')
             , Rd_code('Rd_compact()')
             , Rd(' with a list of Rd objects.')
             )
    expect_is_exactly(l[[1]], 'Rd_string')
    expect_is_exactly(l[[2]], 'Rd_tag')
    expect_is_exactly(l[[3]], 'Rd')
    val <- Rd_compact(l)
    expect_length(val, 3)
    expect_is_exactly(val, 'Rd')
    expect_is_exactly(val[[1]], 'Rd_string')
    expect_is_exactly(val[[2]], 'Rd_tag')
    expect_is_exactly(val[[3]], 'Rd_string')
}
