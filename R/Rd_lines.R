#' Combine with line breaks
#'
#' Combine a list of Rd objects with a line break between each.
#'
#' @param l A list of valid Rd objects.
#' @inheritDotParams Rd_canonize
#'
#' @export
Rd_lines <- function(l, ...){
    assert_that(is_valid_Rd_list(l))
    val <- if (all(are_Rd_strings(l, 'RCODE')))
            cl(undim(rbind(l, .Rd(Rd_rcode("\n")))), 'Rd')
        else if(!is_Rd_tag(l) && all_inherit(l, 'Rd'))
            unlist(undim(rbind(l, list(.Rd(Rd_text("\n"))))), FALSE)
        else
            undim(rbind(l, .Rd(Rd_text("\n"))))
    val <- Rd_canonize(cl(val, 'Rd'), ...)
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
if(FALSE){#@ Rd_lines combining Rd together
    l <- list( Rd(Rd_name('testing'))
             , Rd_aliases(c('test', 'testing'))
             , Rd(Rd_title('A test title'))
             , Rd(Rd_description('This is for testing purposes only.'))
             )
    expect_identical( Rd_lines(l)
                    , Rd_c( l[[1]], Rd('\n')
                          , l[[2]], Rd('\n')
                          , l[[3]], Rd('\n')
                          , l[[4]]
                          ))
}
