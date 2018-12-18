### Extensions to S3 class #####
Rd_get_element <- function(x, ..., drop=TRUE){
    if (...length() != 1L)
        pkg_error( "incorrect number of subscripts"
                 , type="invalid_subscripts")
    val <- if (is.character(tag <- ..1)){
        assert_that(is.string(tag))
        tags <- purrr::map_chr(x, get_attr, 'Rd_tag', '')
        i <- which(tags == tag)
        if (length(i) == 0L) pkg_error(._("tag %s not found", sQuote(tag)), type="not_found") else
        if (length(i) >= 2L) pkg_error(._("multiple elements matching tag %s found", sQuote(tag)), type="multiple_found") else
        as.list(x)[[i]]
    } else {
        i <- ..1
        unclass(x)[[...]]
    }
    if (is(val, 'Rd')) return(val)
    if (is.character(val)){
        tag <- get_Rd_tag(val)
        if (val == '\n')
            return(s(val, class=c( 'Rd_newline', 'Rd_' %<<<% tag, 'Rd_tag')))
        else
        if (is.null(tag))
            return(cl(val, 'Rd_TEXT'))
        else
            return(cl(val, c('Rd_' %<<<% tag, 'Rd_tag')))
    }
    if (!is_exactly(val, 'list')) return(val)
    tag <- get_Rd_tag(val)
    if (is.null(tag)) return(s(val, class='Rd'))
    if (!is.null(tag)) {
        return(cl(val, c('Rd_tag', 'Rd')))
    }
}
# @export
# `[[.Rd` <- function(...){Rd_get_element(...)}
# if(FALSE){#@testing [[.Rd & [.Rd
#     test.file <- system.file("examples", "Normal.Rd", package = 'Rd')
#     txt <- tools::parse_Rd(test.file)
#     txt <- Rd_rm_srcref(txt)
#     txt <- Rd_unclass(txt)
#     class(txt) <- 'Rd'
#
#     expect_is_exactly(txt, 'Rd')
#
#     expect_error(txt[[1,2,3]], class="Rd::Rd_get_element-error-invalid_subscripts")
#
#     expect_is_exactly(txt[['\\arguments']], 'Rd_tag')
#     expect_is_exactly(txt[['\\arguments']][[1]], 'Rd_newline')
#     expect_is_exactly(txt[['\\arguments']][[2]], 'Rd_indent')
#     expect_is_exactly(txt[['\\arguments']][[3]], 'Rd_tag')
#     expect_is_exactly(txt[['\\arguments']][[c(3,1)]], 'Rd')
#     expect_is_exactly(txt[['\\arguments']][[c(3,1,1)]], 'Rd_TEXT')
#     expect_is_exactly(txt[['\\arguments']][[c(3,2,1)]], 'Rd_TEXT')
#
#     expect_is_exactly(txt[['\\arguments']][[3L]], 'Rd_tag')
#     expect_is_exactly(txt[['\\arguments']][[3L]][[1L]], 'Rd')
#
#     expect_is_exactly(txt[[2]], "Rd_newline")
#     expect_is_exactly(txt[[c(48, 11)]], "Rd_TEXT")
# }

# @export
# `[.Rd` <- function(x, ..., drop=FALSE){
#     if (!(...length() <= length(dim(x) %||% 1L) ))
#         pkg_error( "incorrect number of subscripts"
#                  , type="invalid_subscripts")
#     if (is.character(tag <- ..1)) {
#         tags <- purrr::map_chr(x, get_attr, 'Rd_tag', '')
#         i <- which(tags %in% tag)
#         return(Recall(x, i, drop=drop))
#     }
#     cl(NextMethod('['), 'Rd')
# }
# if(FALSE){#@testing [[.Rd & [.Rd
#     txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
#     expect_valid(txt)
#     expect_true(is.list(txt))
#
#     expect_is(txt[[10]], "Rd_tag")
#     expect_is(txt[[10]], "Rd")
#     expect_valid(txt[[10]])
#
#     expect_error(txt[1,2,3], class="Rd::[.Rd-error-invalid_subscripts")
#
#     val <- txt[1]
#
#     expect_is_exactly(val, 'Rd')
#     expect_length(val, 1L)
#     expect_true(is.list(val))
#     expect_false(is.list(txt[[1]]))
#     expect_valid(is.list(txt[1]))
#     expect_valid(is.list(txt[[1]]))
#
#     char <- as.character(val)
#     expect_identical(char, "% File src/library/stats/man/Normal.Rd")
#
#     expect_identical( as.character(txt[[10]])
#                     , c("\\name", "{", "Normal", "}"))
#     expect_identical( as.character(txt[[10]])
#                     , as.character(txt[ 10 ])
#                     )
#
#
#     expect_is_exactly(txt[[2L]], 'Rd_newline')
#     expect_is_exactly(txt[['\\arguments']], 'Rd_tag')
#     expect_is_exactly(txt[['\\arguments']][[2L]], 'Rd_indent')
#
#     expect_identical( txt[['\\seealso']]
#                     , txt[[46L]]
#                     )
#     expect_error(txt[['bibbidy']], ._("tag %s not found", sQuote("bibbidy")))
#     expect_error( txt[['\\alias']]
#                 , "multiple elements matching tag")
#
#     expect_identical( txt['\\alias']
#                     , txt[c(12, 14, 16, 18, 20)]
#                     )
#     expect_identical( class(.Rd.newline[[1]])
#                     , c('Rd_newline', 'Rd_TEXT', 'Rd_tag', 'Rd')
#                     )
# }

# `[.Rd_tag` <- function(x, ..., drop=FALSE){
#     s(cl(NextMethod('['), 'Rd_tag')
#      , Rd_tag=attr(x, 'Rd_tag'))
# }
# if(FALSE){#@testing
#     txt <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
#
#     x <- txt[['\\arguments']]
#     expect_is(x, 'Rd_tag')
#     expect_identical(attr(x, 'Rd_tag'), '\\arguments')
#     y <- x[1:6]
#     expect_is(y, 'Rd_tag')
#     expect_identical(attr(y, 'Rd_tag'), '\\arguments')
# }

# @export
# c.Rd  <- function(...){
#     # l <- list(...)
#     # tags <- lapply(l, get_Rd_tag)
#     # is.text <- sapply(l, is.character))
#     # if (all(is.text)){
#     #     if(tags <- )
#     #
#     # }
#
#
#     if (is.list(..1))
#         return(fwd(NextMethod(), ..1))
#     else
#         cl(NextMethod(), 'Rd')
#     # l <- list(...)
#     # is.rd <- sapply(l, is_Rd, strict=FALSE, deep=FALSE)
#     # assert_that(all(is.rd))
#     # lengths <- purrr::map_int(l, length)
#     # val <- vector('list', sum(ifelse(is.rd, lengths, 1L)))
#     # j <- 1L
#     # for (i in seq_along(l))
#     #     if (is.list(l[[i]]) && !is_Rd_tag(l[[i]])) {
#     #         val[seq(j, j+lengths[[i]]-1L)] <- l[[i]]
#     #         j <- j + lengths[[i]]
#     #     } else {
#     #         val[j] <- l[i]
#     #         j <- j + 1L
#     #     }
#     # return(s(val, class='Rd'))
# }
# if(FALSE){#@testing
#     x <- Rd_text('testing')
#     l <- list(.Rd.newline[[1]], x, .Rd.newline[[1]])
#     y <- c(.Rd.newline, x, .Rd.newline)
#     expect_equal(l, unclass(y))
#
#     a <- Rd_text('hello')
#     b <- Rd_text(' ')
#     c <- Rd_text('world')
#     x <- s(list(a, b, c), class='Rd')
#     lst <- c(.Rd.newline, x, .Rd.newline)
#     expect_is(lst, 'Rd')
#     expect_length(lst, 5)
#     expect_is(lst[[1]], 'Rd_newline')
#     expect_is(lst[[2]], 'Rd_TEXT')
#     expect_is(lst[[4]], 'Rd_TEXT')
#     expect_is(lst[[5]], 'Rd_newline')
#
#     content <- c(.Rd.code.newline
#                 , Rd_rcode("require(graphics)\n")
#                 , .Rd.code.newline
#                 , Rd_rcode("dnorm(0) == 1/sqrt(2*pi)\n")
#                 , Rd_rcode("dnorm(1) == exp(-1/2)/sqrt(2*pi)\n")
#                 , Rd_rcode("dnorm(1) == 1/sqrt(2*pi*exp(1))\n")
#                 )
#     expect_is_exactly(content, 'Rd')
#     expect_length(content, 6L)
#
#     expect_is_exactly(unclass(content)[[1]],'Rd_newline')
#     expect_is_exactly(unclass(content)[[3]],'Rd_newline')
#
#     expect_is_exactly(unclass(content)[[2]],'Rd_RCODE')
#     expect_is_exactly(unclass(content)[[4]],'Rd_RCODE')
#     expect_is_exactly(unclass(content)[[5]],'Rd_RCODE')
#     expect_is_exactly(unclass(content)[[6]],'Rd_RCODE')
# }
# if(FALSE){#@testing
#     x <- c( Rd_usage(Rd_rcode("summary(object, \\dots)"))
#           , list( s("summary(", Rd_tag = "RCODE"))
#           , list( s("object, ", Rd_tag = "RCODE"))
#           , Rd_usage(Rd_rcode("\\dots, digits)"))
#           )
#     is_Rd_tag(x, '\\usage')
#
# }

