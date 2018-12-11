### Rd Utilities #####
#nocov start
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
get_Rd_tag <- function(x)get_attr(x, 'Rd_tag')
#nocov end
.Rd_get_indent <- function(x){
    if (!inherits(x, 'Rd')){
        ws <- gsub("^( *)[^ ].*$", "\\1", x)
        if (length(x)==1) {
            if (nchar(ws)==0) return(NULL)
            else return(s( ws
                         , Rd_tag = "TEXT"
                         , class = c('Rd_indent', 'Rd_TEXT', 'Rd_tag', 'Rd')
                         ))
        } else {
            sapply(ws, function(ws)
                if (nchar(ws)==0L) return(NULL) else
                s( ws
                 , Rd_tag = "TEXT"
                 , class = c('Rd_indent', 'Rd_TEXT', 'Rd_tag', 'Rd')
                 ))
        }
    } else {
        indent <- NULL
        for (i in seq_along(x)) {
            if (!inherits(x[[i]], 'Rd_indent')) break
            else indent <- paste0(indent, x[[i]])
        }
        if (is.null(indent)) return(NULL)
        return(s( indent
                , Rd_tag = "TEXT"
                , class = c('Rd_indent', 'Rd_TEXT', 'Rd_tag', 'Rd')
                ))
    }
}
if(FALSE){
    x <- "   hello world"
    indent <- .Rd_get_indent(x)
    expect_is(indent, 'Rd_indent')
    expect_is(indent, 'Rd_TEXT')
    expect_is(indent, 'Rd_tag')
    expect_is(indent, 'Rd')
    expect_equivalent(unclass(indent), "   ")
    expect_null(.Rd_get_indent("hello world"))

    x <- c( 'hello', ' big', '  wide', '   world')
    indent <- .Rd_get_indent(x)
    expect_null(indent[[1]])
    expect_all_inherit(indent[-1], 'Rd_indent')
    expect_all_inherit(indent[-1], 'Rd_TEXT')

    x <- Rd_text(c("     hello world"))
    expect_is(x[[1]], 'Rd_indent')
    indent <- .Rd_get_indent(x)
    expect_identical(indent, x[[1]])
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
             , Rd_rcode("proposition \\%otherwise\\% alternate"))
    exp <- Rd( Rd_rcode("value \\%if\\% proposition\n")
             , Rd_rcode("proposition \\%otherwise\\% alternate\n"))
    val <- Rd_lines(l)
    expect_identical(val, exp)
}
