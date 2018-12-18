### Tag Convenience Functions #####

Rd_alias <- function(alias){Rd_tag('alias', Rd_symb(alias)) %if% assert_that(length(alias)==1)}
if(FALSE){



}

Rd_aliases <- function(aliases){Rd_lines(lapply(aliases, Rd_alias), 'Rd')}
Rd_author <- function(author){
    if (is(author, 'person'))
        author <- toRd(author)
    else
        assert_that( is(author, 'Rd'), msg="author must be of `Rd` or `person` class.")
    if (!is_exactly(author, 'Rd')) author <- Rd(author)
    Rd_tag('author', content=author)
}
Rd_arguments <-
function( ...
        , items = list(...)
        , indent= TRUE
        , indent.with=indent.with
        ){
    indent.with <- Rd_clean_indent(indent.with)
    assert_that( all_are_tag(items, '\\item') )
    content <- rbind( .Rd.newline
                    , indent.with %if% indent
                    , items
                    )
    Rd_tag(tag='arguments', content=content, opt=NULL, wrap.lines = FALSE)
}
Rd_code <- function(x){Rd_tag('code', Rd_rcode(x))}
Rd_concept <- function(name){Rd_tag('concept', Rd_text(name))}
Rd_concepts <- function(concepts){Rd_lines(lapply(concepts, Rd_concept), 'Rd')}
Rd_description <- function(...) {Rd_tag("description", content=compact_Rd(Rd(...)))}
Rd_examples <- function(..., content=list(...), opt=NULL) {
    Rd_tag('examples', content=content, opt=opt, wrap.lines=FALSE)
}
Rd_item <- function(arg, description) {
    s( list(Rd(arg), Rd(description))
     , Rd_tag = "\\item"
     , class = c('Rd_tag', 'Rd'))
}
Rd_keyword <- function(name){Rd_tag('keyword', Rd_text(name))}
Rd_keywords <- function(keys){Rd(lapply(keys, Rd_keyword))}
Rd_name <- function(name){Rd_tag('name', Rd_symb(name))}
Rd_title <- function(title){Rd_tag('title', Rd_text(title))}
Rd_usage <- function(..., content=compact_Rd(Rd(...))){
    val <- Rd_tag('usage', content=content, opt=NULL, wrap.lines = FALSE)
    assert_that(all_are_tag(val, c('RCODE', '\\S3method', '\\S4method')))
    return(val)
}
Rd_value <- function(value){Rd_tag('value', content=value)}
if(FALSE){#@testing Rd_* tags
    rd <- tools::parse_Rd(system.file("examples", "Normal.Rd", package = 'Rd'))
    txt <- Rd_rm_srcref(rd)

    expect_identical(Rd_alias('Normal'), txt[[12L]])

    expect_identical(Rd_author(Rd('My Name')), Rd_author(Rd_text('My Name')))

    expect_equal( val <- Rd_arguments( Rd_item("x, q", "vector of quantiles.")
                              , Rd_item('p', "vector of probabilities.")
                              , indent = TRUE
                              , indent.with = "  "
                              ), x <- txt[['\\arguments']][1:7])

    desc <- Rd_description( .Rd.newline
                          , Rd_text("  Density, distribution function, quantile function and random\n")
                          , Rd_text("  generation for the normal distribution with mean equal to ")
                            ,  Rd_tag('code', Rd_rcode('mean')), .Rd.newline
                          , Rd_text("  and standard deviation equal to ")
                            , Rd_tag('code', Rd_rcode('sd'))
                          , Rd_text(".\n")
                          )
    expect_identical( collapse0(as.character(desc))
                    , collapse0(as.character(txt[['\\description']])))

    expect_identical( Rd_examples( .Rd.code.newline
                                 , Rd_rcode("require(graphics)\n")
                                 , .Rd.code.newline
                                 , Rd_rcode("dnorm(0) == 1/sqrt(2*pi)\n")
                                 , Rd_rcode("dnorm(1) == exp(-1/2)/sqrt(2*pi)\n")
                                 , Rd_rcode("dnorm(1) == 1/sqrt(2*pi*exp(1))\n")
                                 )
                      , txt[[52]][1:6] )
    expect_identical( Rd_examples( content=Rd( .Rd.code.newline
                                             , Rd_rcode("require(graphics)\n")
                                             , .Rd.code.newline
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

    expect_identical(Rd_usage( .Rd.code.newline
                               , Rd_rcode("dnorm(x, mean = 0, sd = 1, log = FALSE)\n")
                               , Rd_rcode("pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)\n")
                               , Rd_rcode("qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)\n")
                               , Rd_rcode("rnorm(n, mean = 0, sd = 1)\n")
    ), txt[['\\usage']])
}

