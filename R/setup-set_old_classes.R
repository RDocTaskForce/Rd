#' @import pkgcond
#' @import postlogic
#' @importFrom assertthat is.count is.flag is.string see_if validate_that
#' @importFrom methods is
#' @importFrom utils bibentry person tail head
#' @importFrom testthat compare
# @importFrom testextra are all_inherit is_exactly class0


# setClass("Rd_object", contains = 'VIRTUAL')
# setClass("Rd_list", contains = c('VIRTUAL', 'Rd_object', 'list'))

setOldClass('bibentry')
setOldClass('person')

setOldClass(c('Rd', 'Rd_list', 'Rd_object', 'list'))
setOldClass(c('Rd_tag', 'Rd_list', 'Rd_object', 'list'))
setOldClass(c('Rd_string', 'Rd_object', 'character'))

# setIs("Rd", "Rd_list")
# setIs("Rd_tag", "Rd_list")
# setIs("Rd_string", "Rd_object")

if(FALSE){#@testing Old classes
    expect_true(is(Rd_text('text'), 'Rd_string'))
    expect_true(is(Rd_text('text'), 'Rd_object'))

    expect_true(is(.Rd(Rd_text('text')), 'Rd'))
    expect_true(is(.Rd(Rd_text('text')), 'Rd_list'))
    expect_true(is(.Rd(Rd_text('text')), 'Rd_object'))

    expect_true(is(Rd_tag('\\strong', Rd_text('text'), .check=FALSE), 'Rd_tag'))
    expect_true(is(Rd_tag('\\strong', Rd_text('text'), .check=FALSE), 'Rd_list'))
    expect_true(is(Rd_tag('\\strong', Rd_text('text'), .check=FALSE), 'Rd_object'))

    expect_false(is(Rd_text('text'), 'character'))
    expect_false(is(.Rd(Rd_text('text')), 'Rd_tag'))
    expect_false(is(.Rd(Rd_text('text')), 'list'))
    expect_false(is(Rd_tag('\\strong', Rd_text('text'), .check=FALSE), 'Rd'))
    expect_false(is(Rd_tag('\\strong', Rd_text('text'), .check=FALSE), 'list'))
}

setAs('Rd_tag', 'Rd', function(from){
    assert_that(is_valid_Rd_list(from))
    .Rd(from)
})
