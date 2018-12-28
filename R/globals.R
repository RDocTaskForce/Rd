### Global Variables #####
.Rd.string.tags <- c("TEXT", "RCODE", "VERB", "COMMENT", "UNKNOWN", "LIST")

#' @name pieces
#' @aliases Rd.newline Rd.code.newline Rd.break
#' @title Rd Pieces
#' @description
#' These objects are provided to help construct Rd documents.
#'
#' @usage
#' Rd.newline
#' Rd.code.newline
#' Rd.break
NULL

#' @export
Rd.newline <- .Rd(Rd_text('\n'))

#' @export
Rd.code.newline <- .Rd(Rd_rcode('\n'))

#' @export
Rd.break <- .Rd(Rd_text('\n\n'))
