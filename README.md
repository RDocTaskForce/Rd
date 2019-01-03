
<!-- README.md is generated from README.Rmd. Please edit that file -->
Rd <img src="man/figures/logo.png" align="right" height=140/>
=============================================================

[![Travis build status](https://travis-ci.org/RDocTaskForce/Rd.svg?branch=master)](https://travis-ci.org/RDocTaskForce/Rd) [![Coverage status](https://codecov.io/gh/RDocTaskForce/Rd/branch/master/graph/badge.svg)](https://codecov.io/github/RDocTaskForce/Rd?branch=master) [![CRAN version](http://www.r-pkg.org/badges/version/Rd)](https://cran.r-project.org/package=Rd) [![life-cycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

The goal of Rd is to extend the base utilities in the `tools` and `utils` package to facilitate creation, checking and manipulation of R documentation (Rd) objects in code.

Installation
------------

You can install the released version of Rd from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("Rd")
```

Examples
--------

This is a basic example for constructing an Rd document from scratch:

``` r
## Recreating the first few lines of the `example` help file. 
R <- Rd_tag("\\R")
Rd( Rd_name('example'), '\n'
  , Rd_alias('example'), '\n'
  , Rd_title("Run an Examples Section from the Online Help"), '\n'
  , Rd_description( "Run all ", R, " code from the "
                  , Rd_tag("\\bold", "Examples"), " part of \n"    
                  , R, "'s online help topic ", Rd_code("topic")
                  , " with possible exceptions \n"
                  , Rd_code("dontrun"), ", "
                  , Rd_code("dontshow"), ", and "
                  , Rd_code("donttest"), ",\n see "
                  , Rd_tag("\\sQuote", "Details"), " below."
                  )
  )
#> \name{example}
#> \alias{example}
#> \title{Run an Examples Section from the Online Help}
#> \description{
#> Run all \R code from the \bold{Examples} part of 
#> \R's online help topic \code{topic} with possible exceptions 
#> \code{dontrun}, \code{dontshow}, and \code{donttest},
#>  see \sQuote{Details} below.
#> }
```

Note that the line breaks are unnecessary for functionality, they are included

Details and Internals
=====================

Differences from core R
-----------------------

Rd Objects are composed of character strings and lists of character strings or nested lists ultimately ending in character strings. The primary tool for reading in Rd object is the `tools::parse_Rd()` function. This will return a list with the base object being an `Rd` object but none of the elements contained are classed. The `Rd` package extends this with providing more methods and functions for handling these. In `Rd` all objects are S3 classed, including tags, text, code, and comments.

Classes
-------

### `Rd`

This exists in the base R and is what is returned by `Rd_text('test')`. The `Rd` package adds S3 Methods `[[`, `[`, `c`, and `format`. `Rd` objects must be lists to be valid.

### `Rd_string`

The `Rd` text is a character string, a length one non-empty character vector. It contains several different categories of text which is specified in the `Rd_tag` attribute:

-   `"TEXT"` - The generic text type
-   `"RCODE"` - Tagged as R code, but note that this should be wrapped by an appropriate `Rd_tag` object described below.
-   `"VERB"` - Symbols such as function names.
-   `"COMMENT"` - Comments in Rd, each should lead with the comment character `'%'`.
-   `"UNKNOWN"` - If it cannot be categorized.

It should be noted that the different types of `Rd_string` is for contextual identification, but for printing and formatting all are returned verbatim.

### `Rd_tag`

The `Rd_tag` objects are converted to the latex like `\tag` commands when formatted for printing or writing files. The tag to be used is stored in the `Rd_tag` attribute.
