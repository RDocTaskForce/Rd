keyword.db <- local({
    keyword.lines <- readLines(normalizePath(file.path(R.home('doc'), 'KEYWORDS.db')))
    keyword.descriptions <- gsub("^\\s+", "", sapply(strsplit(keyword.lines, ':', fixed=TRUE), getElement, 2))

    keyword.heirarchy   <- strsplit(sapply(strsplit(keyword.lines, ':', fixed=TRUE), head, 1), '|', fixed=TRUE)
    keyword             <- sapply( keyword.heirarchy, tail, 1)
    keyword.parent.list <- sapply(sapply( keyword.heirarchy, tail, -1), tail, 1)
    keyword.parent      <- unlist(ifelse( sapply( keyword.parent.list, length), keyword.parent.list, NA_character_))

    db <-
    data.frame( KEYWORD     = keyword
              , PARENT      = keyword.parent
              , DESCRIPTION = keyword.descriptions
              )
    rownames(db) = keyword
    db
})
