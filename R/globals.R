### Global Variables #####
.Rd.string.tags <- c("TEXT", "RCODE", "VERB", "COMMENT", "UNKNOWN", "LIST")
.Rd.newline <- s(list(s( "\n"
                       , Rd_tag="TEXT"
                       , class=c('Rd_newline', 'Rd_TEXT', 'Rd_tag')
                       )), class= 'Rd')
.Rd.code.newline <- s(list(s( "\n"
                       , Rd_tag="RCODE"
                       , class=c('Rd_newline', 'Rd_RCODE', 'Rd_tag', 'Rd')
                       )), class= 'Rd')
.Rd.break <- s(list(s('\n\n'
                     , Rd_tag = 'TEXT'
                     , class = c('Rd_break', 'Rd_newline', 'Rd_tag', 'Rd')
                     )), class='Rd')
.Rd.text.classes <- c('Rd_TEXT', 'Rd_indent', 'Rd_newline', 'NULL')
