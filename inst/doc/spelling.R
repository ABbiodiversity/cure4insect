library(spelling)
library(hunspell)
check_spelling <- function(x)
    sort(unique(unlist(hunspell(readLines(x), format = "html"))))

spell_check_package("~/repos/cure4insect")

check_spelling("~/repos/cure4insect/README.md")
check_spelling("~/repos/cure4insect/inst/www/index.html")
check_spelling("~/repos/cure4insect/inst/doc/example-species-report.Rmd")
