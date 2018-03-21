## single species stuff
library(knitr)
(f1 <- system.file("doc", "example-species-report.Rmd", package = "cure4insect"))
knit(f1)

## multi species stuff
(f2 <- system.file("doc", "custom-report.R", package = "cure4insect"))
source(f2)

## poly level prediction
## ... to be added ...

## spelling
if (FALSE) {
    library(spelling)
    library(hunspell)
    check_spelling <- function(x)
        sort(unique(unlist(hunspell(readLines(x), format = "html"))))

    spell_check_package("~/repos/cure4insect")

    check_spelling("~/repos/cure4insect/README.md")
    check_spelling("~/repos/cure4insect/inst/www/index.html")
    check_spelling("~/repos/cure4insect/inst/doc/example-species-report.Rmd")
}
