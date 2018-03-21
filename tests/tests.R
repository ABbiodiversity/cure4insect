## single species stuff
library(knitr)
(f1 <- system.file("doc", "example-species-report.Rmd", package = "cure4insect"))
knit(f1)

## multi species stuff
(f2 <- system.file("doc", "custom-report.R", package = "cure4insect"))
source(f2)

## poly level prediction
## ... to be added ...
