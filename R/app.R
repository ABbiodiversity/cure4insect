## files that need to be available as part of the package for OpenCPU
app_read_csv <- function(...) read.csv(...)

app_test <- function(...) {
    l <- list(...)
    for (i in seq_along(l))
        l[[i]] <- paste0(names(l)[i], ": ", l[[i]])
    rval <- paste(l, collapse="\n\n")
    .send_email(l$address, mimepart=rval)
    rval
}

