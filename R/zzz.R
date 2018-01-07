.onAttach <- function(libname, pkgname){
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname),
                    fields=c("Version", "Date"))
    packageStartupMessage(paste(pkgname, ver[1], "\t", ver[2]))
    if (is.null(getOption("cure4insect"))) {
        options("cure4insect" = as.list(drop(read.dcf(
            file=system.file("config/defaults.conf", package=pkgname)))))
    }
    invisible(NULL)
}

.onUnload <- function(libpath){
    options("cure4insect" = NULL)
    invisible(NULL)
}
