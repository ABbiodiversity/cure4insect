.send_email <-
function(address=NULL, mimepart=NULL)
{
    if (!is.null(address)) {
        sender <- getOption("cure4insect")$sender
        subject <- "Custom Report"
        body <- list("Hi,\n\nYour custom report results are attached.\n\nWith regards,\n\nthe ABMI Science")
        if (!is.null(mimepart))
            body[[2]] <- mime_part(mimepart, paste0("Custom_Report_", Sys.Date()))
        try(sent <- sendmail(sprintf("<%s>", sender),
            sprintf("<%s>", address),
            subject, body,
            control=list(smtpServer="ASPMX.L.GOOGLE.COM")))
        if (!inherits(sent, "try-error") && .verbose())
            cat("email sent to", address, "\n")
    }
    invisible(NULL)
}

## mime_part for c4iblock class (data frame to be written as csv)
mime_part.c4iblock <-
function (x, name = deparse(substitute(x)), ...)
{
    f <- tempfile()
    on.exit(file.remove(f))
    write.csv(x, file = f, ...)
    .file_attachment(f, name = sprintf("%s.csv", name), type = "text/plain")
}

## attach any file: need applicable type
## file provided instead of an object
## name is the basename (without path but with extension)
## e.g.: .mime_part_zipfile("x.zip", "x.zip", "application/zip")
.mime_part_file <- function(fn, name, type="text/plain", ...)
    .file_attachment(fn, name = name, type = type)

## unexported stuff from sendmailR
.file_attachment <-
function (fn, name, type = "application/octet-stream", disposition = "attachment")
{
    if (missing(name))
        name <- basename(fn)
    text <- base64enc::base64encode(fn, linewidth = 72, newline = "\n")
    headers <- list(`Content-Type` = type, `Content-Disposition` = sprintf("%s; filename=%s",
        disposition, name), `Content-Transfer-Encoding` = "base64")
    .mime_part(headers = headers, text = text)
}
.mime_part <-
function (headers, file = NULL, text = NULL)
{
    if (!is.null(file) && !is.null(text))
        stop("Can only provide file or text for mime part.")
    e <- environment()
    reg.finalizer(e, .mime_part_finalizer, onexit = TRUE)
    class(e) <- "mime_part"
    e
}
.mime_part_finalizer <-
function (x)
{
    if (!is.null(x$file))
        file.remove(x$file)
}
