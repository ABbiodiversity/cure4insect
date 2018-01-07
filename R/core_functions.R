library(intrval)
#library(mefa4)
#library(rgdal)
#library(rgeos)
#library(sp)
#library(raster)
library(Matrix)
library(sendmailR)
options("cure4insect" = list(
    baseurl = "http://ftp.public.abmi.ca/species.abmi.ca/reports",
    version = "2017",
    sender = sprintf("<x@\\%s>", Sys.info()[4])))

## store object for full grid and species
.c4if <- new.env(parent=emptyenv())
## store object for subset of the grid and species
.c4is <- new.env(parent=emptyenv())
.c4i1 <- new.env(parent=emptyenv())
if(getRversion() >= "2.15.1")
    utils::globalVariables(c(".c4if", ".c4is", ".c4i1"))

## load data set that is common (full grid and species)
## KA_2012, KA_2014: sector areas by 1km unit
## KT: 10km unit mapping to 1km units
## XY: coordinates of 1km units
## SP: species lookup table
load_common_data <-
function(path=NULL, version=NULL)
{
    clear_common_data()
    opts <- getOption("cure4insect")
    if (is.null(path))
        path <- opts$baseurl
    if (is.null(version))
        version <- opts$version
    fn <- file.path(path, version, "data", "kgrid_areas_by_sector.RData")
    if (!startsWith(path, "http://")) {
        load(fn, envir=.c4if)
    } else {
        con <- url(fn)
        load(con, envir=.c4if)
        close(con)
    }
    invisible(NULL)
}
clear_common_data <- function()
    rm(list=ls(envir=.c4if), envir=.c4if)
#names(.c4if)
#load_common_data()
#names(.c4if)
#clear_common_data()
#names(.c4if)

## make a subset
clear_subset_data <- function()
    rm(list=ls(envir=.c4is), envir=.c4is)
subset_common_data <-
function(id=NULL, species="all")
{
    clear_subset_data()
    #requireNamespace("Matrix")

    vals <- c("all","birds","lichens","mammals","mites","mosses","vplants")
    x <- .c4if$SP
    if (length(species) == 1L && species %in% vals) {
        SPPfull <- if (species == "all")
            rownames(x) else rownames(x)[x$taxon==species]
    } else {
        SPPfull <- species
    }
    assign("SPsub", x[rownames(x) %in% SPPfull,,drop=FALSE], envir=.c4is)

    if (is.null(id))
        id <- rownames(.c4if$KT)
    id <- id[id %in% rownames(.c4if$KT)]
    id <- sort(id)
    id10 <- sort(unique(as.character(.c4if$KT[id, "Row10_Col10"])))
    assign("KTsub", .c4if$KT[id,,drop=FALSE], envir=.c4is)
    assign("A_2012", colSums(.c4if$KA_2012[id,,drop=FALSE]), envir=.c4is)
    assign("A_2014", colSums(.c4if$KA_2014[id,,drop=FALSE]), envir=.c4is)

    invisible(NULL)
}

## load data for a species
clear_species_data <- function()
    rm(list=ls(envir=.c4i1), envir=.c4i1)
load_species_data <-
function(species, boot=TRUE, path=NULL, version=NULL)
{
    clear_species_data()
    opts <- getOption("cure4insect")
    if (is.null(path))
        path <- opts$baseurl
    if (is.null(version))
        version <- opts$version
    taxon <- as.character(.c4if$SP[species, "taxon"])
    assign("species", species, envir=.c4i1)
    assign("taxon", taxon, envir=.c4i1)
    assign("boot", boot, envir=.c4i1)
    fn1 <- file.path(path, version, "results", taxon, "sector", paste0(species, ".RData"))
    fn2 <- file.path(path, version, "results", taxon, "boot", paste0(species, ".RData"))
    if (!startsWith(path, "http://")) {
        load(fn1, envir=.c4i1)
        if (boot)
            load(fn2, envir=.c4i1)
    } else {
        con <- url(fn1)
        load(con, envir=.c4i1)
        close(con)
        if (boot) {
            con <- url(fn2)
            load(con, envir=.c4i1)
            close(con)
        }
    }
    invisible(NULL)
}
#names(.c4i1)
#load_species_data("Ovenbird")
#names(.c4i1)
#clear_species_data()
#names(.c4i1)

calculate_results <-
function(level=0.9)
{
    PIX <- rownames(.c4is$KTsub)
    PIX10 <- unique(as.character(.c4is$KTsub$Row10_Col10))
    cn <- c("Native", "Misc", "Agriculture", "Forestry", "RuralUrban", "Energy", "Transportation")
    a <- c(0.5*(1-level), 1-0.5*(1-level))
    MAX <- max(max(rowSums(.c4i1$SA.Curr)), max(rowSums(.c4i1$SA.Ref)))
    SA.Curr <- .c4i1$SA.Curr[PIX,cn]
    SA.Ref <- .c4i1$SA.Ref[PIX,cn]
    MEAN <- max(mean(rowSums(SA.Curr)), mean(rowSums(SA.Ref)))
    CS <- colSums(SA.Curr)
    RS <- colSums(SA.Ref)
    NC <- sum(CS)
    NR <- sum(RS)
    SI <- 100 * min(NC, NR) / max(NC, NR)
    if (.c4i1$boot) {
        Curr.Boot <- .c4i1$Curr.Boot[PIX10,]
        Ref.Boot <- .c4i1$Ref.Boot[PIX10,]
        Curr.Boot <- Curr.Boot[match(.c4is$KTsub$Row10_Col10, rownames(Curr.Boot)),]
        Ref.Boot <- Ref.Boot[match(.c4is$KTsub$Row10_Col10, rownames(Ref.Boot)),]
        CB <- colSums(Curr.Boot)
        RB <- colSums(Ref.Boot)
        NC_CI <- quantile(CB, a)
        NR_CI <- quantile(RB, a)
        SI_CI <- quantile(100 * pmin(CB, RB) / pmax(CB, RB), a)
    } else {
        NC_CI <- c(NA, NA)
        names(NC_CI) <- paste0(100*a, "%")
        NR_CI <- SI_CI <- NC_CI
    }
    Sector_Total <- (100 * (CS - RS) / NR)[-1]
    Sector_UnderHF <- (100 * (CS - RS) / RS)[-1]
    KA <- if (.c4i1$taxon == "birds") .c4is$A_2012 else .c4is$A_2014
    Sector_Area <- (100 * KA / sum(KA))[names(Sector_Total)]
    Sector_Unit <- 100 * Sector_Total / Sector_Area
    list(
        taxon=.c4i1$taxon,
        species=.c4i1$species,
        max=MAX,
        mean=MEAN,
        level=level,
        boot=.c4i1$boot,
        intactness=rbind(
            Current=c(Estimate=NC, NC_CI),
            Reference=c(Estimate=NR, NR_CI),
            Intactness=c(Estimate=SI, SI_CI)),
        sector=rbind(
            Current=CS[-1],
            Reference=RS[-1],
            Area=Sector_Area,
            Total=Sector_Total,
            UnderHF=Sector_UnderHF,
            Unit=Sector_Unit))
}

flatten_results <-
function(x)
{
    Cm <- list()
    df <- data.frame(SpeciesID=x$species, Taxon=x$taxon)
    rownames(df) <- x$species
    df$CI_Level <- x$level
    KEEP <- x$mean > x$max * 0.01
    if (!x$boot)
        Cm[[length(Cm)+1]] <- "Confidence intervals were not requested."
    if (KEEP) {
        df$Abund_Curr_Est <- x$intactness["Current", 1]
        df$Abund_Curr_LCL <- x$intactness["Current", 2]
        df$Abund_Curr_UCL <- x$intactness["Current", 3]
        if (x$boot && x$intactness["Current",1] %)(% x$intactness["Current",2:3])
            Cm[[length(Cm)+1]] <- "Current abundance estimate is outside of CI: region probably too small."
        df$Abund_Ref_Est <- x$intactness["Reference", 1]
        df$Abund_Ref_LCL <- x$intactness["Reference", 2]
        df$Abund_Ref_UCL <- x$intactness["Reference", 3]
        if (x$boot && x$intactness["Reference",1] %)(% x$intactness["Reference",2:3])
            Cm[[length(Cm)+1]] <- "Reference abundance estimate is outside of CI: region probably too small."
        df$SI_Est <- x$intactness["Intactness", 1]
        df$SI_LCL <- x$intactness["Intactness", 2]
        df$SI_UCL <- x$intactness["Intactness", 3]
        if (x$boot && x$intactness["Intactness",1] %)(% x$intactness["Intactness",2:3])
            Cm[[length(Cm)+1]] <- "Intactness estimate is outside of CI."
    } else {
        df$Abund_Curr_Est <- NA
        df$Abund_Curr_LCL <- NA
        df$Abund_Curr_UCL <- NA
        df$Abund_Ref_Est <- NA
        df$Abund_Ref_LCL <- NA
        df$Abund_Ref_UCL <- NA
        df$SI_Est <- NA
        df$SI_LCL <- NA
        df$SI_UCL <- NA
        Cm[[length(Cm)+1]] <- "Abundance did not reach the 1% threshold in the region."
    }
    z <- x$sector
    z[is.na(z)] <- 0
    fd <- matrix(t(z), 1)
    colnames(fd) <- paste0(rep(rownames(z), each=ncol(z)), "_", colnames(z))
    df <- cbind(df, fd)
    df$Comments <- paste(unlist(Cm), collapse=" ")
    df
}

custom_report <-
function(address=NULL, level=0.9)
{
    SPP <- rownames(.c4is$SPsub)
    OUT <- list()
    for (i in seq_along(SPP)) {
        if (interactive()) {
            cat(SPP[i], i, "/", length(SPP), "\n")
            flush.console()
        }
        load_species_data(SPP[i])
        OUT[[i]] <- calculate_results(level=level)
    }
    rval <- do.call(rbind, lapply(OUT, flatten_results))
    if (!is.null(address)) {
        from <- getOption("cure4insect")$sender
        subject <- "Custom Report"
        ## change iris to the zip file
        body <- list("Hi,\n\nYour custom report results are attached.\n\nWith regards,\n\nthe ABMI Science",
            mime_part(rval))
        try(sendmail(from, sprintf("<%s>", address), subject, body,
                 control=list(smtpServer="ASPMX.L.GOOGLE.COM")))
    }
    rval
}

load_common_data()
SPP <- "Ovenbird"
PIX <- c("182_362", "182_363", "182_364", "182_365", "182_366", "182_367",
    "182_368", "182_369", "182_370", "182_371", "182_372")
subset_common_data(id=PIX, species=SPP)
load_species_data("Ovenbird")
calculate_results()

load_species_data("Ovenbird", boot=FALSE)
calculate_results()

SPP <- c("AlderFlycatcher", "Achillea.millefolium")
subset_common_data(id=PIX, species=SPP)

load_species_data("Achillea.millefolium", boot=FALSE)
x <- calculate_results()
flatten_results(x)

## todo:
## OK - function which detrends the results (1-liner)
## OK - write function that loops over species and makes the output
## - send email

sprintf("<%s>", address)

library(sendmailR)
from <- sprintf("<ABMI_Science@\\%s>", Sys.info()[4])
#from <- "<solymos@ualberta.ca>"
to <- "<psolymos@gmail.com>"
subject <- "Your intactness results are ready"
## change iris to the zip file
body <- list("Hi,\n\nYour custom report results are attached.\n\nWith regards,\n\nthe ABMI Science", mime_part(iris))
sendmail(from, to, subject, body,
         control=list(smtpServer="ASPMX.L.GOOGLE.COM"))
