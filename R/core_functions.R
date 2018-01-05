#library(intrval)
#library(mefa4)
#library(rgdal)
#library(rgeos)
#library(sp)
#library(raster)
#options("cure4insect" = list(
#    baseurl = "http://ftp.public.abmi.ca/species.abmi.ca/reports",
#    version = "2017"))

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
names(.c4if)
load_common_data()
names(.c4if)
clear_common_data()
names(.c4if)

## make a subset
clear_subset <- function()
    rm(list=ls(envir=.c4is), envir=.c4is)
subset_data <-
function(id=NULL, species="all")
{
    clear_subset()

    vals <- c("all","birds","lichens","mammals","mites","mosses","vplants")
    x <- .c4if$SP
    if (length(species) == 1L && species %in% vals) {
        SPPfull <- if (species == "all")
            rownames(x) else rownames(x)[x$taxon==species]
    } else {
        SPPfull <- intersect(species, rownames(x))
    }
    assign("SPsub", x[SPPfull,,drop=FALSE], envir=.c4is)

    if (is.null(id))
        id <- rownames(.c4if$KT)
    id <- sort(intersect(id, rownames(.c4if$KT)))
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
names(.c4i1)
load_species_data("Ovenbird")
names(.c4i1)
clear_species_data()
names(.c4i1)

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
        species=.c4i1$taxon,
        max=MAX,
        mean=MEAN,
        level=level,
        boot=.c4i1$boot,
        intactness=rbind(
            Current=c(Estimate=NC, NC_CI),
            Reference=c(Estimate=NR, NR_CI),
            Intactness=c(Estimate=SI, SI_CI)),
        sector=rbind(
            Area=Sector_Area,
            Total=Sector_Total,
            UnderHF=Sector_UnderHF,
            Unit=Sector_Unit))
}

load_common_data()
SPP <- "Ovenbird"
PIX <- c("182_362", "182_363", "182_364", "182_365", "182_366", "182_367",
    "182_368", "182_369", "182_370", "182_371", "182_372")
subset_data(id=PIX, species=SPP)
load_species_data("Ovenbird")
calculate_results()

load_species_data("Ovenbird", boot=FALSE)
calculate_results()

## todo:
## - function which detrends the results (1-liner)
## - write function that loops over species and makes the output
## - send email

