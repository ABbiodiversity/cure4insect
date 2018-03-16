## store object for full grid and species
.c4if <- new.env(parent=emptyenv())
## store object for subset of the grid and species
.c4is <- new.env(parent=emptyenv())
if(getRversion() >= "2.15.1")
    utils::globalVariables(c(".c4if", ".c4is"))
#.c4if=cure4insect:::.c4if
#.c4is=cure4insect:::.c4is

## load data set that is common (full grid and species)
## KA_2012, KA_2014: sector areas by 1km unit
## KT: 10km unit mapping to 1km units
## XY: coordinates of 1km units
## SP: species lookup table
clear_common_data <- function()
    rm(list=ls(envir=.c4if), envir=.c4if)
load_common_data <-
function(path=NULL, version=NULL)
{
    if (is_loaded()) {
        if (.verbose()) {
            cat("common data already loaded\n")
            flush.console()
        }
    } else {
        if (.verbose()) {
            cat("loading common data\n")
            flush.console()
        }
        opts <- getOption("cure4insect")
        if (is.null(path))
            path <- opts$path
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
    }
    invisible(NULL)
}

## make a subset
clear_subset_data <- function()
    rm(list=ls(envir=.c4is), envir=.c4is)
subset_common_data <-
function(id=NULL, species="all")
{
    if (!is_loaded())
        stop("common data needed: use load_common_data")
    if (.verbose()) {
        cat("arranging subsets\n")
        flush.console()
    }
    clear_subset_data()

    if (!is.null(dim(species))) # if provided as table, use 1st col
        species <- as.character(species[,1L])
    vals <- c("all","birds","lichens","mammals","mites","mosses","vplants",
        "upland", "lowland", "native", "nonnat")
    x <- .c4if$SP
    if (length(species) == 1L && tolower(species)  %in% vals) {
        if (species %in%  c("all", "birds","lichens","mammals","mites","mosses","vplants"))
            SPPfull <- get_all_species(taxon=species)
        if (species %in% c("upland", "lowland"))
            SPPfull <- get_all_species(habitat=species)
        if (species %in% c("native", "nonnat"))
            SPPfull <- get_all_species(status=species)
    } else {
        SPPfull <- species
        any(SPPfull %ni% rownames(x))
            stop("all species must ba valid IDs")
    }
    SPPfull <- rownames(x)[rownames(x) %in% SPPfull]
    if (length(SPPfull) <= 0)
        stop("no species selected")
    assign("SPfull", x,
        envir=.c4is)
    assign("SPsub", x[SPPfull,,drop=FALSE],
        envir=.c4is)

    if (is.null(id))
        id <- rownames(.c4if$KT)
    if (inherits(id, "SpatialPolygons"))
        id <- overlay_polygon(id)
    if (!is.null(dim(id))) # if provided as table, use 1st col
        id <- as.character(id[,1L])
    if (!is.character(id))
        id <- as.character(id)
    ## QS-to-km mapping
    if (.validate_id(id, type="qs")) {
         if (.verbose()) {
            cat("matching quarter sections\n")
            flush.console()
        }
        id <- qs2km(id[id %in% get_all_qsid()])
    }
    ## validating Row_Col IDs
    id <- id[id %in% rownames(.c4if$KT)]
    id <- sort(id)
    if (length(id) <= 0)
        stop("no spatial IDs selected")
    if (!.validate_id(id, type="km"))
        stop("spatial id not valid")
    id10 <- sort(unique(as.character(.c4if$KT[id, "Row10_Col10"])))
    assign("KTsub", .c4if$KT[id,,drop=FALSE],
        envir=.c4is)
    assign("A_2012", Matrix::colSums(.c4if$KA_2012[id,,drop=FALSE]),
        envir=.c4is)
    assign("A_2014", Matrix::colSums(.c4if$KA_2014[id,,drop=FALSE]),
        envir=.c4is)

    invisible(NULL)
}

## load data for a species
## avoid somehow common data: only spp table needed
load_species_data <-
function(species, boot=TRUE, path=NULL, version=NULL)
{
    opts <- getOption("cure4insect")
    if (is.null(path))
        path <- opts$path
    if (is.null(version))
        version <- opts$version
    taxon <- as.character(.c4if$SP[species, "taxon"])
    if (taxon != "birds" && boot)
        warning("bootstrap based estimates are being checked: treat CI results with caution")
    .load_species_data(species=species,
        boot=boot, path=path, version=version, taxon=taxon)
}
.load_species_data <-
function(species, boot=TRUE, path=NULL, version=NULL, taxon)
{
    opts <- getOption("cure4insect")
    if (is.null(path))
        path <- opts$path
    if (is.null(version))
        version <- opts$version
    y <- new.env()
    assign("species", species, envir=y)
    assign("taxon", taxon, envir=y)
    assign("boot", boot, envir=y)
    fn1 <- file.path(path, version, "results", taxon, "sector", paste0(species, ".RData"))
    fn2 <- file.path(path, version, "results", taxon, "boot", paste0(species, ".RData"))
    if (!startsWith(path, "http://")) {
        load(fn1, envir=y)
        if (boot)
            load(fn2, envir=y)
    } else {
        con <- url(fn1)
        load(con, envir=y)
        close(con)
        if (boot) {
            con <- url(fn2)
            load(con, envir=y)
            close(con)
        }
    }
    class(y) <- "c4i1"
    y
}

calculate_results <-
function(y, level=0.9)
{
    if (length(names(.c4is)) < 1)
        stop("spatial subsets needed: use subset_common_data")
    if (!inherits(y, "c4i1"))
        stop("y must be of class c4i1")
    .calculate_results(y=y, level=level, .c4is=as.list(.c4is))
}
.calculate_results <-
function(y, level=0.9, .c4is)
{
    cn <- c("Native", "Misc", "Agriculture", "Forestry", "RuralUrban", "Energy", "Transportation")
    a <- c(0.5*(1-level), 1-0.5*(1-level))
    MAX <- max(quantile(rowSums(y$SA.Curr), 0.99), quantile(rowSums(y$SA.Ref), 0.99))
    PIX <- rownames(.c4is$KTsub)
    ## Rockies and unmodelled regions should be excluded
    PIX <- PIX[PIX %in% rownames(y$SA.Curr)]
    SA.Curr <- y$SA.Curr[PIX,cn]
    SA.Ref <- y$SA.Ref[PIX,cn]
    ## subset can have 0 rows when outside of modeled range:
    ## this leads to mean(numeric(0))=NaN but should be 0
    if (length(PIX) > 0) {
        cr <- rowSums(SA.Curr)
        rf <- rowSums(SA.Ref)
        MEAN_cr <- mean(cr[cr <= quantile(cr, 0.99)])
        MEAN_rf <- mean(rf[rf <= quantile(rf, 0.99)])
        MEAN <- max(MEAN_cr, MEAN_rf)
    } else {
        MEAN <- 0
    }
    CS <- colSums(SA.Curr)
    RS <- colSums(SA.Ref)
    NC <- sum(CS)
    NR <- sum(RS)
    SI <- 100 * min(NC, NR) / max(NC, NR)
    SI2 <- if (NC <= NR) SI else 200 - SI
    if (y$boot) {
        KTsubsub <- .c4is$KTsub[PIX,,drop=FALSE]
        Curr.Boot <- y$Curr.Boot
        Ref.Boot <- y$Ref.Boot
        KTsubsub <- KTsubsub[KTsubsub$Row10_Col10 %in% rownames(Curr.Boot),,drop=FALSE]
        PIX10 <- unique(as.character(KTsubsub$Row10_Col10))
        Curr.Boot <- Curr.Boot[PIX10,,drop=FALSE]
        Ref.Boot <- Ref.Boot[PIX10,,drop=FALSE]
        Curr.Boot <- Curr.Boot[match(KTsubsub$Row10_Col10, rownames(Curr.Boot)),]
        Ref.Boot <- Ref.Boot[match(KTsubsub$Row10_Col10, rownames(Ref.Boot)),]
        CB <- colSums(Curr.Boot)
        RB <- colSums(Ref.Boot)
        SIB <- 100 * pmin(CB, RB) / pmax(CB, RB)
        SI2B <- ifelse(CB <= RB, SIB, 200 - SIB)
        NC_CI <- quantile(CB, a)
        NR_CI <- quantile(RB, a)
        SI_CI <- quantile(SIB, a, na.rm=TRUE) # division by 0 can occur
        SI2_CI <- quantile(SI2B, a, na.rm=TRUE) # division by 0 can occur
    } else {
        CB <- RB <- rep(NA, 100)
        NC_CI <- c(NA, NA)
        names(NC_CI) <- paste0(100*a, "%")
        NR_CI <- SI_CI <- SI2_CI <- NC_CI
    }
    Sector_Total <- (100 * (CS - RS) / NR)[-1]
    Sector_UnderHF <- (100 * (CS - RS) / RS)[-1]
    KA <- if (y$taxon == "birds") # area in km^2
        .c4is$A_2012 else .c4is$A_2014
    Sector_Area <- (100 * KA / sum(KA))[names(Sector_Total)]
    Sector_Unit <- 100 * Sector_Total / Sector_Area
    out <- list(
        taxon=y$taxon,
        species=y$species,
        max=MAX,
        mean=MEAN,
        level=level,
        boot=y$boot,
        boot_current=CB,
        boot_reference=RB,
        intactness=rbind(
            Current=c(Estimate=NC, NC_CI),
            Reference=c(Estimate=NR, NR_CI),
            Intactness=c(Estimate=SI, SI_CI),
            Intactness2=c(Estimate=SI2, SI2_CI)),
        sector=rbind(
            Current=CS[-1],
            Reference=RS[-1],
            Area=Sector_Area,
            #Area=KA,
            Total=Sector_Total,
            UnderHF=Sector_UnderHF,
            Unit=Sector_Unit))
    class(out) <- "c4iraw"
    out
}
.calculate_limit <-
function(y, limit=NULL)
{
    if (is.null(limit))
        limit <- as.numeric(getOption("cure4insect")$limit)
    if (limit %)(% c(0,1))
        stop("limit value must be between in [0, 1]")
    cn <- c("Native", "Misc", "Agriculture", "Forestry", "RuralUrban", "Energy", "Transportation")
    MAX <- max(quantile(rowSums(y$SA.Curr), 0.99), quantile(rowSums(y$SA.Ref), 0.99))
    PIX <- rownames(.c4is$KTsub)
    ## Rockies and unmodelled regions should be excluded
    PIX <- PIX[PIX %in% rownames(y$SA.Curr)]
    SA.Curr <- y$SA.Curr[PIX,cn]
    SA.Ref <- y$SA.Ref[PIX,cn]
    ## subset can have 0 rows when outside of modeled range:
    ## this leads to mean(numeric(0))=NaN but should be 0
    if (length(PIX) > 0) {
        cr <- rowSums(SA.Curr)
        rf <- rowSums(SA.Ref)
        MEAN_cr <- mean(cr[cr <= quantile(cr, 0.99)])
        MEAN_rf <- mean(rf[rf <= quantile(rf, 0.99)])
        MEAN <- max(MEAN_cr, MEAN_rf)
    } else {
        MEAN <- 0
    }
    list(
        max=MAX,
        mean=MEAN,
        limit=limit,
        keep=MEAN >= MAX * limit)
}

set_options <-
function(...)
{
    opar <- getOption("cure4insect")
    args <- list(...)
    if (length(args)) {
        if (length(args) == 1 && is.list(args[[1]])) {
            npar <- args[[1]]
        }
        else {
            npar <- opar
            npar[match(names(args), names(npar))] <- args
        }
        options(cure4insect = npar)
    }
    invisible(opar)
}

is_loaded <- function()
    length(names(.c4if)) > 0

get_id_locations <- function() {
    if (!is_loaded())
        stop("common data needed: use load_common_data")
    .c4if$XY
}

get_species_table <-
function(taxon="all", habitat, status)
{
    if (!is_loaded())
        stop("common data needed: use load_common_data")
    taxon <- match.arg(taxon, c("all","birds","lichens","mammals",
        "mites","mosses","vplants"), several.ok=TRUE)
    out <- .c4if$SP
    keep <- if (taxon == "all")
        rep(TRUE, nrow(out)) else out$taxon %in% taxon
    cat(table(out$habitat_assoc))
    if (!missing(habitat)) {
        habitat <- match.arg(habitat, c("upland", "lowland"))
        keep <- if (habitat == "upland") {
            keep & out$habitat_assoc == "Upland"
        } else {
            keep & out$habitat_assoc == "Lowland"
        }
    }
    if (!missing(status)) {
        status <- match.arg(status, c("native", "nonnative"))
        keep <- if (status == "native")
            keep & out$native else keep & !out$native
    }
    out[keep,,drop=FALSE]
}

get_all_id <- function()
    rownames(coordinates(get_id_locations()))

get_all_species <- function(taxon="all", habitat, status)
    rownames(get_species_table(taxon, habitat, status))

.verbose <- function() {
    x <- getOption("cure4insect")$verbose
    !is.null(x) && x > 0
}

get_version_info <- function() {
    if (!is_loaded())
        stop("common data needed: use load_common_data")
    .c4if$VER
}

get_all_qsid <- function() {
    if (!is_loaded())
        stop("common data needed: use load_common_data")
    names(.c4if$QT2KT)
}
qs2km <- function(qsid) {
    if (!is_loaded())
        stop("common data needed: use load_common_data")
    unique(as.character(.c4if$QT2KT[qsid]))
}

.validate_id <- function(id, type=c("km", "qs")) {
    chr <- switch(match.arg(type),
        "km"="_",
        "qs"="-")
    all(grepl(chr, id))
}

get_subset_info <- function() {
    c(species=nrow(.c4is$SPsub), pixels=nrow(.c4is$KTsub))
}
