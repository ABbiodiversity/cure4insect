## store object for full grid and species
.c4if <- new.env(parent=emptyenv())
## store object for subset of the grid and species
.c4is <- new.env(parent=emptyenv())
if(getRversion() >= "2.15.1")
    utils::globalVariables(c(".c4if", ".c4is"))
#.c4if=cure4insect:::.c4if
#.c4is=cure4insect:::.c4is

## this function figues out the right footprint area version
## depending on version and taxon
.get_KA <- function(version, taxon) {
    if (version == "2017") {
        KA <- if (taxon == "birds")
            "KA_2012" else "KA_2014"
    }
    if (version == "2018") {
        KA <- "KA_2016"
    }
    KA
}

## load data set that is common (full grid and species)
## KA_2012, KA_2014: sector areas by 1km unit
## KT: 10km unit mapping to 1km units
## XY: coordinates of 1km units
## SP: species lookup table
## CF: coefficients
## CFbirds: joint coefficients for birds
## QT2KT: quarter section to km grid crosswalk (nearest)
## VER: version information
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
        if (.verbose()) {
            print(get_version_info())
            flush.console()
        }
    }
    invisible(NULL)
}

## make a subset
clear_subset_data <- function()
    rm(list=ls(envir=.c4is), envir=.c4is)
.species_set <- function(species) {
    if (!is.null(dim(species))) # if provided as table, use 1st col
        species <- as.character(species[,1L])
    vals <- c("all","birds","lichens","mammals","mites","mosses","vplants",
        "upland", "lowland", "native", "nonnat", "north", "south")
    x <- .c4if$SP
    if (length(species) == 1L && tolower(species)  %in% vals) {
        if (species %in%  c("all", "birds","lichens","mammals","mites","mosses","vplants"))
            SPPfull <- get_all_species(taxon=species)
        if (species %in% c("upland", "lowland"))
            SPPfull <- get_all_species(habitat=species)
        if (species %in% c("native", "nonnat"))
            SPPfull <- get_all_species(status=species)
        if (species %in% c("north", "south"))
            SPPfull <- get_all_species(mregion=species)
    } else {
        SPPfull <- species
        if (any(SPPfull %ni% rownames(x)))
            stop("all species must be valid IDs")
    }
    SPPfull <- rownames(x)[rownames(x) %in% SPPfull]
    if (length(SPPfull) <= 0)
        stop("no species selected")
    SPPfull
}
subset_common_data <-
function(id=NULL, species="all")
{
    if (!is_loaded())
        stop("common data needed: use load_common_data")

    x <- .c4if$SP
    SPPfull <- .species_set(species)

    if (is.null(id))
        id <- rownames(.c4if$KT)
    if (inherits(id, "SpatialPolygons"))
        id <- overlay_polygon(id)
    ## if provided as table, use 1st col
    if (!is.null(dim(id))) {
        if (.verbose()) {
            cat("1st column of spatial id table used\n")
            flush.console()
        }
        id <- as.character(id[,1L])
    }
    if (!is.character(id))
        id <- as.character(id)
    if (length(id) <= 0)
        stop("no spatial IDs selected")
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
    if (!.validate_id(id, type="km"))
        stop("spatial id not valid")

    id10 <- sort(unique(as.character(.c4if$KT[id, "Row10_Col10"])))
    KT <- .c4if$KT
    ## South: >= 0; North: <= 0
    KT$mregion <- as.integer(-1*.select_id("north") + .select_id("south"))

    ## assignment happens only if all checks passed
    if (.verbose()) {
        cat("arranging subsets\n")
        flush.console()
    }
    clear_subset_data()
    assign("KTsub", KT[id,,drop=FALSE],
        envir=.c4is)
    if (getOption("cure4insect")$version == "2017") {
        assign("A_2012", Matrix::colSums(.c4if$KA_2012[id,,drop=FALSE]),
            envir=.c4is)
        assign("A_2014", Matrix::colSums(.c4if$KA_2014[id,,drop=FALSE]),
            envir=.c4is)
    } else {
        assign("A_2016", Matrix::colSums(.c4if$KA_2016[id,,drop=FALSE]),
            envir=.c4is)
    }
    assign("SPfull", x,
        envir=.c4is)
    assign("SPsub", x[SPPfull,,drop=FALSE],
        envir=.c4is)
    invisible(NULL)
}

## load data for a species
## avoid somehow common data: only spp table needed
load_species_data <-
function(species, boot=NULL, path=NULL, version=NULL)
{
    opts <- getOption("cure4insect")
    if (is.null(path))
        path <- opts$path
    if (is.null(version))
        version <- opts$version
    if (is.null(boot))
        boot <- as.logical(opts$boot)
    taxon <- as.character(.c4if$SP[species, "taxon"])
    model_north <- .c4if$SP[species, "model_north"]
    model_south <- .c4if$SP[species, "model_south"]
    .load_species_data(species=species,
        boot=boot, path=path, version=version, taxon=taxon,
        model_north=model_north, model_south=model_south)
}
.load_species_data <-
function(species, boot=NULL, path=NULL, version=NULL,
taxon, model_north, model_south)
{
    opts <- getOption("cure4insect")
    if (is.null(path))
        path <- opts$path
    if (is.null(version))
        version <- opts$version
    if (is.null(boot))
        boot <- as.logical(opts$boot)
    y <- new.env()
    assign("species", species, envir=y)
    assign("taxon", taxon, envir=y)
    assign("model_north", model_north, envir=y)
    assign("model_south", model_south, envir=y)
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
    q <- as.numeric(getOption("cure4insect")$trunc)
    ## handle non additivity of spclim component (sertor vs total)
    if (getOption("cure4insect")$version != "2017" && y$taxon != "birds") {
        MAX <- max(
            quantile(y$Totals[,"Curr"], q, na.rm=TRUE),
            quantile(y$Totals[,"Ref"], q, na.rm=TRUE))
    } else {
        MAX <- max(
            quantile(rowSums(y$SA.Curr), q, na.rm=TRUE),
            quantile(rowSums(y$SA.Ref), q, na.rm=TRUE))
    }
    KTsub <- .c4is$KTsub
    ## Rockies for non-birds and unmodelled regions excluded
    if (y$taxon != "birds")
        KTsub <- KTsub[KTsub$reg_nr %ni% "Rocky Mountain",,drop=FALSE]
    ## South: >= 0; North: <= 0
    if (!all(c(y$model_north, y$model_south))) {
        KTsub <- if (y$model_north) {
            KTsub[KTsub$mregion <= 0,,drop=FALSE]
        } else {
            KTsub[KTsub$mregion >= 0,,drop=FALSE]
        }
    }
    PIX <- rownames(KTsub)
    PIX <- PIX[PIX %in% rownames(y$SA.Curr)]
    SA.Curr <- y$SA.Curr[PIX,cn,drop=FALSE]
    SA.Ref <- y$SA.Ref[PIX,cn,drop=FALSE]
    ## subset can have 0 rows when outside of modeled range:
    ## this leads to mean(numeric(0))=NaN but should be 0
    ## the variable predicted has the info about this fact
    if (length(PIX) > 0) {
        ## handle non additivity of spclim component (sertor vs total)
        if (getOption("cure4insect")$version != "2017" && y$taxon != "birds") {
            TOT <- y$Totals[PIX,,drop=FALSE]
            cr <- TOT[,"Curr"]
            rf <- TOT[,"Ref"]
        } else {
            cr <- rowSums(SA.Curr)
            rf <- rowSums(SA.Ref)
        }
        MEAN_cr <- mean(cr[cr <= quantile(cr, q)])
        MEAN_rf <- mean(rf[rf <= quantile(rf, q)])
        MEAN <- max(MEAN_cr, MEAN_rf)
        predicted <- TRUE
    } else {
        MEAN <- 0
        predicted <- FALSE
    }
    CS <- colSums(SA.Curr)
    RS <- colSums(SA.Ref)
    NC <- sum(CS)
    NR <- sum(RS)
    SI <- 100 * min(NC, NR) / max(NC, NR)
    SI2 <- if (NC <= NR) SI else 200 - SI
    if (y$boot) {
        KTsubsub <- KTsub[PIX,,drop=FALSE]
        Curr.Boot <- y$Curr.Boot
        Ref.Boot <- y$Ref.Boot
        KTsubsub <- KTsubsub[KTsubsub$Row10_Col10 %in% rownames(Curr.Boot),,drop=FALSE]
        PIX10 <- unique(as.character(KTsubsub$Row10_Col10))
        Curr.Boot <- Curr.Boot[PIX10,,drop=FALSE]
        Ref.Boot <- Ref.Boot[PIX10,,drop=FALSE]
        Curr.Boot <- Curr.Boot[match(KTsubsub$Row10_Col10, rownames(Curr.Boot)),,drop=FALSE]
        Ref.Boot <- Ref.Boot[match(KTsubsub$Row10_Col10, rownames(Ref.Boot)),,drop=FALSE]
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

    ## HF area depends on version and taxa
    if (getOption("cure4insect")$version == "2017") {
        KA <- if (y$taxon == "birds") # area in km^2
            .c4is$A_2012 else .c4is$A_2014
    } else {
        KA <- .c4is$A_2016
    }

    Sector_Area <- (100 * KA / sum(KA))[names(Sector_Total)]
    Sector_Unit <- 100 * Sector_Total / Sector_Area
    out <- list(
        taxon=y$taxon,
        species=y$species,
        max=MAX,
        mean=MEAN,
        level=level,
        predicted=predicted,
        model_north=y$model_north,
        model_south=y$model_south,
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
    q <- as.numeric(getOption("cure4insect")$trunc)
    ## handle non additivity of spclim component (sertor vs total)
    if (getOption("cure4insect")$version != "2017" && y$taxon != "birds") {
        MAX <- max(
            quantile(y$Totals[,"Curr"], q, na.rm=TRUE),
            quantile(y$Totals[,"Ref"], q, na.rm=TRUE))
    } else {
        MAX <- max(
            quantile(rowSums(y$SA.Curr), q, na.rm=TRUE),
            quantile(rowSums(y$SA.Ref), q, na.rm=TRUE))
    }
    KTsub <- .c4is$KTsub
    ## Rockies for non-birds and unmodelled regions excluded
    if (y$taxon != "birds")
        KTsub <- KTsub[KTsub$reg_nr %ni% "Rocky Mountain",,drop=FALSE]
    ## South: >= 0; North: <= 0
    if (!all(c(y$model_north, y$model_south))) {
        KTsub <- if (y$model_north) {
            KTsub[KTsub$mregion <= 0,,drop=FALSE]
        } else {
            KTsub[KTsub$mregion >= 0,,drop=FALSE]
        }
    }
    PIX <- rownames(KTsub)
    ## Rockies and unmodelled regions should be excluded
    PIX <- PIX[PIX %in% rownames(y$SA.Curr)]
    SA.Curr <- y$SA.Curr[PIX,cn]
    SA.Ref <- y$SA.Ref[PIX,cn]
    ## subset can have 0 rows when outside of modeled range:
    ## this leads to mean(numeric(0))=NaN but should be 0
    if (length(PIX) > 0) {
        ## handle non additivity of spclim component (sertor vs total)
        if (getOption("cure4insect")$version != "2017" && y$taxon != "birds") {
            TOT <- y$Totals[PIX,,drop=FALSE]
            cr <- TOT[,"Curr"]
            rf <- TOT[,"Ref"]
        } else {
            cr <- rowSums(SA.Curr)
            rf <- rowSums(SA.Ref)
        }
        MEAN_cr <- mean(cr[cr <= quantile(cr, q)])
        MEAN_rf <- mean(rf[rf <= quantile(rf, q)])
        MEAN <- max(MEAN_cr, MEAN_rf)
        predicted <- TRUE
    } else {
        MEAN <- 0
        predicted <- FALSE
    }
    list(
        max=MAX,
        mean=MEAN,
        limit=limit,
        predicted=predicted,
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
function(taxon="all", mregion="both", habitat, status)
{
    if (!is_loaded())
        stop("common data needed: use load_common_data")
    taxon <- match.arg(taxon, c("all","birds","lichens","mammals",
        "mites","mosses","vplants"), several.ok=TRUE)
    out <- .c4if$SP
    keep <- if (length(taxon) == 1 && taxon == "all")
        rep(TRUE, nrow(out)) else out$taxon %in% taxon
    mregion <- match.arg(mregion, c("both", "north", "south"))
    if (mregion != "both") {
        keep <- if (mregion == "north")
            keep & out$model_north else keep & out$model_south
    }
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

.select_id <- function(mregion="both", nr=NULL, nsr=NULL, luf=NULL) {
    if (!is_loaded())
        stop("common data needed: use load_common_data")
    mregion <- match.arg(mregion, c("both", "north", "south"))
    keep <- rep(TRUE, nrow(.c4if$KT))
    if (!is.null(nr)) {
        nr <- match.arg(nr, levels(.c4if$KT$reg_nr), several.ok=TRUE)
        keep[.c4if$KT$reg_nr %ni% nr] <- FALSE
    }
    if (!is.null(nsr)) {
        nsr <- match.arg(nsr, levels(.c4if$KT$reg_nsr), several.ok=TRUE)
        keep[.c4if$KT$reg_nsr %ni% nsr] <- FALSE
    }
    if (!is.null(luf)) {
        luf <- match.arg(luf, levels(.c4if$KT$reg_luf), several.ok=TRUE)
        keep[.c4if$KT$reg_luf %ni% luf] <- FALSE
    }
    ## mregion to override selection outside of model region
    if (mregion != "both") {
        if (mregion == "north") {
            ss <- .c4if$KT$reg_nr != "Grassland"
        } else {
            ss <- .c4if$KT$reg_nr %in% c("Grassland", "Parkland") |
                .c4if$KT$reg_nsr == "Dry Mixedwood"
            ss[.c4if$KT$reg_nsr == "Dry Mixedwood" &
                coordinates(.c4if$XY)[,2] > 56.7] <- FALSE
        }
        keep <- keep & ss
    }
    keep
}

get_id_table <- function(mregion="both", nr=NULL, nsr=NULL, luf=NULL) {
    .c4if$KT[.select_id(mregion, nr, nsr, luf),,drop=FALSE]
}

get_all_id <- function(mregion="both", nr=NULL, nsr=NULL, luf=NULL)
    rownames(.c4if$KT)[.select_id(mregion, nr, nsr, luf)]

get_all_species <- function(taxon="all", mregion="both", habitat, status)
    rownames(get_species_table(taxon, mregion, habitat, status))

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

get_subset_id <- function()
    rownames(.c4is$KTsub)

get_subset_species <- function()
    rownames(.c4is$SPsub)

## turn bird density into P(Y>0)
p_bird <- function(D, area=c("ha", "km"), pair_adj=2) {
    A <- switch(match.arg(area),
        "km" = 100,
        "ha" = 1)
    1 - exp(-D * A * pair_adj)
}

## make a local copy of files
## need common data 1st
dowload_data <- function(dir, species="all", version=NULL, ...) {
    ## note: this is hard coded!
    path <- "http://sc-dev.abmi.ca/reports"
    if (!is_loaded())
        stop("common data needed: use load_common_data")
    if (is.null(version))
        version <- opts$version
    spplist <- .species_set(species)
    SP <- .c4if$SP[spplist,,drop=FALSE]
    if (!dir.exists(file.path(dir)))
        stop(sprintf("%s directory does not exist", dir))
    if (!dir.exists(file.path(dir, version)))
        dir.create(file.path(dir, version))
    if (!dir.exists(file.path(dir, version, "data")))
        dir.create(file.path(dir, version, "data"))
    if (!dir.exists(file.path(dir, version, "results")))
        dir.create(file.path(dir, version, "results"))
    .get <- function(din, dou, fn, ...) {
        if (!file.exists(file.path(dou, fn)))
            download.file(
                url=file.path(din, fn),
                destfile=file.path(dou, fn), ...)
        invisible(TRUE)
    }
    .getspp <- function(spp, ...) {
        taxon <- as.character(SP[spp, "taxon"])
        if (!dir.exists(file.path(dir, version, "results", taxon)))
            dir.create(file.path(dir, version, "results", taxon))
        for (i in c("boot", "sector", "spclim")) {
        if (!dir.exists(file.path(dir, version, "results", taxon, i)))
            dir.create(file.path(dir, version, "results", taxon, i))
            .get(file.path(path, version, "results", taxon, i),
                file.path(dir, version, "results", taxon, i),
                paste0(spp, ".RData"), ...)
        }
        invisible(TRUE)
    }
    ## common data
    out <- .get(file.path(path, version, "data"),
        file.path(dir, version, "data"),
        "kgrid_areas_by_sector.RData", ...)
    for (spp in spplist) {
        .getspp(spp, ...)
    }
    invisible(NULL)
}
