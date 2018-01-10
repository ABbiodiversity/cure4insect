## store object for full grid and species
.c4if <- new.env(parent=emptyenv())
## store object for subset of the grid and species
.c4is <- new.env(parent=emptyenv())
.c4i1 <- new.env(parent=emptyenv())
if(getRversion() >= "2.15.1")
    utils::globalVariables(c(".c4if", ".c4is", ".c4i1"))
#.c4i1=cure4insect:::.c4i1
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
        clear_common_data()
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
    if (.verbose()) {
        cat("arranging subsets\n")
        flush.console()
    }
    clear_subset_data()

    if (!is.null(dim(species))) # if provided as table, use 1st col
        species <- as.character(species[,1L])
    vals <- c("all","birds","lichens","mammals","mites","mosses","vplants")
    x <- .c4if$SP
    if (length(species) == 1L && species %in% vals) {
        SPPfull <- if (species == "all")
            rownames(x) else rownames(x)[x$taxon==species]
    } else {
        SPPfull <- species
    }
    SPPfull <- rownames(x)[rownames(x) %in% SPPfull]
    if (length(SPPfull) <= 0)
        stop("no species selected")
    assign("SPsub", x[SPPfull,,drop=FALSE],
        envir=.c4is)

    if (is.null(id))
        id <- rownames(.c4if$KT)
    if (inherits(id, "SpatialPolygons"))
        id <- overlay_polygon(id)
    if (!is.null(dim(id))) # if provided as table, use 1st col
        id <- as.character(id[,1L])
    id <- id[id %in% rownames(.c4if$KT)]
    id <- sort(id)
    if (length(id) <= 0)
        stop("no spatial IDs selected")
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
clear_species_data <- function()
    rm(list=ls(envir=.c4i1), envir=.c4i1)
load_species_data <-
function(species, boot=TRUE, path=NULL, version=NULL)
{
    clear_species_data()
    opts <- getOption("cure4insect")
    if (is.null(path))
        path <- opts$path
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

calculate_results <-
function(level=0.9)
{
    cn <- c("Native", "Misc", "Agriculture", "Forestry", "RuralUrban", "Energy", "Transportation")
    a <- c(0.5*(1-level), 1-0.5*(1-level))
    MAX <- max(max(rowSums(.c4i1$SA.Curr)), max(rowSums(.c4i1$SA.Ref)))
    PIX <- rownames(.c4is$KTsub)
    ## Rockies and unmodelled regions should be excluded
    PIX <- PIX[PIX %in% rownames(.c4i1$SA.Curr)]
    SA.Curr <- .c4i1$SA.Curr[PIX,cn]
    SA.Ref <- .c4i1$SA.Ref[PIX,cn]
    MEAN <- max(mean(rowSums(SA.Curr)), mean(rowSums(SA.Ref)))
    CS <- colSums(SA.Curr)
    RS <- colSums(SA.Ref)
    NC <- sum(CS)
    NR <- sum(RS)
    SI <- 100 * min(NC, NR) / max(NC, NR)
    SI2 <- if (NC <= NR) SI else 200 - SI
    if (.c4i1$boot) {
        #PIX10 <- unique(as.character(.c4is$KTsub$Row10_Col10))
        KTsubsub <- .c4is$KTsub[PIX,,drop=FALSE]
        Curr.Boot <- .c4i1$Curr.Boot
        Ref.Boot <- .c4i1$Ref.Boot
        KTsubsub <- KTsubsub[KTsubsub$Row10_Col10 %in% rownames(Curr.Boot),,drop=FALSE]
        PIX10 <- unique(as.character(KTsubsub$Row10_Col10))
        #compare_sets(PIX10,rownames(Curr.Boot))
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
        SI_CI <- quantile(SIB, a)
        SI2_CI <- quantile(SI2B, a)
    } else {
        CB <- RB <- rep(NA, 100)
        NC_CI <- c(NA, NA)
        names(NC_CI) <- paste0(100*a, "%")
        NR_CI <- SI_CI <- SI2_CI <- NC_CI
    }
    Sector_Total <- (100 * (CS - RS) / NR)[-1]
    Sector_UnderHF <- (100 * (CS - RS) / RS)[-1]
    KA <- if (.c4i1$taxon == "birds") .c4is$A_2012 else .c4is$A_2014
    Sector_Area <- (100 * KA / sum(KA))[names(Sector_Total)]
    Sector_Unit <- 100 * Sector_Total / Sector_Area
    out <- list(
        taxon=.c4i1$taxon,
        species=.c4i1$species,
        max=MAX,
        mean=MEAN,
        level=level,
        boot=.c4i1$boot,
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
            Total=Sector_Total,
            UnderHF=Sector_UnderHF,
            Unit=Sector_Unit))
    class(out) <- "c4iraw"
    out
}

flatten_results <-
function(x, raw_boot=FALSE, limit=0.01)
{
    if (limit %)(% c(0,1))
        stop("limit value must be between in [0, 1]")
    Cm <- list()
    df <- data.frame(SpeciesID=x$species, Taxon=x$taxon)
    rownames(df) <- x$species
    df$CI_Level <- x$level
    KEEP <- x$mean > x$max * limit
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
        df$SI2_Est <- x$intactness["Intactness2", 1]
        df$SI2_LCL <- x$intactness["Intactness2", 2]
        df$SI2_UCL <- x$intactness["Intactness2", 3]
        if (x$boot && x$intactness["Intactness2",1] %)(% x$intactness["Intactness2",2:3])
            Cm[[length(Cm)+1]] <- "Two-sided intactness estimate is outside of CI."
    } else {
        df$Abund_Curr_Est <- NA
        df$Abund_Curr_LCL <- NA
        df$Abund_Curr_UCL <- NA
        df$Abund_Ref_Est <- NA
        df$Abund_Ref_LCL <- NA
        df$Abund_Ref_UCL <- NA
        df$SI_Est <- NA
        df$SI2_Est <- NA
        df$SI2_LCL <- NA
        df$SI2_UCL <- NA
        Cm[[length(Cm)+1]] <- paste0("Abundance did not reach the ",
            round(100*limit,2), "% threshold in the region.")
    }
    z <- x$sector
    z[is.na(z)] <- 0
    fd <- matrix(t(z), 1)
    colnames(fd) <- paste0(rep(rownames(z), each=ncol(z)), "_", colnames(z))
    df <- cbind(df, fd)
    df$Comments <- paste(unlist(Cm), collapse=" ")
    if (raw_boot) {
        cr <- matrix(x$boot_current, 1)
        colnames(cr) <- paste0("Boot_Curr_", 1:100)
        rf <- matrix(x$boot_reference, 1)
        colnames(rf) <- paste0("Boot_Ref_", 1:100)
        df <- cbind(df, cr, rf)
    }
    df
}

report_all <-
function(boot=TRUE, path=NULL, version=NULL, level=0.9)
{
    SPP <- rownames(.c4is$SPsub)
    n <- length(SPP)
    OUT <- list()
    ETA <- NULL
    if (.verbose())
        cat("processing species:\n")
    t0 <- proc.time()[3]
    for (i in seq_len(n)) {
        if (.verbose()) {
            cat("* ", i, "/", length(SPP), " ", SPP[i], ", ETA: ",
                getTimeAsString(ETA), sep="")
            flush.console()
        }
        load_species_data(SPP[i], boot=boot, path=path, version=version)
        OUT[[i]] <- calculate_results(level=level)
        dt <- proc.time()[3] - t0
        cat(", elapsed:", getTimeAsString(dt), "\n")
        ETA <- (n - i) * dt / i
    }
    names(OUT) <- SPP
    OUT
}

custom_report <-
function(id=NULL, species="all",
path=NULL, version=NULL,
address=NULL, boot=TRUE,
level=0.9, raw_boot=FALSE, limit=0.01)
{
    load_common_data(path=path, version=version)

    if (.verbose()) {
        cat("arranging subsets\n")
        flush.console()
    }
    subset_common_data(id=id, species=species)

    OUT <- report_all(boot=boot, path=path, version=version, level=level)
    rval <- do.call(rbind, lapply(OUT, flatten_results, raw_boot=raw_boot,
        limit=limit))
    if (!is.null(address)) {
        sender <- getOption("cure4insect")$sender
        subject <- "Custom Report"
        body <- list("Hi,\n\nYour custom report results are attached.\n\nWith regards,\n\nthe ABMI Science",
            mime_part(rval, paste0("Custom_Report_", Sys.Date())))
        try(sent <- sendmail(sprintf("<%s>", sender),
            sprintf("<%s>", address),
            subject, body,
            control=list(smtpServer="ASPMX.L.GOOGLE.COM")))
        if (!inherits(sent, "try-error"))
            cat("email sent to", address, "\n")
    }
    rval
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
        stop("use `load_common_data()` first")
    .c4if$XY
}

get_species_table <- function()  {
    if (!is_loaded())
        stop("use `load_common_data()` first")
    .c4if$SP
}

get_all_id <- function()
    rownames(coordinates(get_id_locations()))

get_all_species <- function()
    rownames(get_species_table())

.verbose <- function() {
    x <- getOption("cure4insect")$verbose
    !is.null(x) && x > 0
}
