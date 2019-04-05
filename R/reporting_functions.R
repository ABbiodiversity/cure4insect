## todo: keep model_north and model_south columns, columns to rename

flatten <- function (x, ...) UseMethod("flatten")
flatten.c4iraw <-
function(x, raw_boot=FALSE, limit=NULL, ...)
{
    if (is.null(limit))
        limit <- as.numeric(getOption("cure4insect")$limit)
    if (limit %)(% c(0,1))
        stop("limit value must be between in [0, 1]")
    Cm <- list()
    df <- data.frame(SpeciesID=x$species, Taxon=x$taxon)
    tmp <- data.frame(.c4if$SP[x$species, c("CommonName", "ScientificName",
        "TSNID", "model_north", "model_south", "model_region", "habitat_assoc", "native")],
        .c4if$VER[x$taxon,c("version", "yr_first",
        "yr_last", "method", "hf", "veg", "model")])
    ## if full data not loaded, we get NULL
    if (nrow(tmp) < 1L) {
        #warning("load_common_data before flattening to avoid some NAs")
        tmp <- data.frame(CommonName=NA, ScientificName=NA,
            TSNID=NA, model_north=NA, model_south=NA, model_region=NA,
            habitat_assoc=NA, native=NA,
            version=NA, yr_first=NA, yr_last=NA, method=NA,
            hf=NA, veg=NA, model=NA)
    } else {
        tmp <- droplevels(tmp)
    }
    df <- data.frame(df, tmp)
    rownames(df) <- x$species
    df$model_north <- x$model_north
    df$model_south <- x$model_south
    df$Mean <- x$mean
    df$Max <- x$max
    df$Limit <- limit
    df$Predicted <- x$predicted
    KEEP <- x$mean >= x$max * limit
    df$Keep <- KEEP
    if (!KEEP) {
        msg <- if (x$predicted) {
            paste0("Regional mean abundance <",
                 round(100*limit,1), "% of provincial maximum.")
        } else {
            "No overlap between selected spatial subset and modelled species range."
        }
        Cm[[length(Cm)+1]] <- msg
    }
    if (!x$boot) {
        df$CI_Level <- NA
        Cm[[length(Cm)+1]] <- "Confidence intervals were not requested."
    } else {
        df$CI_Level <- x$level
    }
    df$Abund_Curr_Est <- x$intactness["Current", 1]
    if (df$Abund_Curr_Est == 0) {
        df$Abund_Curr_LCL <- 0
        df$Abund_Curr_UCL <- 0
    } else {
        df$Abund_Curr_LCL <- x$intactness["Current", 2]
        df$Abund_Curr_UCL <- x$intactness["Current", 3]
        if (x$boot && x$intactness["Current",1] %)(% x$intactness["Current",2:3])
            Cm[[length(Cm)+1]] <- "Current abundance estimate is outside of CI: region probably too small."
    }
    df$Abund_Ref_Est <- x$intactness["Reference", 1]
    if (df$Abund_Ref_Est == 0) {
        df$Abund_Ref_LCL <- 0
        df$Abund_Ref_UCL <- 0
    } else {
        df$Abund_Ref_LCL <- x$intactness["Reference", 2]
        df$Abund_Ref_UCL <- x$intactness["Reference", 3]
        if (x$boot && x$intactness["Reference",1] %)(% x$intactness["Reference",2:3])
            Cm[[length(Cm)+1]] <- "Reference abundance estimate is outside of CI: region probably too small."
    }
    if (is.na(x$intactness["Intactness", 1])) {
        df$SI_Est <- NA
        df$SI2_Est <- NA
        df$SI2_LCL <- NA
        df$SI2_UCL <- NA
    } else {
        df$SI_Est <- x$intactness["Intactness", 1]
        df$SI2_Est <- x$intactness["Intactness2", 1]
        df$SI2_LCL <- x$intactness["Intactness2", 2]
        df$SI2_UCL <- x$intactness["Intactness2", 3]
        if (x$predicted && x$boot &&
            x$intactness["Intactness2",1] %)(% x$intactness["Intactness2",2:3])
                Cm[[length(Cm)+1]] <- "Two-sided intactness estimate is outside of CI."
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

.get_cores <- function(cores=NULL) {
    if (is.null(cores))
        cores <- as.integer(getOption("cure4insect")$cores)
    as.integer(max(1, min(detectCores(), cores, na.rm=TRUE)))
}

report_all <-
function(boot=NULL, path=NULL, version=NULL, level=0.9, cores=NULL)
{
    if (!is_loaded())
        stop("common data needed: use load_common_data")
    SPP <- rownames(.c4is$SPsub)
    cores <- .get_cores(cores=cores)
    if (cores > 1L) {
        if (.Platform$OS.type == "windows") {
            cl <- makeCluster(cores)
            clusterEvalQ(cl, library(cure4insect))
            on.exit(stopCluster(cl))
        } else {
            cl <- cores
        }
    } else {
        cl <- NULL
    }
    if (.verbose()) {
        cat("processing species: ",
            if (!is.null(cl)) "parallel " else "",
            "work in progress...\n", sep="")
    } else {
        opb <- pboptions(type="none")
        on.exit(pboptions(opb), add=TRUE)
    }
    fun <- function(z, boot=NULL, path=NULL, version=NULL, level=0.9, .c4is) {
        .calculate_results(.load_species_data(z,
            boot=boot, path=path, version=version,
            taxon=as.character(.c4is$SPsub[z, "taxon"]),
            model_north=.c4is$SPsub[z, "model_north"],
            model_south=.c4is$SPsub[z, "model_south"]),
            level=level, .c4is=.c4is)
    }
    OUT <- pblapply(SPP, fun, boot=boot, path=path, version=version,
        level=level, .c4is=as.list(.c4is), cl=cl)
    names(OUT) <- SPP
    class(OUT) <- "c4ilist"
    OUT
}

## this is for testing: i.e. looking into failed requests (cores ignored)
.report_all_by1 <-
function(boot=NULL, path=NULL, version=NULL, level=0.9, cores=NULL)
{
    if (!is_loaded())
        stop("common data needed: use load_common_data")
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
        y <- load_species_data(SPP[i], boot=boot, path=path, version=version)
        OUT[[i]] <- calculate_results(y, level=level)
        dt <- proc.time()[3] - t0
        cat(", elapsed:", getTimeAsString(dt), "\n")
        ETA <- (n - i) * dt / i
    }
    names(OUT) <- SPP
    class(OUT) <- "c4ilist"
    OUT
}

custom_report <-
function(id=NULL, species="all",
path=NULL, version=NULL,
address=NULL, boot=NULL,
level=0.9, cores=NULL,
raw_boot=FALSE, limit=NULL)
{
    load_common_data(path=path, version=version)
    subset_common_data(id=id, species=species)
    OUT <- report_all(boot=boot, path=path, version=version,
        level=level, cores=cores)
    rval <- do.call(rbind, lapply(OUT, flatten, raw_boot=raw_boot,
        limit=limit))
    class(rval) <- c("c4iblock", class(rval))
    .send_email(address, mimepart=rval)
    class(rval) <- c("c4idf", "data.frame")
    rval
}

custom_predict <-
function(species, xy, veg, soil, path=NULL, version=NULL, ...)
{
    load_common_data(path=path, version=version)
    object <- load_spclim_data(species=species, path=path, version=version)
    if (!missing(veg) && !is.factor(veg))
        veg <- as.factor(veg)
    if (!missing(soil) && !is.factor(soil))
        soil <- as.factor(soil)
    predict(object, xy=xy, veg=veg, soil=soil, ...)
}
