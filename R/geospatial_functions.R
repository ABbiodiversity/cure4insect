overlay_polygon <-
function(ply)
{
    XY <- get_id_locations() # stops when not yet loaded
    if (!inherits(ply, "SpatialPolygons"))
        stop("must inherit from class SpatialPolygons")
    if (!identicalCRS(XY, ply))
        ply <- spTransform(ply, proj4string(XY))
    if (.verbose())
        cat("running spatial overlay\n")
    o <- over(XY, ply)
    rownames(coordinates(XY))[!is.na(o)]
}

.read_raster_template <- function()
    raster(system.file("extdata/AB_1km_mask.tif", package="cure4insect"))

## rc is table with row and col indices for value
## rt is raster template to be used
.make_raster <-
function(value, rc, rt)
{
    value <- as.numeric(value)
    r <- as.matrix(Xtab(value ~ Row + Col, rc))
    r[is.na(as.matrix(rt))] <- NA
    raster(x=r, template=rt)
}

## SD and CoV applies to current abudnance
rasterize_results <- function(y)
{
    if (!is_loaded())
        stop("common data needed: use load_common_data")
    if (length(names(y)) < 1)
        stop("species data needed: use load_species_data")
    KT <- .c4if$KT
    NC <- rowSums(y$SA.Curr)
    NR <- rowSums(y$SA.Ref)
    SI <- 100 * pmin(NC, NR) / pmax(NC, NR)
    SI2 <- ifelse(NC <= NR, SI, 200 - SI)
    i <- match(rownames(KT), names(NC))
    KT$NC <- NC[i]
    KT$NR <- NR[i]
    KT$SI <- SI[i]
    KT$SI2 <- SI2[i]

    if (y$boot) {
        CB <- y$Curr.Boot
        SE <- apply(CB, 1, sd)
        CV <- SE / rowMeans(CB)
        j <- match(KT$Row10_Col10, rownames(CB))
        KT$SE <- SE[j]
        KT$CV <- CV[j]
    } else {
        KT$SE <- NA
        KT$CV <- NA
    }
    ## sentinel value for NAs: recover value from raster
    KT$NC[is.na(KT$NC)] <- -1
    KT$NR[is.na(KT$NR)] <- -1
    KT$SI[is.na(KT$SI)] <- -1
    KT$SI2[is.na(KT$SI2)] <- -1
    KT$SE[is.na(KT$SE)] <- -1
    KT$CV[is.na(KT$CV)] <- -1
    requireNamespace("raster")
    rt <- .read_raster_template()
    cn <- c("NC", "NR", "SI", "SI2", "SE", "CV")
    rl <- lapply(cn, function(z) {
        r <- .make_raster(KT[,z], rc=KT, rt=rt)
        r[r < 0] <- NA # sentinel values to NA
        r
    })
    names(rl) <- cn
    stack(rl)
}

load_spclim_data <-
function(species, boot=TRUE, path=NULL, version=NULL)
{
    if (!is_loaded())
        stop("common data needed: use load_common_data")
    opts <- getOption("cure4insect")
    if (is.null(path))
        path <- opts$path
    if (is.null(version))
        version <- opts$version
    spinfo <- .c4if$SP[species, ]
    taxon <- as.character(spinfo$taxon)
    ## joint and marginal coefs for birds are on log scale
    if (taxon == "birds") {
        if (spinfo$model_north) {
            cveg <- .c4if$CFbirds$joint$veg[species,] # log scale
            cveg["SoftLin"] <- log(mean(exp(cveg[c("Shrub", "GrassHerb")])))
            cveg["HardLin"] <- -10
        } else {
            cveg <- NULL
        }
        if (spinfo$model_south) {
            csoil <- .c4if$CFbirds$joint$soil[species,] # log scale
            csoil["SoftLin"] <- log(mean(exp(csoil), na.rm=TRUE)) # SoftLin is NA
            csoil["HardLin"] <- -10
            caspen <- .c4if$CFbirds$joint$paspen[species,]
        } else {
            csoil <- NULL
            caspen <- NULL
        }
    ## marginal coefs for other taxa are on probability scale
    } else {
        if (spinfo$model_north) {
            cveg <- binomial("logit")$linkfun(.c4if$CF$coef$veg[species,]) # p scale
        } else {
            cveg <- NULL
        }
        if (spinfo$model_south) {
            csoil <- binomial("logit")$linkfun(.c4if$CF$coef$soil[species,]) # p scale
            caspen <- .c4if$CF$coef$paspen[species,]
        } else {
            csoil <- NULL
            caspen <- NULL
        }
    }
    cveg <- cveg[get_levels()$veg]
    csoil <- csoil[get_levels()$soil]
    y <- new.env()
    assign("species", species, envir=y)
    assign("taxon", taxon, envir=y)
    assign("cveg", cveg, envir=y)
    assign("csoil", csoil, envir=y)
    assign("caspen", caspen, envir=y)
    fn <- file.path(path, version, "results", taxon, "spclim", paste0(species, ".RData"))
    if (!startsWith(path, "http://")) {
        load(fn, envir=y)
    } else {
        con <- url(fn)
        load(con, envir=y)
        close(con)
    }
    class(y) <- "c4ispclim"
    y
}

get_levels <- function()
    list(veg=colnames(.c4if$CF$lower$veg), soil=colnames(.c4if$CF$lower$soil))

.check <- function(x, ref) {
    z <- deparse(substitute(x))
    if (!is.factor(x))
        stop(paste(z, "is not factor"))
    if (any(levels(x) %ni% ref))
        warning(paste(z, "had unmatched levels: NA's introduced"))
    NULL
}

combine_veg_soil <-
function(xy, veg, soil)
{
    rpa <- raster(system.file("extdata/pAspen.tif", package="cure4insect"))
    if (!identicalCRS(xy, rpa))
        xy <- spTransform(xy, proj4string(rpa))
    ipa <- extract(rpa, xy)
    .combine_veg_soil(ipa, veg, soil)
}
.combine_veg_soil <-
function(ipa, veg, soil)
{
    ipa * veg + (1 - ipa) * soil
}

## handle soft lin aspect through an option for birds:
## coef approach does not require rf, early seral does ???
predict.c4ispclim <-
function(object, xy, veg, soil, ...)
{
    if (!inherits(xy, "SpatialPoints"))
        stop("xy must be of class SpatialPoints")
    ## coefs in object are on log/logit scale, need linkinv
    fi <- if (object$taxon == "birds")
        poisson("log")$linkinv else binomial("logit")$linkinv
    if (missing(veg) && missing(soil))
        stop("veg or soil must be provided")
    DO <- list(
        veg=!missing(veg),
        soil=!missing(soil))
    DO$comb <- DO$veg & DO$soil
    OUT <- data.frame(matrix(NA, nrow(coordinates(xy)), 3))
    colnames(OUT) <- names(DO)
    xy <- spTransform(xy, proj4string(.read_raster_template()))
    if (DO$veg) {
        if (is.null(object$cveg)) {
            warning(sprintf("veg based estimates are unavailable for %s", object$species))
            DO$comb <- FALSE
        } else {
            if (length(veg) != nrow(coordinates(xy)))
                stop("length(veg) must equal number of points in xy")
            .check(veg, names(object$cveg))
            #if (any(veg == "SoftLin") && object$taxon == "birds")
            #    warning("veg contained SoftLin: check your assumptions")
            iveg <- extract(object$rveg, xy)
            OUT$veg <- fi(object$cveg[match(veg, names(object$cveg))] + iveg)
        }
    }
    if (DO$soil) {
        if (is.null(object$csoil)) {
            warning(sprintf("soil based estimates are unavailable for %s", object$species))
            DO$comb <- FALSE
        } else {
            if (length(soil) != nrow(coordinates(xy)))
                stop("length(soil) must equal number of points in xy")
            .check(soil, names(object$csoil))
            #if (any(soil == "SoftLin") && object$taxon == "birds")
            #    warning("soil contained SoftLin: check your assumptions")
            isoil <- extract(object$rsoil, xy)
            rpa <- raster(system.file("extdata/pAspen.tif", package="cure4insect"))
            ipa <- extract(rpa, xy)
            OUT$soil <- fi(object$csoil[match(soil, names(object$csoil))] +
                object$caspen * ipa + isoil)
        }
    }
    if (DO$comb) {
        OUT$comb <- .combine_veg_soil(ipa, OUT$veg, OUT$soil)
    }
    class(OUT) <- c("c4ippred", class(OUT))
    OUT
}

predict_mat <- function (object, ...)
    UseMethod("predict_mat")

predict_mat.c4ispclim <-
function(object, xy, veg, soil, ...)
{
    if (!inherits(xy, "SpatialPoints"))
        stop("xy must be of class SpatialPoints")
    ## coefs in object are on log/logit scale, need linkinv
    fi <- if (object$taxon == "birds")
        poisson("log")$linkinv else binomial("logit")$linkinv
    if (missing(veg) && missing(soil))
        stop("veg or soil must be provided")
    xy <- spTransform(xy, proj4string(.read_raster_template()))
    if (!missing(veg)) {
        if (is.null(object$cveg)) {
            warning(sprintf("veg based estimates are unavailable for %s", object$species))
            Nveg <- NULL
        } else {
            if (nrow(veg) != nrow(coordinates(xy)))
                stop("nrow(veg) must equal number of points in xy")
            .check(as.factor(colnames(veg)), names(object$cveg))
            if (any(colnames(veg) == "SoftLin") && object$taxon == "birds")
                warning("veg contained SoftLin: check your assumptions")
            iveg <- extract(object$rveg, xy)
            imatv <- t(array(iveg, dim(veg), dimnames(veg)))
            mveg <- object$cveg[match(colnames(veg), names(object$cveg))]
            Nveg <- fi(t(mveg + imatv)) * veg
        }
    } else {
        Nveg <- NULL
    }
    if (!missing(soil)) {
        if (is.null(object$csoil)) {
            warning(sprintf("soil based estimates are unavailable for %s", object$species))
            Nsoil <- NULL
        } else {
            if (nrow(soil) != nrow(coordinates(xy)))
                stop("nrow(veg) must equal number of points in xy")
            .check(as.factor(colnames(soil)), names(object$csoil))
            if (any(soil == "SoftLin") && object$taxon == "birds")
                warning("soil contained SoftLin: check your assumptions")
            isoil <- extract(object$rsoil, xy)
            rpa <- raster(system.file("extdata/pAspen.tif", package="cure4insect"))
            ipa <- extract(rpa, xy)
            imats <- t(array(object$caspen * ipa + isoil, dim(soil), dimnames(soil)))
            msoil <- object$csoil[match(colnames(soil), names(object$csoil))]
            Nsoil <- fi(t(msoil + imats)) * soil
        }
    } else {
        Nsoil <- NULL
    }
    OUT <- list(veg=Nveg, soil=Nsoil)
    class(OUT) <- c("c4ippredmat")
    OUT
}

.rasterize_multi <- function(y, type=c("richness", "intactness"), rt)
{
    if (!is_loaded())
        stop("common data needed: use load_common_data")
    if (length(names(y)) < 1)
        stop("species data needed: use load_species_data")
    z <- switch(type,
        "richness"="NC",
        "intactness"="SI")
    KT <- .c4if$KT
    NC <- rowSums(y$SA.Curr)
    i <- match(rownames(KT), names(NC))
    KT$NC <- NC[i]
    #KT$NC[is.na(KT$NC)] <- -1
    KT$NC[is.na(KT$NC)] <- -0
    if (type == "intactness") {
        NR <- rowSums(y$SA.Ref)
        SI <- 100 * pmin(NC, NR) / pmax(NC, NR)
        KT$SI <- SI[i]
        #KT$SI[is.na(KT$SI)] <- -1
        KT$SI[is.na(KT$SI)] <- 100
    }
    r <- .make_raster(KT[,z], rc=KT, rt=rt)
    r[r < 0] <- NA # sentinel values to NA
    r
}

## multi-species intactness and richness maps
## clip: apply spatial IDs to crop & mask
## limit: threshold for intactness average
make_multispecies_map <-
function(type=c("richness", "intactness"),
path=NULL, version=NULL, clip=TRUE, limit=NULL)
{
    type <- match.arg(type)
    SPP <- rownames(.c4is$SPsub)
    n <- length(SPP)
    KEEP <- rep(TRUE, n)
    requireNamespace("raster")
    rt <- .read_raster_template()
    rmask <- .make_raster(ifelse(rownames(.c4if$KT) %in% rownames(.c4is$KTsub), 1, 0),
        .c4if$KT, rt)
    rmask[rmask == 0] <- NA
    ETA <- NULL
    if (.verbose())
        cat("processing species:\n")
    t0 <- proc.time()[3]
    i <- 1L
    if (.verbose()) {
        cat("* ", i, "/", length(SPP), " ", SPP[i], ", ETA: ",
            getTimeAsString(ETA), sep="")
        flush.console()
    }
    y <- load_species_data(SPP[i], boot=FALSE, path=path, version=version)
    LIM <- .calculate_limit(y, limit=limit)
    KEEP[i] <- LIM$keep
    r0 <- .rasterize_multi(y, type, rt)
    if (clip)
        r0 <- mask(r0, rmask)
    if (!KEEP[i] && type == "intactness") {
        r0[!is.na(values(r0))] <- 0
        MSG <- sprintf("--- DOPPED (%.3f%s)", 100*LIM$mean / LIM$max, "%")
    } else {
        MSG <- sprintf("(%.1f%s)", 100*LIM$mean / LIM$max, "%")
    }
    if (type == "richness" && as.character(.c4is$SPsub[SPP[1L], "taxon"]) == "birds")
            r0 <- 1-exp(-1*r0)
    dt <- proc.time()[3] - t0
    cat(", elapsed:", getTimeAsString(dt), MSG, "\n")
    ETA <- (n - i) * dt / i
    for (i in seq_len(n)[-1]) {
        if (.verbose()) {
            cat("* ", i, "/", length(SPP), " ", SPP[i], ", ETA: ",
                getTimeAsString(ETA), sep="")
            flush.console()
        }
        y <- load_species_data(SPP[i], boot=FALSE, path=path, version=version)
        LIM <- .calculate_limit(y, limit=limit)
        KEEP[i] <- LIM$keep
        if (!KEEP[i] && type == "intactness") {
            MSG <- sprintf("--- DOPPED (%.3f%s)", 100*LIM$mean / LIM$max, "%")
        } else {
            r <- .rasterize_multi(y, type, rt)
            if (clip)
                r <- mask(r, rmask)
            if (type == "richness" &&
                as.character(.c4is$SPsub[SPP[1L], "taxon"]) == "birds")
                    r <- 1-exp(-1*r)
            r0 <- r + r0
            MSG <- sprintf("(%.1f%s)", 100*LIM$mean / LIM$max, "%")
        }
        dt <- proc.time()[3] - t0
        cat(", elapsed:", getTimeAsString(dt), MSG, "\n")
        ETA <- (n - i) * dt / i
    }
    if (type == "intactness")
        r0 <- r0 / sum(KEEP)
    if (clip)
        r0 <- trim(r0, values = NA)
    r0
}
