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
    o <- over(XY, as(ply, "SpatialPolygons"))
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

.truncate <- function(x, trunc=NULL) {
    if (is.null(trunc))
        v <- as.numeric(getOption("cure4insect")$trunc)
    if (inherits(x, "raster")) {
        z <- values(x)
        q <- quantile(z, v, na.rm=TRUE)
        z[!is.na(z) & z > q] <- q
        values(x) <- z
    } else {
        q <- quantile(x, v, na.rm=TRUE)
        x[x > q] <- q
    }
    x
}

## SD and CoV applies to current abudnance
rasterize_results <- function(y)
{
    if (!is_loaded())
        stop("common data needed: use load_common_data")
    if (length(names(y)) < 1)
        stop("species data needed: use load_species_data")
    KT <- .c4if$KT
    ## handle non additivity of spclim component (sertor vs total)
    if (getOption("cure4insect")$version != "2017" && y$taxon != "birds") {
        NC <- y$Totals[,"Curr"]
        NR <- y$Totals[,"Ref"]
    } else {
        NC <- rowSums(y$SA.Curr)
        NR <- rowSums(y$SA.Ref)
    }
    NC <- .truncate(NC)
    NR <- .truncate(NR)

    SI <- 100 * pmin(NC, NR) / pmax(NC, NR)
    SI2 <- ifelse(NC <= NR, SI, 200 - SI)
    i <- match(rownames(KT), names(NC))
    KT$NC <- NC[i]
    KT$NR <- NR[i]
    KT$SI <- SI[i]
    KT$SI2 <- SI2[i]
    nr <- levels(.c4if$KT$reg_nr)
    if (y$taxon != "birds")
        nr <- nr[nr %ni% "Rocky Mountain"]
    if (!all(c(y$model_north, y$model_south))) {
        ss <- !.select_id(if (y$model_north) "north" else "south", nr=nr)
    } else {
        ss <- !.select_id("both", nr=nr)
    }
    if (any(ss)) {
        KT$NC[ss] <- NA
        KT$NR[ss] <- NA
        KT$SI[ss] <- NA
        KT$SI2[ss] <- NA
    }
    if (y$boot) {
        CB <- y$Curr.Boot
        SE <- apply(CB, 1, sd)
        CV <- SE / rowMeans(CB)
        j <- match(KT$Row10_Col10, rownames(CB))
        KT$SE <- SE[j]
        KT$CV <- CV[j]
        if (any(ss)) {
            KT$SE[ss] <- NA
            KT$CV[ss] <- NA
        }
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
function(species, path=NULL, version=NULL)
{
    if (!is_loaded())
        stop("common data needed: use load_common_data")
    opts <- getOption("cure4insect")
    ROADCOEF <- (-Inf)
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
            if (version == "2017")
                cveg["SoftLin"] <- log(mean(exp(cveg[c("Shrub", "GrassHerb")])))
            cveg["HardLin"] <- ROADCOEF
        } else {
            cveg <- NULL
        }
        if (spinfo$model_south) {
            csoil <- .c4if$CFbirds$joint$soil[species,] # log scale
            if (version == "2017")
                csoil["SoftLin"] <- log(mean(exp(csoil), na.rm=TRUE)) # SoftLin is NA
            csoil["HardLin"] <- ROADCOEF
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
    assign("version", version, envir=y)
    assign("species", species, envir=y)
    assign("taxon", taxon, envir=y)
    assign("cveg", cveg, envir=y)
    assign("csoil", csoil, envir=y)
    assign("caspen", caspen, envir=y)
    fn <- file.path(path, version, "results", taxon, "spclim", paste0(species, ".RData"))
    if (!startsWith(path, "https://")) {
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
function(xy, veg, soil, method="simple")
{
    ## rw and iw are weights for the NORTH
    rw <- raster(system.file("extdata/wNorth.tif", package="cure4insect"))
    if (!identicalCRS(xy, rw))
        xy <- spTransform(xy, proj4string(rw))
    iw <- extract(rw, xy, method)
    .combine_veg_soil(iw, veg, soil)
}

## w: weight captures the weight for the NORTH models when in overlap
## which needs pre processing (N only=1, S only=0, overlap=w)
.combine_veg_soil <-
function(w, veg, soil)
{
    w * veg + (1 - w) * soil
}

.tr_xy <- function(xy) {
    if (inherits(xy, "SpatialPoints"))
        return(xy)
    if (is.null(dim(xy)))
        stop("xy must be a SpatialPoints or a matrix-like object")
    if (ncol(xy) != 2L)
        stop("xy must have 2 columns (lon, lat)")
    xy <- as.data.frame(xy)
    colnames(xy) <- c("X", "Y")
    coordinates(xy) <- ~ X + Y
    proj4string(xy) <- proj4string(.c4if$XY)
    xy
}

## handle soft lin aspect through an option for birds:
## coef approach does not require rf, early seral does ???
predict.c4ispclim <-
function(object, xy, veg, soil, method="simple", ...)
{
#    if (!inherits(xy, "SpatialPoints"))
#        stop("xy must be of class SpatialPoints")
    xy <- .tr_xy(xy)
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
            iveg <- extract(object$rveg, xy, method)
            OUT$veg <- fi(object$cveg[match(veg, names(object$cveg))] + iveg)
            if (any(veg == "SoftLin") && object$taxon == "birds" && object$version == "2017") {
                warning("veg contained SoftLin: check your assumptions")
                OUT$veg[veg == "SoftLin"] <- NA
            }
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
            isoil <- extract(object$rsoil, xy, method)
            ## pAspen here used as habitat covariate NOT as North weight
            rpa <- raster(system.file("extdata/pAspen.tif", package="cure4insect"))
            ipa <- extract(rpa, xy, method)
            OUT$soil <- fi(object$csoil[match(soil, names(object$csoil))] +
                object$caspen * ipa + isoil)
            if (any(soil == "SoftLin") && object$taxon == "birds" && object$version == "2017") {
                warning("soil contained SoftLin: check your assumptions")
                OUT$soil[soil == "SoftLin"] <- NA
            }
        }
    }
    if (DO$comb) {
        #OUT$comb <- .combine_veg_soil(ipa, OUT$veg, OUT$soil)
        OUT$comb <- combine_veg_soil(xy, OUT$veg, OUT$soil, method=method)
    }
    class(OUT) <- c("c4ippred", class(OUT))
    OUT
}

predict_mat <- function (object, ...)
    UseMethod("predict_mat")

predict_mat.c4ispclim <-
function(object, xy, veg, soil, method="simple", ...)
{
    xy <- .tr_xy(xy)
    ## coefs in object are on log/logit scale, need linkinv
    fi <- if (object$taxon == "birds")
        poisson("log")$linkinv else binomial("logit")$linkinv
    if (missing(veg) && missing(soil))
        stop("veg or soil must be provided")
    xy <- spTransform(xy, proj4string(.read_raster_template()))
    if (!missing(veg) && !is.null(veg)) {
        if (is.null(object$cveg)) {
            warning(sprintf("veg based estimates are unavailable for %s", object$species))
            Nveg <- NULL
        } else {
            if (nrow(veg) != nrow(coordinates(xy)))
                stop("nrow(veg) must equal number of points in xy")
            .check(as.factor(colnames(veg)), names(object$cveg))
            iveg <- extract(object$rveg, xy, method)
            imatv <- t(array(iveg, dim(veg), dimnames(veg)))
            mveg <- object$cveg[match(colnames(veg), names(object$cveg))]
            Nveg <- fi(t(mveg + imatv)) * veg
            if (any(colnames(veg) == "SoftLin") && object$taxon == "birds" && object$version == "2017") {
                warning("veg contained SoftLin: check your assumptions")
                Nveg[,colnames(veg) == "SoftLin"] <- NA
            }
        }
    } else {
        Nveg <- NULL
    }
    if (!missing(soil) && !is.null(soil)) {
        if (is.null(object$csoil)) {
            warning(sprintf("soil based estimates are unavailable for %s", object$species))
            Nsoil <- NULL
        } else {
            if (nrow(soil) != nrow(coordinates(xy)))
                stop("nrow(veg) must equal number of points in xy")
            .check(as.factor(colnames(soil)), names(object$csoil))
            isoil <- extract(object$rsoil, xy, method)
            ## pAspen here used as habitat covariate NOT as North weight
            rpa <- raster(system.file("extdata/pAspen.tif", package="cure4insect"))
            ipa <- extract(rpa, xy, method)
            imats <- t(array(object$caspen * ipa + isoil, dim(soil), dimnames(soil)))
            msoil <- object$csoil[match(colnames(soil), names(object$csoil))]
            Nsoil <- fi(t(msoil + imats)) * soil
            if (any(colnames(soil) == "SoftLin") && object$taxon == "birds" && object$version == "2017") {
                warning("soil contained SoftLin: check your assumptions")
                Nsoil[,colnames(soil) == "SoftLin"] <- NA
            }
        }
    } else {
        Nsoil <- NULL
    }
    OUT <- list(veg=Nveg, soil=Nsoil)
    class(OUT) <- c("c4ippredmat")
    OUT
}

## sentinel value used to add back NAs, but NA pattern can vary, so set to
## some preset value (0 for abund, 100 for SI) instead
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
    ## handle non additivity of spclim component (sertor vs total)
    if (getOption("cure4insect")$version != "2017" && y$taxon != "birds") {
        NC <- y$Totals[,"Curr"]
    } else {
        NC <- rowSums(y$SA.Curr)
    }
    NC <- .truncate(NC)

    i <- match(rownames(KT), names(NC))
    KT$NC <- NC[i]
    #KT$NC[is.na(KT$NC)] <- -1
    KT$NC[is.na(KT$NC)] <- 0
    if (type == "intactness") {
        NR <- rowSums(y$SA.Ref)
        SI <- 100 * pmin(NC, NR) / pmax(NC, NR)
        KT$SI <- SI[i]
        #KT$SI[is.na(KT$SI)] <- -1
        KT$SI[is.na(KT$SI)] <- 100
    }
    r <- .make_raster(KT[,z], rc=KT, rt=rt)
    #r[r < 0] <- NA # sentinel values to NA
    r
}

## multi-species intactness and richness maps
## clip: apply spatial IDs to crop & mask
## limit: threshold for intactness average
##        NOTE: limit is applied on the whole region
##        and not by LUF x NSR (as required by BMF)
make_multispecies_map <-
function(type=c("richness", "intactness"),
path=NULL, version=NULL, clip=TRUE, limit=NULL,
area="ha", pair_adj=2)
{
    type <- match.arg(type)
    SPP <- rownames(.c4is$SPsub)
    n <- length(SPP)
    KEEP <- rep(TRUE, n)
    rt <- .read_raster_template()
    rmask <- make_subset_map()
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
        MSG <- sprintf("--- DROPPED (%.3f%s)", 100*LIM$mean / LIM$max, "% of limit")
    } else {
        MSG <- sprintf("(%.1f%s)", 100*LIM$mean / LIM$max, "% of limit")
    }
    if (type == "richness" && as.character(.c4is$SPsub[SPP[1L], "taxon"]) == "birds")
            r0 <- p_bird(r0, area=area, pair_adj=pair_adj)
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
            MSG <- sprintf("--- DROPPED (%.3f%s)", 100*LIM$mean / LIM$max, "% of limit")
        } else {
            r <- .rasterize_multi(y, type, rt)
            if (clip)
                r <- mask(r, rmask)
            if (type == "richness" &&
                as.character(.c4is$SPsub[SPP[1L], "taxon"]) == "birds")
                    r <- p_bird(r, area=area, pair_adj=pair_adj)
            r0 <- r + r0
            MSG <- sprintf("(%.1f%s)", 100*LIM$mean / LIM$max, "% of limit")
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

make_subset_map <-
function()
{
    .make_raster(ifelse(rownames(.c4if$KT) %in% rownames(.c4is$KTsub), 1, 0),
        .c4if$KT, .read_raster_template())
}
