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

if (FALSE) {

## add pAspen and pWater to KT or make tif?

load_common_data()
load_species_data("Ovenbird")
x <- rasterize_results()

type="CV"
## color gradient for reference and current
col_abund <- rev(c("#D73027","#FC8D59","#FEE090","#E0F3F8","#91BFDB","#4575B4"))
## color gradient for difference map
col_diff <- c("#C51B7D","#E9A3C9","#FDE0EF","#E6F5D0","#A1D76A","#4D9221")
## color gradient for SI map
col_SI <- c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE08B",
    "#FFFFBF","#D9EF8B", "#A6D96A", "#66BD63", "#1A9850", "#006837")
## color gradient for CV & SE map: rev(brewer.pal(10, "RdYlGn"))
col_unc <- rev(c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE08B", "#D9EF8B",
    "#A6D96A", "#66BD63", "#1A9850", "#006837"))
col_water <- rgb(0.4, 0.3, 0.8)
col_excl <- "lightcyan4"
br <- c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, Inf)

col <- switch(type,
    "NC" =col_abund,
    "NR" =col_abund,
    "DF" =col_diff,
    "SI" =col_SI,
    "SI2"=col_diff,
    "SE" =col_unc,
    "CV" =col_unc)
r <- if (type == "DF")
    x[["NC"]] - x[["NR"]] else x[[type]]
if (type == "SE")
    Mean <- mean(values(x[["SE"]] / x[["CV"]]), na.rm=TRUE)
if (type %in% c("NC", "NR"))
    Max <- max(values(x[["NC"]]), values(x[["NR"]]), na.rm=TRUE)
m <- if (type %in% c("DF", "SI2"))
    200 else 100
# import from grDevices:
cols <- colorRampPalette(cls)(m)
if (type %in% c("SE", "CV"))
    cols <- col_unc
#.read_raster_template <- cure4insect:::.read_raster_template
rt <- .read_raster_template()
df_fun <- function(df) {
    df <- df / Max
    df <- sign(df) * abs(df)^0.5
    df <- pmin(200, ceiling(99 * df)+100)
    #df[df==0] <- 1
    df
}
vals <- switch(type,
    "NC" =pmax(1, pmin(100, round(100 * sqrt(values(r)/Max)))),
    "NR" =pmax(1, pmin(100, round(100 * sqrt(values(r)/Max)))),
    "DF" =df_fun(values(r)),
    "SI" =pmax(1, pmin(100, round(values(r)))),
    "SI2"=pmax(1, pmin(200, round(values(r)))),
    "SE" =cut(values(r)/Mean, breaks=br, include.lowest=TRUE, labels=FALSE),
    "CV" =cut(values(r), breaks=br, include.lowest=TRUE, labels=FALSE))
cols <- cols[ceiling(min(vals, na.rm=TRUE)):ceiling(max(vals, na.rm=TRUE))]
values(r) <- vals
#plot(rt, col=col_excl, axes=FALSE, box=FALSE)
plot(r, col=cols, axes=FALSE, box=FALSE)

#crvegm <- rowMeans(crveg)
#crvegsd <- apply(crveg, 1, sd)
#crvegsd[crvegsd==0] <- 0.000001
#covC <- crvegsd / crvegm
#covC[crvegm==0] <- mean(covC[crvegm!=0], na.rm=TRUE) # will not stick out...

#SI <- round(100 * pmin(cr, rf) / pmax(cr, rf))
#SI[is.na(SI)] <- 100 # 0/0 is defined as 100 intact
# CoV

## polygon level prediction

library(cure4insect)
#species="AlderFlycatcher"
species="Achillea.millefolium"
load_common_data()
.c4if=cure4insect:::.c4if
#path="w:/reports"
y <- load_spclim_data(species)
DO <- list(
    veg_rf=T,veg_cr=T,
    soil_rf=T,soil_cr=T,
    comb_rf=T,comb_cr=T)

## handle soft lin aspect through an option for birds:
## coef approach does not require rf, early seral does ???
predict.c4ispclim <-
function(object, veg_rf, veg_cr, soil_rf, soil_cr, xy, ...)
{
    if (!inherits(xy, "SpatialPoints"))
        stop("xy must be of class SpatialPoints")
    .check <- function(x, ref) {
        z <- deparse(substitute(x))
        if (!is.factor(x))
            stop(paste(z, "is not factor"))
        if (any(levels(x) %ni% ref))
            warning(paste(z, "had unmatched levels: NA's introduced"))
        NULL
    }
    fi <- if (object$taxon == "birds")
        poisson("log")$linkinv else binomial("logit")$linkinv
    DO <- list(
        veg_rf=!missing(veg_rf),
        veg_cr=!missing(veg_cr),
        soil_rf=!missing(soil_rf),
        soil_cr=!missing(soil_cr))
    DO$comb_rf <- DO$veg_rf & DO$soil_rf
    DO$comb_cr <- DO$veg_cr & DO$soil_cr
    OUT <- data.frame(matrix(NA, nrow(coordinates(xy)), 6))
    colnames(OUT) <- names(DO)
    xy <- spTransform(xy, proj4string(.read_raster_template()))
    if (DO$comb_rf || DO$comb_cr) {
        rpa <- raster(system.file("extdata/pAspen.tif", package="cure4insect"))
        ipa <- extract(rpa, xy)
    }
    if (DO$veg_rf || DO$veg_cr) {
        iveg <- extract(object$rveg, xy)
    }
    if (DO$soil_rf || DO$soil_cr) {
        isoil <- extract(object$rsoil, xy)
    }
    if (veg_rf) {
        .check(veg_rf)
        OUT$veg_rf <- fi(object$cveg[match(veg_rf, names(object$cveg))] + iveg)
    }
    if (veg_cr) {
        .check(veg_cr)
        OUT$veg_cr <- fi(object$cveg[match(veg_cr, names(object$cveg))] + iveg)
    }
    if (soil_rf) {
        .check(soil_rf)
        OUT$soil_rf <- fi(object$csoil[match(soil_rf, names(object$csoil))] + isoil)
    }
    if (veg_cr) {
        .check(soil_cr)
        OUT$soil_cr <- fi(object$csoil[match(soil_cr, names(object$csoil))] + isoil)
    }
    if (comb_rf) {
        OUT$comb_rf <- ipa * veg_rf + (1 - ipa) * soil_rf
    }
    if (comb_cr) {
        OUT$comb_cr <- ipa * veg_cr + (1 - ipa) * soil_cr
    }
    OUT
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
    taxon <- as.character(.c4if$SP[species, "taxon"])
    .load_spclim_data(species=species,
        boot=boot, path=path, version=version, taxon=taxon)
}
.load_spclim_data <-
function(species, boot=TRUE, path=NULL, version=NULL, taxon)
{
    opts <- getOption("cure4insect")
    if (is.null(path))
        path <- opts$path
    if (is.null(version))
        version <- opts$version
    if (taxon == "birds") {
        cveg <- .c4if$CFbirds$marginal$veg[species,]
        cveg["HardLin"] <- 0
        cveg["SoftLin"] <- mean(cveg[c("Shrub", "GrassHerb")])
        csoil <- .c4if$CFbirds$marginal$soil[species,]
        csoil["HardLin"] <- 0
        csoil["SoftLin"] <- mean(csoil[c("Shrub", "GrassHerb")])
    } else {
        cveg <- .c4if$CF$coef$veg[species,]
        csoil <- .c4if$CF$coef$soil[species,]
    }
    cveg <- cveg[names(cveg) %ni% c("AverageCoef", "SoftLin10", "HardLin10")]
    csoil <- csoil[names(csoil) %ni% c("AverageCoef", "SoftLin10", "HardLin10")]
    y <- new.env()
    assign("species", species, envir=y)
    assign("taxon", taxon, envir=y)
    assign("cveg", cveg, envir=y)
    assign("csoil", csoil, envir=y)
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



}
