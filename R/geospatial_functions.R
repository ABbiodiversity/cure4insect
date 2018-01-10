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

## rc is table with row and col indices for value
## rt is raster template to be used
.make_raster <-
function(value, rc, rt)
{
    requireNamespace("raster")
    requireNamespace("mefa4")
    #library(raster)
    #library(mefa4)
    if (missing(rc)) {
        opts <- set_options(verbose=0)
        on.exit(set_options(opts))
        load_common_data()
        rc <- .c4if$KT
    }
    if (missing(rt))
        rt <- raster::raster(system.file("extdata/AB_1km_mask.tif", package="cure4insect"))
    value <- as.numeric(value)
    r <- as.matrix(mefa4::Xtab(value ~ Row + Col, rc))
    r[is.na(as.matrix(rt))] <- NA
    raster::raster(x=r, template=rt)
}

## SD and CoV applies to current abudnance
rasterize_results <- function()
{
    opts <- set_options(verbose=0)
    on.exit(set_options(opts))
    load_common_data()
    if (length(names(.c4i1)) < 1)
        stop("species data needed: use load_species_data")
    KT <- .c4if$KT
    NC <- rowSums(.c4i1$SA.Curr)
    NR <- rowSums(.c4i1$SA.Ref)
    SI <- 100 * pmin(NC, NR) / pmax(NC, NR)
    SI2 <- ifelse(NC <= NR, SI, 200 - SI)
    i <- match(rownames(KT), names(NC))
    KT$NC <- NC[i]
    KT$NR <- NR[i]
    KT$SI <- SI[i]
    KT$SI2 <- SI2[i]

    if (.c4i1$boot) {
        CB <- .c4i1$Curr.Boot
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

    rt <- raster(system.file("extdata/AB_1km_mask.tif", package="cure4insect"))
    cn <- c("NC", "NR", "SI", "SI2", "SE", "CV")
    rl <- lapply(cn, function(z) {
        r <- make_raster(KT[,z], rc=KT, rt=rt)
        r[r < 0] <- NA # sentinel values to NA
        r
    })
    names(rl) <- cn
    stack(rl)
}
