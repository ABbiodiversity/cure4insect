## this is a full report skeleton to be used as template
#devtools::install_github("ABbiodiversity/cure4insect")
library(jsonlite)
library(cure4insect)
load_common_data()

#SPP <- c("AlderFlycatcher", "Achillea.millefolium")
SPP <- get_all_species()
SPP <- sample(SPP, 10)

library(rgdal)
dsn <- system.file("extdata/OSA_bound.geojson", package="cure4insect")
ply <- readOGR(dsn=dsn)
ID <- overlay_polygon(ply)

species <- SPP
id <- ID
#.c4if=cure4insect:::.c4if
#.c4is=cure4insect:::.c4is

base <-"~/GoogleWork/abmi/c4i_experiments/_testsite"


## ------------- starts here ---------

subset_common_data(id, species)

## copy the files

file.copy(from=file.path(system.file(package="cure4insect"), "site", "."),
    to=base, recursive=TRUE)
unlink(file.path(base, "species", "species.html"))

## species listing

sptab <- get_species_table()[get_subset_species(),,drop=FALSE]
sptab$Species <- as.character(sptab$Species)
sptab$CommonName <- as.character(sptab$CommonName)
sptab$ScientificName <- as.character(sptab$ScientificName)
sptab$tnice <- sptab$taxon
levels(sptab$tnice) <- c("Birds", "Lichens", "Mammals", "Soil Mites", "Bryophytes",
    "Vascular plants")
sptab$tnice <- as.character(sptab$tnice)
sptab$display <- ifelse(as.character(sptab$Species) ==
    as.character(sptab$ScientificName),
    paste0(sptab$ScientificName),
    paste0(sptab$CommonName, " (", sptab$ScientificName, ")"))
sptab$display <- as.character(sptab$display)
sptab <- sptab[order(sptab$display),]
species <- rownames(sptab)

f <- file(file.path(base, "species", "specieslist.js"))
js1 <- c("var species = ", toJSON(sptab$display),
    "\n\nvar link = ", toJSON(rownames(sptab)))
cat(js1, file=f) # --> save this into specieslist.js
close(f)

## settings info
ver_info <- get_version_info()
sub_info <- get_subset_info()
ver <- read.dcf(file=system.file("DESCRIPTION", package="cure4insect"),
    fields=c("Version", "Date"))
pkg_ver <- paste0("cure4insect version ", ver[1], " (", ver[2], ")")
sett <- list(species=unname(sub_info[1]), pixels=unname(sub_info[2]),
    rversion=R.Version()$version.string,
    version=pkg_ver, date=as.character(as.Date(Sys.time())))

f <- file(file.path(base, "settings", "settings.js"))
cat("var settings =", toJSON(sett, pretty=TRUE), file=f)
close(f)

## settings images
sub_map <- make_subset_map()
dir.create(file.path(base, "settings", "images"))
#svg("settings/images/selection.svg", height=9, width=6)
png(file.path(base, "settings", "images", "selection.png"),
    height=900*2, width=600*2, res=72*2, pointsize = 18)
op <- par(mar=c(0.5, 3, 1, 0))
plot(sub_map, legend=FALSE, axes=FALSE, box=FALSE,
    col=colorRampPalette(c("#e5f5f9", "#2ca25f"))(2))
legend("bottomleft", bty="n", title="Legend", fill=c("#e5f5f9", "#2ca25f"),
    legend=c("Not selected", "Selected"), border=NA)
par(op)
dev.off()

## single species setup

rt <- .read_raster_template()
rmask <- sub_map
rmask[rmask == 0] <- NA
rreg0 <- mask(rt, rmask)
rreg0 <- trim(rreg0, values = NA)
ar <- diff(bbox(rreg0)[2,])/diff(bbox(rreg0)[1,])

resx <- list() # store list results
res <- list() # store flat results
r_si <- NULL # SI map
r_ri <- NULL # richness map
KEEP <- rep(TRUE, length(species))


for (i in seq_along(species)) {

    cat("* ", i, "/", length(species), " ", species[i], "\n", sep="")
    flush.console()

    spp <- species[i]

    dir.create(file.path(base, "species", spp), showWarnings=FALSE)
    dir.create(file.path(base, "species", spp, "images"), showWarnings=FALSE)
    file.copy(from=file.path(system.file(package="cure4insect"), "site", "species", "species.html"),
        to=file.path(base, "species", spp, "index.html"))

    y <- load_species_data(spp)
    x <- calculate_results(y)

    z <- flatten(x)
    z$tnice <- switch(as.character(z$Taxon),
        "birds"="Birds",
        "lichens"="Lichens",
        "mammals"="Mammals",
        "mites"="Soil Mites",
        "mosses"="Bryophytes",
        "vplants"="Vascular plants")
    z$display <- ifelse(is.na(z$CommonName),
        paste0(z$ScientificName),
        paste0(z$CommonName, " (", z$ScientificName, ")"))
    f <- file(file.path(base, "species", spp, "data.js"))
    cat("var data =", toJSON(z, pretty=TRUE, digits=2), file=f)
    close(f)

    r <- rasterize_results(y)
    rreg <- mask(r, rmask)
    #rreg <- trim(rreg, values = NA)
    rreg <- crop(rreg, extent(rreg0))

    col1 <- colorRampPalette(c('#ffffe5','#fff7bc','#fee391','#fec44f','#fe9929',
        '#ec7014','#cc4c02','#993404','#662506'))(100)
    col2 <- colorRampPalette(c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1',
        '#dd3497','#ae017e','#7a0177','#49006a'))(100)
    col3 <- colorRampPalette(c('#762a83','#9970ab','#c2a5cf','#e7d4e8','#f7f7f7',
        '#d9f0d3','#a6dba0','#5aae61','#1b7837'))(101)
    SIrange <- pmax(0, pmin(100, round(range(values(rreg[["SI"]]),na.rm=TRUE)))) + 1
    col3 <- col3[SIrange[1]:SIrange[2]]
    col4 <- colorRampPalette(c('#810f7c', '#ffffd4', '#006d2c'))(201)
    SI2range <- pmax(0, pmin(200, round(range(values(rreg[["SI2"]]),na.rm=TRUE)))) + 1
    col4 <- col4[SI2range[1]:SI2range[2]]
    Nmax1 <- max(values(rreg[["NC"]]),na.rm=TRUE)
    Nmax0 <- max(values(rreg[["NR"]]),na.rm=TRUE)
    Nmax <- max(Nmax1, Nmax0)
    values(rreg[["NC"]])[which(values(rreg[["NC"]]) == Nmax1)] <- Nmax
    values(rreg[["NR"]])[which(values(rreg[["NR"]]) == Nmax0)] <- Nmax


    png(file.path(base, "species", spp, "images", "map-nc.png"),
        width=1000, height=1000*ar, res=72*2)
    op <- par(mar=c(0.5, 3, 1, 0))
    plot(rreg0, col="#6baed6", axes=FALSE, box=FALSE, legend=FALSE)
    plot(rreg[["NC"]], col=col1, add=TRUE)
    par(op)
    dev.off()

    png(file.path(base, "species", spp, "images", "map-nr.png"),
        width=1000, height=1000*ar, res=72*2)
    op <- par(mar=c(0.5, 3, 1, 0))
    plot(rreg0, col="#6baed6", axes=FALSE, box=FALSE, legend=FALSE)
    plot(rreg[["NR"]], col=col1, add=TRUE)
    par(op)
    dev.off()

    png(file.path(base, "species", spp, "images", "map-se.png"),
        width=1000, height=1000*ar, res=72*2)
    op <- par(mar=c(0.5, 3, 1, 0))
    plot(rreg0, col="#6baed6", axes=FALSE, box=FALSE, legend=FALSE)
    plot(rreg[["SE"]], col=col2, add=TRUE)
    par(op)
    dev.off()

    png(file.path(base, "species", spp, "images", "map-cv.png"),
        width=1000, height=1000*ar, res=72*2)
    op <- par(mar=c(0.5, 3, 1, 0))
    plot(rreg0, col="#6baed6", axes=FALSE, box=FALSE, legend=FALSE)
    plot(rreg[["CV"]], col=col2, add=TRUE)
    par(op)
    dev.off()

    png(file.path(base, "species", spp, "images", "map-si.png"),
        width=1000, height=1000*ar, res=72*2)
    op <- par(mar=c(0.5, 3, 1, 0))
    plot(rreg0, col="#6baed6", axes=FALSE, box=FALSE, legend=FALSE)
    plot(rreg[["SI"]], col=col3, add=TRUE)
    par(op)
    dev.off()

    png(file.path(base, "species", spp, "images", "map-si2.png"),
        width=1000, height=1000*ar, res=72*2)
    op <- par(mar=c(0.5, 3, 1, 0))
    plot(rreg0, col="#6baed6", axes=FALSE, box=FALSE, legend=FALSE)
    plot(rreg[["SI2"]], col=col4, add=TRUE)
    par(op)
    dev.off()

    png(file.path(base, "species", spp, "images", "sector-regional.png"),
        width=1000, height=1000, res=72*2)
    plot_sector(x, type="regional", main="Effects on regional population",
        ylim=c(-100, 100))
    dev.off()

    png(file.path(base, "species", spp, "images", "sector-underhf.png"),
        width=1000, height=1000, res=72*2)
    plot_sector(x, type="underhf", "Effects on population under footprint",
        ylim=c(-100, 100))
    dev.off()

    png(file.path(base, "species", spp, "images", "sector-unit.png"),
        width=1000, height=1000, res=72*2)
    plot_sector(x, type="unit", "Regional effects per unit area")
    dev.off()

    ## storing results

    resx[[spp]] <- x
    res[[spp]] <- z
    KEEP[i] <- z$Keep

    if (is.null(r_si)) {
        r_si <- rreg[["SI"]]
        r_si[is.na(r_si)] <- 100
        if (!z$Keep)
            r_si[!is.na(values(r_si))] <- 0
    } else {
        tmp <- rreg[["SI"]]
        tmp[is.na(tmp)] <- 100
        if (z$Keep)
            r_si <- r_si + tmp
    }

    if (is.null(r_ri)) {
        r_ri <- rreg[["NC"]]
        r_ri[is.na(r_ri)] <- 0
        if (z$Taxon=="birds")
            r_ri <- 1-exp(-1*r_ri)
    } else {
        tmp2 <- rreg[["NC"]]
        tmp2[is.na(tmp2)] <- 0
        if (z$Taxon=="birds")
            tmp2 <- 1-exp(-1*tmp2)
        r_ri <- r_ri + tmp2
    }


}

r_si <- r_si / sum(KEEP)
r_si <- mask(r_si, rreg0)
r_ri <- mask(r_ri, rreg0)
rr <- stack(list(Intactness=r_si, Richness=r_ri))
dir.create(file.path(base, "data"), showWarnings=FALSE)

writeRaster(rr, file.path(base, "data", "multispecies_results.tif"))

zz <- do.call(rbind, res)
class(zz) <- c("c4idf", class(z))
rept <- list(species=unname(sub_info[1]), pixels=unname(sub_info[2]),
    intactness=mean(zz$SI_Est))
write.csv(zz, row.names=FALSE, file=file.path(base, "data", "species_results.csv"))
sel_tab <- data.frame(ID=get_subset_id())
write.csv(sel_tab, row.names=FALSE, file=file.path(base, "data", "spatial_subset_id.csv"))

f3 <- file(file.path(base, "report", "results.js"))
cat("var results =", toJSON(rept, pretty=TRUE), file=f3)
close(f3)

dir.create(file.path(base, "report", "images"), showWarnings=FALSE)

col3 <- colorRampPalette(c('#762a83','#9970ab','#c2a5cf','#e7d4e8','#f7f7f7',
    '#d9f0d3','#a6dba0','#5aae61','#1b7837'))(101)
SIrange <- pmax(0, pmin(100, round(range(values(r_si),na.rm=TRUE)))) + 1
col3 <- col3[SIrange[1]:SIrange[2]]
png(file.path(base, "report", "images", "multi-map-intactness.png"),
    width=1000, height=1000*ar, res=72*2)
op <- par(mar=c(0.5, 3, 1, 0))
plot(rreg0, col="#6baed6", axes=FALSE, box=FALSE, legend=FALSE)
plot(r_si, col=col3, add=TRUE)
par(op)
dev.off()

col5 <- colorRampPalette(c('#fff7fb','#ece2f0','#d0d1e6','#a6bddb','#67a9cf',
    '#3690c0','#02818a','#016c59','#014636'))(100)
png(file.path(base, "report", "images", "multi-map-richness.png"),
    width=1000, height=1000*ar, res=72*2)
op <- par(mar=c(0.5, 3, 1, 0))
plot(rreg0, col="#6baed6", axes=FALSE, box=FALSE, legend=FALSE)
plot(r_ri, col=col5, add=TRUE)
par(op)
dev.off()

png(file.path(base, "report", "images", "multi-sector-total.png"),
    width=1000, height=1000, res=72*2)
plot_sector(zz, type="regional", main="Effects on regional population",
    ylim=c(-100, 100))
dev.off()

png(file.path(base, "report", "images", "multi-sector-underhf.png"),
    width=1000, height=1000, res=72*2)
plot_sector(zz, type="underhf", "Effects on population under footprint",
    ylim=c(-100, 100))
dev.off()

png(file.path(base, "report", "images", "multi-sector-unit.png"),
    width=1000, height=1000, res=72*2)
plot_sector(zz, type="unit", "Regional effects per unit area")
dev.off()



## 1.
## keep everything in zip
## unzip
## OK -- save js data files (settings, species)
## OK -- save images (settings)
## 2.
## OK -- use 1-spp template, create dir and /species/spp/index.html
## OK -- save images
## OK -- this should be done as part of the loop
## 3.
## OK -- finally calculate multi-species stats and richn/SI maps
## OK -- save images and data
## 4.
## zip everything
## ?? where to keep csv and raster objects ?? /data/ ?
## OK -- save spatial ID list as well


