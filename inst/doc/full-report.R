## this is a full report skeleton to be used as template
#devtools::install_github("ABbiodiversity/cure4insect")
library(jsonlite)
library(cure4insect)
load_common_data()

SPP <- c("AlderFlycatcher", "Achillea.millefolium")

library(rgdal)
dsn <- system.file("extdata/OSA_bound.geojson", package="cure4insect")
ply <- readOGR(dsn=dsn)
ID <- overlay_polygon(ply)

#SPP <- get_all_species()
#ID <- get_all_id()

base <-"~/GoogleWork/abmi/c4i_experiments/_site"

subset_common_data(ID, SPP)
species <- SPP
id <- ID
#.c4if=cure4insect:::.c4if
#.c4is=cure4insect:::.c4is

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

f <- file(file.path(base, "species/specieslist.js"))
js1 <- c("var species = ", toJSON(sptab$display),
    "\n\nvar link = ", toJSON(rownames(sptab)))
cat(js1, file=f) # --> save this into specieslist.js
close(f)

## settings info
ver_info <- get_version_info()
sub_info <- get_subset_info()

f <- file(file.path(base, "settings/settings.js"))
cat("var selection =", toJSON(sub_info), file=f)
close(f)

toJSON(sub_info, "values", pretty=TRUE)
toJSON(ver_info, "values", pretty=TRUE)
#toJSON(ver_info[1,], "col", pretty=TRUE) # use this per species

## settings images
sub_map <- make_subset_map()
#svg("settings/images/selection.svg", height=9, width=6)
png(file.path(base, "settings/images/selection.png"),
    height=900*2, width=600*2, res=72*2, pointsize = 18)
op <- par(mar=c(0.5, 3, 1, 0))
plot(sub_map, legend=FALSE, axes=FALSE, box=FALSE,
    col=colorRampPalette(c("#e5f5f9", "#2ca25f"))(2))
legend("bottomleft", bty="n", title="Legend", fill=c("#e5f5f9", "#2ca25f"),
    legend=c("Not selected", "Selected"), border=NA)
par(op)
dev.off()

## single species

rt <- .read_raster_template()
rmask <- sub_map
rmask[rmask == 0] <- NA
rreg0 <- mask(rt, rmask)
rreg0 <- trim(rreg0, values = NA)
ar <- diff(bbox(rreg0)[2,])/diff(bbox(rreg0)[1,])

spp <- species[1]

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
rreg <- trim(rreg, values = NA)

col1 <- colorRampPalette(c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679',
    '#41ab5d','#238443','#006837','#004529'))(100)
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


png(file.path(base, "species", spp, "/images/map-nc.png"),
    width=1000, height=1000*ar, res=72*2)
op <- par(mar=c(0.5, 3, 1, 0))
plot(rreg0, col="#6baed6", axes=FALSE, box=FALSE, legend=FALSE)
plot(rreg[["NC"]], col=col1, add=TRUE)
par(op)
dev.off()

png(file.path(base, "species", spp, "/images/map-nr.png"),
    width=1000, height=1000*ar, res=72*2)
op <- par(mar=c(0.5, 3, 1, 0))
plot(rreg0, col="#6baed6", axes=FALSE, box=FALSE, legend=FALSE)
plot(rreg[["NR"]], col=col1, add=TRUE)
par(op)
dev.off()

png(file.path(base, "species", spp, "/images/map-se.png"),
    width=1000, height=1000*ar, res=72*2)
op <- par(mar=c(0.5, 3, 1, 0))
plot(rreg0, col="#6baed6", axes=FALSE, box=FALSE, legend=FALSE)
plot(rreg[["SE"]], col=col2, add=TRUE)
par(op)
dev.off()

png(file.path(base, "species", spp, "/images/map-cv.png"),
    width=1000, height=1000*ar, res=72*2)
op <- par(mar=c(0.5, 3, 1, 0))
plot(rreg0, col="#6baed6", axes=FALSE, box=FALSE, legend=FALSE)
plot(rreg[["CV"]], col=col2, add=TRUE)
par(op)
dev.off()

png(file.path(base, "species", spp, "/images/map-si.png"),
    width=1000, height=1000*ar, res=72*2)
op <- par(mar=c(0.5, 3, 1, 0))
plot(rreg0, col="#6baed6", axes=FALSE, box=FALSE, legend=FALSE)
plot(rreg[["SI"]], col=col3, add=TRUE)
par(op)
dev.off()

png(file.path(base, "species", spp, "/images/map-si2.png"),
    width=1000, height=1000*ar, res=72*2)
op <- par(mar=c(0.5, 3, 1, 0))
plot(rreg0, col="#6baed6", axes=FALSE, box=FALSE, legend=FALSE)
plot(rreg[["SI2"]], col=col4, add=TRUE)
par(op)
dev.off()

.plot_sector2(
            Curr=x$sector["Current",],
            Ref=x$sector["Reference",],
            RefTotal=x$intactness["Reference", 1],
            regional=FALSE, ylim=c(-100,100), main="Effects on population under footprint")

png(file.path(base, "species", spp, "/images/sector-regional.png"),
    width=1000, height=1000, res=72*2)
plot_sector(x, type="regional", main="Effects on regional population",
    ylim=c(-100, 100))
dev.off()

png(file.path(base, "species", spp, "/images/sector-underhf.png"),
    width=1000, height=1000, res=72*2)
plot_sector(x, type="underhf", "Effects on population under footprint",
    ylim=c(-100, 100))
dev.off()

png(file.path(base, "species", spp, "/images/sector-unit.png"),
    width=1000, height=1000, res=72*2)
plot_sector(x, type="unit", "Regional effects per unit area")
dev.off()


## 1.
## keep everything in zip
## unzip
## OK -- save js data files (settings, species)
## OK -- save images (settings)
## 2.
## use 1-spp template, create dir and /species/spp/index.html
## save images
## this should be done as part of the loop
## 3.
## finally calculate multi-species stats and richn/SI maps
## save images and data
## 4.
## zip everything
## ?? where to keep csv and raster objects ?? /data/ ?



# plots: check svg

# run all spp all pixels


## add pAspen and pWater to KT or make tif?

#devtools::install_github("ABbiodiversity/cure4insect")
library(cure4insect)

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
