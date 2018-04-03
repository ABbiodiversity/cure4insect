## this is a full report skeleton to be used as template
#devtools::install_github("ABbiodiversity/cure4insect")

## load libraries
library(mefa4)
library(jsonlite)
library(cure4insect)
#opar <- set_options(path = "w:/reports")

## load common data
load_common_data()

## specify spatial subset
## -- use OSA bound
#library(rgdal)
#dsn <- system.file("extdata/OSA_bound.geojson", package="cure4insect")
#ply <- readOGR(dsn=dsn)
## -- use a simple polygon boundary
xy <- matrix(c(
    -117.791807, 57.159552,
    -113.617002, 56.279422,
    -115.748350, 55.228470,
    -114.408018, 54.200443,
    -118.648740, 54.825449,
    -117.791807, 57.159552), ncol=2, byrow=TRUE)
ply <- SpatialPolygons(list(Polygons(list(Polygon(xy)), "x")), 1L)
proj4string(ply) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
id <- overlay_polygon(ply)
#id <- get_all_id(mregion="north")

## specify species subset
#species <- c("AlderFlycatcher", "Achillea.millefolium") # 2 species
#species <- get_all_species("birds") # all birds
set.seed(234);species <- sample(get_all_species(), 10) # 10 random species
## 33 OF birds
#species <- c("BaltimoreOriole", "BaybreastedWarbler", "BlackpollWarbler",
#    "BlackthroatedGreenWarbler", "BlueheadedVireo", "BorealChickadee",
#    "BrownCreeper", "CanadaWarbler", "CapeMayWarbler", "EveningGrosbeak",
#    "GoldencrownedKinglet", "HairyWoodpecker", "LeastFlycatcher",
#    "MagnoliaWarbler", "NorthernWaterthrush", "PhiladelphiaVireo",
#    "PileatedWoodpecker", "PineSiskin", "PurpleFinch", "RedbreastedNuthatch",
#    "RedCrossbill", "RosebreastedGrosbeak", "RubycrownedKinglet",
#    "SwainsonsThrush", "VariedThrush", "WarblingVireo", "WesternTanager",
#    "WesternWoodPewee", "WhitebreastedNuthatch", "WhitewingedCrossbill",
#    "WinterWren", "YellowbelliedSapsucker", "YellowrumpedWarbler")

## FALSE --> no single species maps/plots are saved
save_spp_figs <- TRUE
## setting limit
limit <- 0.01

## subregions to evaluate in/out for species based on limit
TAB <- get_id_table()
regs <- interaction(TAB$reg_luf, TAB$reg_nsr, sep="_", drop=TRUE)
#regs <- as.factor(rep("ALL", nrow(TAB))) # use this if no subregions defined
#regs <- TAB$reg_nsr

## specify output folder
base <- "./_site" # this folder is created
zipbase <- "." # put here the zipped results
if (.verbose()) cat("output directory", base, "\n")
if (dir.exists(base))
    unlink(base, recursive=TRUE)
dir.create(base)

## apply subsets
subset_common_data(id, species)
get_subset_info()

str(id <- get_subset_id())
str(species <- get_subset_species())

## specify resolution factor (>0)
resol <- 2
## bg color (water, NA)
BG <- "#41526d" # dark
#BG <- "#6baed6" # light

## copy the files
if (.verbose()) cat("copying files\n")
file.copy(from=file.path(system.file(package="cure4insect"), "site", "."),
    to=base, recursive=TRUE)
unlink(file.path(base, "species", "species.html"))

## species listing
if (.verbose()) cat("writing species listings\n")
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
if (.verbose()) cat("writing info\n")
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

## subset map
if (.verbose()) cat("making subset map\n")
sub_map <- make_subset_map()
dir.create(file.path(base, "settings", "images"))
png(file.path(base, "settings", "images", "selection.png"),
    height=900*2, width=600*2, res=72*resol, pointsize = 18)
op <- par(mar=c(0.5, 3, 1, 0))
plot(sub_map, legend=FALSE, axes=FALSE, box=FALSE,
    col=colorRampPalette(c("#e5f5f9", "#2ca25f"))(2))
legend("bottomleft", bty="n", title="Legend", fill=c("#e5f5f9", "#2ca25f"),
    legend=c("Not selected", "Selected"), border=NA)
par(op)
dev.off()

## single species setup
if (.verbose()) cat("spatial mask and output setup\n")
rt <- .read_raster_template()
rmask <- sub_map
rmask[rmask == 0] <- NA
rreg0 <- mask(rt, rmask)
rreg0 <- trim(rreg0, values = NA)
ar <- diff(bbox(rreg0)[2,])/diff(bbox(rreg0)[1,])

## empty objects to store the results
resx <- list() # store list results
res <- list() # store flat results
r_si <- NULL # SI map
r_ri <- NULL # richness map
KEEP <- rep(TRUE, length(species))

## loop over species
if (.verbose()) cat("loop over species\n")
for (i in seq_along(species)) {
    spp <- species[i]
    if (.verbose())
        cat("* ", i, "/", length(species), " ", spp, "\n", sep="")
    flush.console()
    ## species folder
    if (.verbose()) cat("\t- setting up species folder\n"); flush.console()
    dir.create(file.path(base, "species", spp), showWarnings=FALSE)
    dir.create(file.path(base, "species", spp, "images"), showWarnings=FALSE)
    file.copy(from=file.path(system.file(package="cure4insect"), "site", "species", "species.html"),
        to=file.path(base, "species", spp, "index.html"))
    ## load species data
    if (.verbose()) cat("\t- loading species data\n"); flush.console()
    y <- load_species_data(spp)
    ## intactness & sector effects
    if (.verbose()) cat("\t- calculating species summaries\n"); flush.console()
    x <- calculate_results(y)
    ## flatten results and write js object
    z <- flatten(x, limit=limit)
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
    ## rasterize species data
    if (.verbose()) cat("\t- rasterizing species data\n"); flush.console()
    r <- rasterize_results(y)
    rreg <- mask(r, rmask)
    rreg <- crop(rreg, extent(rreg0))
    ## color settings
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

    ## save images
    if (save_spp_figs) {

        if (.verbose()) cat("\t- saving species images\n"); flush.console()
        png(file.path(base, "species", spp, "images", "map-nc.png"),
            width=1000, height=1000*ar, res=72*resol)
        op <- par(mar=c(0.5, 3, 1, 0))
        plot(rreg0, col=BG, axes=FALSE, box=FALSE, legend=FALSE)
        plot(rreg[["NC"]], col=col1, add=TRUE)
        par(op)
        dev.off()

        png(file.path(base, "species", spp, "images", "map-nr.png"),
            width=1000, height=1000*ar, res=72*resol)
        op <- par(mar=c(0.5, 3, 1, 0))
        plot(rreg0, col=BG, axes=FALSE, box=FALSE, legend=FALSE)
        plot(rreg[["NR"]], col=col1, add=TRUE)
        par(op)
        dev.off()

        png(file.path(base, "species", spp, "images", "map-se.png"),
            width=1000, height=1000*ar, res=72*resol)
        op <- par(mar=c(0.5, 3, 1, 0))
        plot(rreg0, col=BG, axes=FALSE, box=FALSE, legend=FALSE)
        plot(rreg[["SE"]], col=col2, add=TRUE)
        par(op)
        dev.off()

        png(file.path(base, "species", spp, "images", "map-cv.png"),
            width=1000, height=1000*ar, res=72*resol)
        op <- par(mar=c(0.5, 3, 1, 0))
        plot(rreg0, col=BG, axes=FALSE, box=FALSE, legend=FALSE)
        plot(rreg[["CV"]], col=col2, add=TRUE)
        par(op)
        dev.off()

        png(file.path(base, "species", spp, "images", "map-si.png"),
            width=1000, height=1000*ar, res=72*resol)
        op <- par(mar=c(0.5, 3, 1, 0))
        plot(rreg0, col=BG, axes=FALSE, box=FALSE, legend=FALSE)
        plot(rreg[["SI"]], col=col3, add=TRUE)
        par(op)
        dev.off()

        png(file.path(base, "species", spp, "images", "map-si2.png"),
            width=1000, height=1000*ar, res=72*resol)
        op <- par(mar=c(0.5, 3, 1, 0))
        plot(rreg0, col=BG, axes=FALSE, box=FALSE, legend=FALSE)
        plot(rreg[["SI2"]], col=col4, add=TRUE)
        par(op)
        dev.off()

        png(file.path(base, "species", spp, "images", "sector-regional.png"),
            width=1000, height=1000, res=72*resol)
        plot_sector(x, type="regional", main="Effects on regional population",
            ylim=c(-100, 100))
        dev.off()

        png(file.path(base, "species", spp, "images", "sector-underhf.png"),
            width=1000, height=1000, res=72*resol)
        plot_sector(x, type="underhf", "Effects on population under footprint",
            ylim=c(-100, 100))
        dev.off()

        png(file.path(base, "species", spp, "images", "sector-unit.png"),
            width=1000, height=1000, res=72*resol)
        plot_sector(x, type="unit", "Regional effects per unit area")
        dev.off()

    }

    ## storing results
    if (.verbose()) cat("\t- finishing results\n"); flush.console()
    resx[[spp]] <- x
    res[[spp]] <- z
    KEEP[i] <- z$Keep

    ## use object regs to evaluate in/out by smaller regions
    DAT <- cbind(NR=rowSums(y$SA.Ref), NC=rowSums(y$SA.Curr))
    DAT <- DAT[match(rownames(TAB), rownames(DAT)),]
    DAT[is.na(DAT)] <- 0
    MEAN <- apply(groupMeans(DAT, 1, regs), 1, max)
    IO <- ifelse(MEAN >= x$max * limit, 1, 0)
    IO <- IO[match(regs, names(IO))]
    rio <- .make_raster(IO, TAB, rt)
    #rio <- mask(rio, rmask)
    rio <- crop(rio, extent(rreg0))

    ## finishing intactness raster
    if (is.null(r_si)) {
        r_si <- rreg[["SI"]]
        r_si[is.na(r_si)] <- 100
        if (!z$Keep)
            r_si[!is.na(values(r_si))] <- 0
        r_si <- r_si * rio
        IOsum <- rio
    } else {
        tmp <- rreg[["SI"]]
        tmp[is.na(tmp)] <- 100
        tmp <- tmp * rio
        if (z$Keep) {
            r_si <- r_si + tmp
            IOsum <- IOsum + rio
        }
    }
    ## finishing richness raster
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
## multi-species rasters
if (.verbose()) cat("multi-species rasters\n")
#r_si <- r_si / sum(KEEP)
r_si <- r_si / IOsum
r_si <- mask(r_si, rreg0)
r_ri <- mask(r_ri, rreg0)

rr <- stack(list(Intactness=r_si, Richness=r_ri, SppInSI=IOsum))
## write raster data as geo tif
dir.create(file.path(base, "data"), showWarnings=FALSE)
writeRaster(rr, file.path(base, "data", "multispecies_results.tif"))
## finalize and save species table and report details
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

## save muti species images
if (.verbose()) cat("multi-species images\n")
dir.create(file.path(base, "report", "images"), showWarnings=FALSE)

col3 <- colorRampPalette(c('#762a83','#9970ab','#c2a5cf','#e7d4e8','#f7f7f7',
    '#d9f0d3','#a6dba0','#5aae61','#1b7837'))(101)
SIrange <- pmax(0, pmin(100, round(range(values(r_si),na.rm=TRUE)))) + 1
col3 <- col3[SIrange[1]:SIrange[2]]
png(file.path(base, "report", "images", "multi-map-intactness.png"),
    width=1000, height=1000*ar, res=72*resol)
op <- par(mar=c(0.5, 3, 1, 0))
plot(rreg0, col=BG, axes=FALSE, box=FALSE, legend=FALSE)
plot(r_si, col=col3, add=TRUE)
par(op)
dev.off()

col5 <- colorRampPalette(c('#fff7fb','#ece2f0','#d0d1e6','#a6bddb','#67a9cf',
    '#3690c0','#02818a','#016c59','#014636'))(100)
png(file.path(base, "report", "images", "multi-map-richness.png"),
    width=1000, height=1000*ar, res=72*resol)
op <- par(mar=c(0.5, 3, 1, 0))
plot(rreg0, col=BG, axes=FALSE, box=FALSE, legend=FALSE)
plot(r_ri, col=col5, add=TRUE)
par(op)
dev.off()

png(file.path(base, "report", "images", "multi-sector-total.png"),
    width=1000, height=1000, res=72*resol)
plot_sector(zz, type="regional", main="Effects on regional population",
    ylim=c(-100, 100))
dev.off()

png(file.path(base, "report", "images", "multi-sector-underhf.png"),
    width=1000, height=1000, res=72*resol)
plot_sector(zz, type="underhf", "Effects on population under footprint",
    ylim=c(-100, 100))
dev.off()

png(file.path(base, "report", "images", "multi-sector-unit.png"),
    width=1000, height=1000, res=72*resol)
plot_sector(zz, type="unit", "Regional effects per unit area")
dev.off()

## zip the output & clean up
#zip(file.path(zipbase, "_site.zip"), file.path(base))
#unlink(base, recursive=TRUE)

## done
if (.verbose()) cat("done\n\n")


