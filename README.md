# cure4insect

> Custom Reporting for Intactness and Sector Effects

[![Linux build status](https://travis-ci.org/ABbiodiversity/cure4insect.svg?branch=master)](https://travis-ci.org/ABbiodiversity/cure4insect)

## Install

```R
devtools::install_github("ABbiodiversity/cure4insect")
```

## Examples

Load the package 1st:

```R
library(cure4insect)
```

#### Workflow with 1 species

`id` is a vector of Row_Col IDs of 1km pixels,
`species` is a vector if species IDs:

```R
load_common_data()

## here is how to inspect all possible spatial and species IDs
str(get_all_id())
str(get_all_species())
plot(xy <- get_id_locations(), pch=".")
summary(xy)
str(get_species_table())

## define spatial and species IDs
Spp <- "Ovenbird"
ID <- c("182_362", "182_363", "182_364", "182_365", "182_366", "182_367",
    "182_368", "182_369", "182_370", "182_371", "182_372")

subset_common_data(id=ID, species=Spp)
y <- load_species_data("Ovenbird")

## calculate results and flatten to a 1-liner
x <- calculate_results(y)
x
flatten(x)
```

#### Workflow with multiple species

`id` and `species` can be defined using text files:

```R
load_common_data()
Spp <- read.table(system.file("extdata/species.txt", package="cure4insect"))
ID <- read.table(system.file("extdata/pixels.txt", package="cure4insect"))
subset_common_data(id=ID, species=Spp)
xx <- report_all()
str(xx)
do.call(rbind, lapply(xx, flatten))
```

Wrapper function:

* `species="all"` runs all species
* `species="mites"` runs all mite species
* `sender="you@example.org"` will send an email with the results attached
* increase `cores` to allow parallel processing

```R
z <- custom_report(id=ID,
    species=c("AlderFlycatcher", "Achillea.millefolium"),
    address=NULL, cores=1)
z
```

Working with a local copy of the results is much faster
set path via function arguments or the options:

```R
## making of the file raw_all.rda
library(cure4insect)
opar <- set_options(path = "w:/reports")
getOption("cure4insect")
load_common_data()
subset_common_data(id=get_all_id(),
    species=get_all_species())
## see how these compare
system.time(res <- report_all(cores=1))
#system.time(res <- report_all(cores=2))
#system.time(res <- report_all(cores=4))
## this is for testing only
#system.time(res <- .report_all_by1())
(set_options(opar)) # reset options
```

A few more words about options:

```R
## options
getOption("cure4insect")
## change configs in this file to make it permanent for a given installation
as.list(drop(read.dcf(file=system.file("config/defaults.conf",
package="cure4insect"))))
```

#### Sector effects plots

```R
## *res*ults from calculate_results, all province, all species
load(system.file("extdata/raw_all.rda", package="cure4insect"))

sector_plot(res[["CanadaWarbler"]], "unit")
sector_plot(res[["CanadaWarbler"]], "regional")
sector_plot(res[["CanadaWarbler"]], "underhf")

z <- do.call(rbind, lapply(res, flatten))
class(z) <- c("c4idf", class(z))
sector_plot(z, "unit") # all species
sector_plot(z[1:100,], "regional") # use a subset
sector_plot(z, "underhf", method="hist") # binned version
```

#### Determining spatial IDs based on spatial polygons

`id` can also be a SpatialPolygons object based on GeoJSON for example:

```R
library(rgdal)
dsn <- system.file("extdata/polygon.geojson", package="cure4insect")
ply <- readOGR(dsn=dsn)
subset_common_data(id=ply, species=Spp)
xx2 <- report_all()
```

Spatial IDs of the 1km x 1km spatial pixel units are to be used for the custom summaries.
The Row_Col field defines the IDs and links the raster cells in the [geodatabase](http://ftp.public.abmi.ca/species.abmi.ca/gis/Grid1km_working.gdb.zip)
or [CSV](http://ftp.public.abmi.ca/species.abmi.ca/gis/Grid1km_working.csv.zip}) (with latitude/longitude in [NAD_1983_10TM_AEP_Forest](http://spatialreference.org/ref/epsg/3402/) projection).

For the web application, use your favourite GIS software, or in R use this:

```R
library(rgdal)
load_common_data()
dsn <- system.file("extdata/OSA_bound.geojson", package="cure4insect")
ply <- readOGR(dsn=dsn)
ID <- overlay_polygon(ply)
## write IDs into a text file
write.table(data.frame(SpatialID=ID), row.names=FALSE, file="SpatialID.txt")

## spatial pixels: selection in red
xy <- get_id_locations()
plot(xy, col="grey", pch=".")
plot(xy[ID,], col="red", pch=".", add=TRUE)

## compare with the polygons
AB <- readOGR(dsn=system.file("extdata/AB_bound.geojson",
    package="cure4insect"))
plot(AB, col="grey")
plot(ply, col="red", add=TRUE)
```

#### Raster objects and maps

The result is a raster stack object with the following layers:

* NC, NR: current and reference abundance,
* SI, SI2: one- and two-sided intactness,
* SE, CV: bootstrap based standard error and coefficient of variation
estimates for current abundance.

```R
load_common_data()
y <- load_species_data("Ovenbird")
r <- rasterize_results(y)
plot(r, "NC") # current abundance map
plot(r, "SE") # standadr errors for current abundance
```

#### Spatially explicit (polygon level) predictions

```R
load_common_data()
## see bird species codes
sptab <- get_species_table()
rownames(sptab)[sptab$taxon == "birds"]
## pick Ovenbird
species <- "Ovenbird"
object <- load_spclim_data(species)

## vegetation/disturbance classes: use as factor
## might need to make a crosswalk, use e.g. mefa4::reclass
(veg <- as.factor(get_levels()$veg))

## for each veg class value, need to have
## spatial locations (can repeat the same value,
## but avoid duplicate rownames)
## use the sp package to get SpatialPoints as here:
XY <- get_id_locations()
coords <- coordinates(XY)[10^5,,drop=FALSE]
rownames(coords) <- NULL
xy <- data.frame(coords[rep(1, length(veg)),])
coordinates(xy) <- ~ POINT_X + POINT_Y
proj4string(xy) <- proj4string(XY)

## predict
pred <- predict(object, xy=xy, veg=veg)
summary(pred)
```

## Web API

The web app sits [here](http://sc-dev.abmi.ca/ocpu/apps/ABbiodiversity/cure4insect/www/).
To get more control over the results, use the [API](https://www.opencpu.org/api.html#api-formats).
For example:

```shell
curl http://sc-dev.abmi.ca/ocpu/library/cure4insect/R/custom_report/csv \
-H "Content-Type: application/json" -d \
'{"id":["182_362", "182_363"], "species":["AlderFlycatcher", "Achillea.millefolium"]}'
```


## Vision

The package can be used to use a local copy of the organized
data and check the results, bulk produce png/pdf figures:
everything that relates to predictions.

Create a Docker container (instead of zip download)
with all the data, so that
it can be deployed as a micro-service (moving things around),
or use locally through Kitematic
(faster to download only once and then use the local copy).

#### Todo

* attachment to include: metadata, readme, IDs, batch figures (zipped)
* make containerized version for local use
* define color schemes and plotting functionality with default thresholds and some legends?
* habitat association and linear footprint figure reproduction
