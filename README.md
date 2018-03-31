# cure4insect

> Custom Reporting for Intactness and Sector Effects

[![Linux build status](https://travis-ci.org/ABbiodiversity/cure4insect.svg?branch=master)](https://travis-ci.org/ABbiodiversity/cure4insect)

The [R](https://www.r-project.org/) package is a decision support tool
that provides an interface to enable
custom reporting for intactness and sector effects
based on estimates and predictions created by the [Alberta
Biodiversity Monitoring Institute (ABMI)](http://abmi.ca/)
in collaboration with the
[Boreal Avian Modelling (BAM) Project](http://www.borealbirds.ca/).

* [Example species report](https://abbiodiversity.github.io/cure4insect/example-species-report.html)
* [Custom report](https://abbiodiversity.github.io/cure4insect/site/)
* [Web app](http://sc-dev.abmi.ca/ocpu/apps/ABbiodiversity/cure4insect/www/)

## License

The estimates, predictions, and related documentation are &copy; ABMI (2014&ndash;2018) under a [CC BY-SA 4.0 license](http://creativecommons.org/licenses/by-sa/4.0/).

The R package itself is licensed under [MIT license](https://github.com/ABbiodiversity/cure4insect/blob/master/LICENSE.md) &copy; 2018 Peter Solymos & ABMI.

## Install

Only GitHub version available now. If you have trouble installing the package,
please file an [issue](https://github.com/ABbiodiversity/cure4insect/issues).

```R
devtools::install_github("ABbiodiversity/cure4insect")
```

## Usage

Load the package:

```R
library(cure4insect)
```

#### Workflow with 1 species

`id` is a vector of Row_Col IDs of 1km pixels,
`species` is a vector if species IDs:

```R
load_common_data()

## define spatial and species IDs (subsets)
Spp <- "Ovenbird"
ID <- c("182_362", "182_363", "182_364", "182_365", "182_366", "182_367",
    "182_368", "182_369", "182_370", "182_371", "182_372")

subset_common_data(id=ID, species=Spp)
## check subsets
str(get_subset_id())
str(get_subset_species())

## load species data
y <- load_species_data(Spp)

## calculate results and flatten to a 1-liner
x <- calculate_results(y)
x
flatten(x)
```

#### Spatial subset specifications

Here is how to inspect all possible spatial IDs:

```R
str(get_all_id())
plot(xy <- get_id_locations(), pch=".")
summary(xy)
```

Spatial `id` can be specified as planning/management region:

```R
## Natural Regions
ID <- get_all_id(nr=c("Boreal", "Foothills"))
## Natural Subregions
ID <- get_all_id(nsr="Lower Boreal Highlands")
## Land Use Framework regions
ID <- get_all_id(luf="North Saskatchewan")
```

Alternatively, `id` can refer to quarter sections
using the `"MER-RGE-TWP-SEC-QS"` format:

```R
Spp <- "Ovenbird"
QSID <- c("4-12-1-2-SE", "4-12-1-2-SW", "4-12-1-3-SE", "4-12-1-3-SW")
qs2km(QSID) # corresponding Row_Col IDs
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

#### Species subset specifications

Here is how to inspect all possible species IDs

```R
str(get_all_species())
str(get_species_table())
```

Select one or more taxonomic group
(mammals, birds, mites, mosses, lichens, vpalnst), 
and fiter for habitat and status:

```R
## birds and mammals
str(get_all_species(taxon=c("birds", "mammals")))
## all upland species
str(get_all_species(taxon="all", habitat="upland"))
## nonnative vascular plants
str(get_all_species(taxon="vplants", status="nonnative"))
```

#### Wrapper functions

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

plot_sector(res[["CanadaWarbler"]], "unit")
plot_sector(res[["CanadaWarbler"]], "regional")
plot_sector(res[["CanadaWarbler"]], "underhf")

z <- do.call(rbind, lapply(res, flatten))
class(z) <- c("c4idf", class(z))
plot_sector(z, "unit") # all species
plot_sector(z[1:100,], "regional") # use a subset
plot_sector(z, "underhf", method="hist") # binned version
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

Use the `make_subset_map()` function to get a raster map of
the spatial selection.

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

It is possible to make multi-species maps as well:
average intactness and expected number of species.

```R
subset_common_data(species=get_all_species(taxon="birds"))
r1 <- make_multispecies_map("richness")
r2 <- make_multispecies_map("intactness")
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

Using composition data in spatial grids as input:

```R
xy <- xy[1:10,]
## unrealistic data set for illustration
mveg <- matrix(0, 10, 6)
colnames(mveg) <- veg[c(1:6 * 10)]
mveg[] <- rpois(60, 10) * rbinom(60, 1, 0.2)
mveg[rowSums(mveg)==0,1] <- 1 # avoid 0 row sum
msoil <- matrix(0, 10, 6)
colnames(msoil) <- get_levels()$soil[1:6]
msoil[] <- rpois(60, 10) * rbinom(60, 1, 0.4)
msoil[rowSums(msoil)==0,1] <- 1 # avoid 0 row sum

## output matrics are abundances
prmat1 <- predict_mat(object, xy, mveg, msoil)

## mean abundance per spatial unit
prmat2 <- predict_mat(object, xy, mveg/rowSums(mveg), msoil/rowSums(msoil))
```

Combining vegetation and soil based predictions:

```R
combine_veg_soil(xy, rowSums(prmat2$veg), rowSums(prmat2$soil))
```

#### Visualize land cover associations

See the following [R markdown](http://rmarkdown.rstudio.com/)
file for a worked example of visualizations available in the package:

```R
file.show(system.file("doc/example-species-report.Rmd", package="cure4insect"))
```

Habitat associations as shown on the [species.abmi.ca](http://species.abmi.ca/) website:

```R
load_common_data()
plot_abundance("Achillea.millefolium", "veg_coef")
plot_abundance("Achillea.millefolium", "soil_coef")
plot_abundance("Achillea.millefolium", "veg_lin")
plot_abundance("Achillea.millefolium", "soil_lin")
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

## Explore single and multi-species results

To get similar output to 
[this](https://abbiodiversity.github.io/cure4insect/site/), 
run script from this file:

```R
file.show(system.file("doc/custom-report.R", package="cure4insect"))
```

