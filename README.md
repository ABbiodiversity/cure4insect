# cure4insect

> Custom Reporting for Intactness and Sector Effects

[![Linux build status](https://travis-ci.org/ABbiodiversity/cure4insect.svg?branch=master)](https://travis-ci.org/ABbiodiversity/cure4insect)

## Install

```R
devtools::install_github("ABbiodiversity/cure4insect")
```

## Examples

```R
library(cure4insect)

## workflow with 1 species --------------------
## ID is a vector of Row_Col IDs of 1km pixels
## species is a vector if species IDs
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
load_species_data("Ovenbird")
x <- calculate_results()
x
flatten_results(x)

## workflow with multiple species ----------------
load_common_data() # use as before
## id and species can be defined using text files
Spp <- read.table(system.file("extdata/species.txt", package="cure4insect"))
ID <- read.table(system.file("extdata/pixels.txt", package="cure4insect"))
subset_common_data(id=ID, species=Spp)
xx <- report_all()
str(xx)
do.call(rbind, lapply(xx, flatten_results))

## ID can also be a SpatialPolygons object based on GeoJSON for example
library(rgdal)
dsn <- system.file("extdata/polygon.geojson", package="cure4insect")
ply <- readOGR(dsn=dsn)
subset_common_data(id=ply, species=Spp)
xx2 <- report_all()

## wrapper function ----------------------
## species="all" runs all species
## species="mites" runs all mite species
## sender="you@example.org" will send an email with the results attached
z <- custom_report(id=ID,
    species=c("AlderFlycatcher", "Achillea.millefolium"),
    address=NULL)
z

## working with a local copy of the results is much faster
## set path via function arguments or the options:
getOption("cure4insect")
(opar <- set_options())
set_options(path = "/your/path/to/local/copy")
(set_options(opar)) # reset options

## change configs in this file to make it permanent for a given installation
as.list(drop(read.dcf(file=system.file("config/defaults.conf",
package="cure4insect"))))
```

## Web API

Get results in csv format (for other formats see [here](https://www.opencpu.org/api.html#api-formats))

```shell
curl http://sc-dev.abmi.ca/ocpu/library/cure4insect/R/custom_report/csv \
-H "Content-Type: application/json" -d \
'{"id":["182_362", "182_363"], "species":["AlderFlycatcher", "Achillea.millefolium"]}'
```

## Todo

* provide downloadable zip of results so that folks can work from local drive
* deveop fully fledged web interface (is/species as csv)
* decide how to report CI for SI, and if raw boot results are needed

fix this:

```
> library(cure4insect)
Loading required package: Matrix
Loading required package: intrval
Loading required package: sendmailR
Loading required package: sp
cure4insect 0.0-2        2018-01-08
>
> ## workflow with 1 species --------------------
> ## ID is a vector of Row_Col IDs of 1km pixels
> ## species is a vector if species IDs
> load_common_data()
> ## here is how to inspect all possible spatial and species IDs
> spp=get_all_species()[1:10]
> ID <- c("182_362", "182_363", "182_364", "182_365", "182_366", "182_367",
+     "182_368", "182_369", "182_370", "182_371", "182_372")
> z <- custom_report(id=ID,
+     species=spp)
loading common data
arranging subsets
processing species: Abies.balsamea 1 / 10
processing species: Achillea.alpina 2 / 10
processing species: Achillea.millefolium 3 / 10
processing species: Actaea.rubra 4 / 10
processing species: Adoxa.moschatellina 5 / 10
processing species: Agoseris.glauca 6 / 10
Error in .c4i1$Curr.Boot[PIX10, , drop = FALSE] : subscript out of bounds



library(cure4insect)
opar <- set_options(path = "w:/reports")
getOption("cure4insect")
load_common_data()
SPP <- get_all_species()
subset_common_data(id=get_all_id(), species=SPP)
t0 <- proc.time()
res <- list()
for (i in 1:length(SPP)) {
    cat("processing species:", SPP[i], i, "/", length(SPP), "\n")
    flush.console()
    load_species_data(SPP[i])
    res[[i]] <- try(calculate_results())
}
(t1 <- proc.time() - t0)


```


