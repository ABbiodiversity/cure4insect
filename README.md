# cure4insect: Custom Reporting for Intactness and Sector Effects

## Install

```R
devtools::install_github("ABbiodiversity/cure4insect")
```

## Examples

```R
library(cure4insect)

## the workflow with 1 species
## ID is a vector of Row_Col IDs of 1km pixels
## species is a vector if species IDs
load_common_data()
Spp <- "Ovenbird"
ID <- c("182_362", "182_363", "182_364", "182_365", "182_366", "182_367",
    "182_368", "182_369", "182_370", "182_371", "182_372")
subset_common_data(id=ID, species=Spp)
load_species_data("Ovenbird")
x <- calculate_results()
x
flatten_results(x)

## wrapper function
## species="all" runs all species
## species="mites" runs all mite species
## sender=you@example.org will send an email with the results attached
z <- custom_report(id=ID, 
    species=c("AlderFlycatcher", "Achillea.millefolium"),
    address=NULL)
z

## working with a local copy of the results is much faster
## set path via function arguments or the options:
(opar <- set_options())
set_options(baseurl = "/your/path/to/local/copy")
(set_options(opar)) # reset options

## change configs in this file to make it permanent for a given installation
system.file("config/defaults.conf", package="cure4insect")
```

## Web API

```
curl http://sc-dev.abmi.ca/ocpu/library/cure4insect/R/custom_report/json \
-H "Content-Type: application/json" -d \
'{"id":["182_362", "182_363"], "species":["AlderFlycatcher", "Achillea.millefolium"]}'
```

## Todo

* provide species table and kgrid as data, so that folks can subset
* provide downloadable zip of results so that folks can work from local drive
* refine interface to accept GeoJSON polygons
* deveop fully fledged web interface
