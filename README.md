**This repo is not actively maintained, please visit [ABbiodiversity/allinone](https://github.com/ABbiodiversity/allinone#allinone) instead**

cure4insect <img src="https://raw.githubusercontent.com/ABbiodiversity/cure4insect/master/abundance.gif" align="right" style="padding-left:10px;background-color:white;" />
===========================================================================================================================================================================

> Custom Reporting for Intactness and Sector Effects

[![Linux build
status](https://travis-ci.org/ABbiodiversity/cure4insect.svg?branch=master)](https://travis-ci.org/ABbiodiversity/cure4insect)

The [R](https://www.r-project.org/) package is a decision support tool
that provides an interface to enable custom reporting for intactness and
sector effects based on estimates and predictions created by the
[Alberta Biodiversity Monitoring Institute (ABMI)](http://abmi.ca/) in
collaboration with the [Boreal Avian Modelling (BAM)
Project](http://www.borealbirds.ca/).

-   [Few slides about the
    motivations](https://abbiodiversity.github.io/cure4insect/intro/)
-   [Example species
    report](https://abbiodiversity.github.io/cure4insect/example/)
-   [Custom report](https://abbiodiversity.github.io/cure4insect/site/)
-   [Web
    app](http://science.abmi.ca/ocpu/apps/ABbiodiversity/cure4insect/www/)

License
-------

The estimates, predictions, and related documentation are © ABMI and BAM
(2014–2018) under a [CC BY-SA 4.0
license](http://creativecommons.org/licenses/by-sa/4.0/).

The R package itself is licensed under [MIT license](LICENSE.md) © 2018
Peter Solymos, Brandon Allen, Ermias T. Azeria, Shannon R. White, ABMI &
BAM.

Getting help or reporting an issue
----------------------------------

To report bugs/issues/feature requests, please file an
[issue](https://github.com/ABbiodiversity/cure4insect/issues).

How to contribute
-----------------

If you would like to contribute to the package, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

Install
-------

Only GitHub version available now:

``` r
remotes::install_github("ABbiodiversity/cure4insect")
```

If it fails for some reason, you can try:

``` r
drat::addRepo("ABbiodiversity")
install.packages("cure4insect")
```

The [NEWS](NEWS.md) file lists user visible changes in the different
versions.

Usage
-----

Load the package and the common data set:

``` r
library(cure4insect)
load_common_data()
## loading common data
##         version yr_first yr_last        method     hf  veg          model
## mammals    2018     2001    2013 snow_tracking 2016v3 v6.1 binomial_logit
## birds      2018     1997    2017   point_count 2016v3 v6.1    poisson_log
## mites      2018     2007    2017     soil_core 2016v3 v6.1 binomial_logit
## mosses     2018     2003    2016   centre_plot 2016v3 v6.1 binomial_logit
## lichens    2018     2003    2017   centre_plot 2016v3 v6.1 binomial_logit
## vplants    2018     2003    2017   centre_plot 2016v3 v6.1 binomial_logit
##         species
## mammals      12
## birds       126
## mites       114
## mosses      125
## lichens     155
## vplants     442
```

Note: it is possible to download the data the package is using to your
hard drive using the `dowload_data` function.

#### Workflow with 1 species

`id` is a vector of `Row_Col` type IDs of 1 km<sup>2</sup> pixels,
`species` is a vector of species IDs:

``` r
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

All the possible spatial IDs can be inspected as:

``` r
str(get_all_id())
plot(xy <- get_id_locations(), pch=".")
summary(xy)
```

Spatial IDs can be specified as planning/management regions:

``` r
## Natural Regions
ID <- get_all_id(nr=c("Boreal", "Foothills"))
## Natural Subregions
ID <- get_all_id(nsr="Lower Boreal Highlands")
## Land Use Framework regions
ID <- get_all_id(luf="North Saskatchewan")
```

Alternatively, `id` can refer to quarter sections using the
`MER-RGE-TWP-SEC-QS` format:

``` r
Spp <- "Ovenbird"
QSID <- c("4-12-1-2-SE", "4-12-1-2-SW", "4-12-1-3-SE", "4-12-1-3-SW")
qs2km(QSID) # corresponding Row_Col IDs
```

The `subset_common_data` function recognizes `MER-RGE-TWP-SEC-QS` type
spatial IDs and onvert those to the `Row_Col` format using the nearest 1
km<sup>2</sup> pixels.

#### Workflow with multiple species

`id` and `species` can be defined using text files:

``` r
load_common_data()
Spp <- read.table(system.file("extdata/species.txt", package="cure4insect"))
str(Spp)
ID <- read.table(system.file("extdata/pixels.txt", package="cure4insect"))
str(ID)
subset_common_data(id=ID, species=Spp)
xx <- report_all()
str(xx)
do.call(rbind, lapply(xx, flatten))
```

#### Species subset specifications

Here is how to inspect all possible species IDs

``` r
str(get_all_species())
str(get_species_table())
```

Select one or more taxonomic groups (mammals, birds, mites, mosses,
lichens, vpalnst), and fiter for habitat and status:

``` r
## birds and mammals
str(get_all_species(taxon=c("birds", "mammals")))
## all upland species
str(get_all_species(taxon="all", habitat="upland"))
## nonnative vascular plants
str(get_all_species(taxon="vplants", status="nonnative"))
```

#### Wrapper functions

-   `species="all"` runs all species
-   `species="mites"` runs all mite species
-   `sender="you@example.org"` will send an email with the results
    attached
-   increase `cores` to allow parallel processing

``` r
z <- custom_report(id=ID,
    species=c("AlderFlycatcher", "Achillea.millefolium"),
    address=NULL, cores=1)
z
```

Working with a local copy of the results is much faster set path via
function arguments or the options:

``` r
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

``` r
## options
getOption("cure4insect")
## change configs in this file to make it permanent for a given installation
as.list(drop(read.dcf(file=system.file("config/defaults.conf",
    package="cure4insect"))))
```

#### Sector effects and intactness plots

``` r
## *res*ults from calculate_results, all province, all species
fn <- paste0("http://science.abmi.ca/reports/",
    getOption("cure4insect")$version, "/misc/raw_all.rda")
con <- url(fn)
load(con)
close(con)

plot_sector(res[["CanadaWarbler"]], "unit")
plot_sector(res[["CanadaWarbler"]], "regional")
plot_sector(res[["CanadaWarbler"]], "underhf")

z <- do.call(rbind, lapply(res, flatten))
class(z) <- c("c4idf", class(z))
plot_sector(z, "unit") # all species
plot_sector(z[1:100,], "regional") # use a subset
plot_sector(z, "underhf", method="hist") # binned version

plot_intactness(z, "SI")
plot_intactness(z, "SI2", method="hist")
```

#### Determining spatial IDs based on spatial polygons

`id` can also be a SpatialPolygons object based on GeoJSON for example:

``` r
library(rgdal)
dsn <- system.file("extdata/polygon.geojson", package="cure4insect")
cat(readLines(dsn), sep="\n")
ply <- readOGR(dsn=dsn)
subset_common_data(id=ply, species=Spp)
plot(make_subset_map())
xx2 <- report_all()
```

Spatial IDs of the 1 km<sup>2</sup> spatial pixel units are to be used
for the custom summaries. The `Row_Col` field defines the IDs and links
the raster cells to the
[geodatabase](http://science.abmi.ca/reports/2017/grids/Grid1km_working.gdb.zip)
or
[CSV](http://science.abmi.ca/reports/2017/grids/Grid1km_working.csv.zip%7D)
(with latitude/longitude in
[NAD\_1983\_10TM\_AEP\_Forest](http://spatialreference.org/ref/epsg/3402/)
projection).

For the [web
application](http://science.abmi.ca/ocpu/apps/ABbiodiversity/cure4insect/www/),
use your favourite GIS software, or in R use this to get the spatial IDs
written into a text file:

``` r
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

Use the `make_subset_map()` function to get a raster map of the spatial
selection.

#### Raster objects and maps

The result is a raster stack object with the following layers:

-   `NC`, `NR`: current and reference abundance,
-   `SI`, `SI2`: one- and two-sided intactness,
-   `SE`, `CV`: bootstrap based standard error and coefficient of
    variation estimates for current abundance.

``` r
load_common_data()
y <- load_species_data("Ovenbird")
r <- rasterize_results(y)
plot(r, "NC") # current abundance map
col <- colorRampPalette(c("darkgreen","yellow","red"))(250)
plot(r, "SE", col=col) # standadr errors for current abundance
```

It is possible to make multi-species maps as well: average intactness
and expected number of species.

``` r
subset_common_data(species=get_all_species(taxon="birds"))
r1 <- make_multispecies_map("richness")
r2 <- make_multispecies_map("intactness")
```

#### Spatially explicit (polygon level) predictions

The 1 km<sup>2</sup> level predictions provide mean abundance per pixel.
Sometimes we need finer detail, e.g. when making predictions as part of
spatially explicit simulations.

First we load the spatial/climate related component of the predictions
(which is a raster object):

``` r
load_common_data()
species <- "Achillea.millefolium"
object <- load_spclim_data(species)
```

The spatial component is then combined with the land cover component
describing vegetation/disturbance/soil classes as a factor.

``` r
## original levels
levels(veg <- as.factor(get_levels()$veg))
levels(soil <- as.factor(get_levels()$soil))
```

Sometimes it is best to create a crosswalk table and reclassify using
e.g. the `mefa4::reclass` function:

``` r
(rc <- data.frame(In=c("pine5", "decid15", "urban", "industrial"),
    Out=c("Pine0", "Deciduous10", "UrbInd", "UrbInd")))
mefa4::reclass(c("pine5", "pine5", "decid15", "urban", "industrial"), rc)
```

We need to have spatial locations for each land cover value (same value
can be repeated, but but avoid duplicate rownames). We use the **sp**
package to make a SpatialPoints object:

``` r
XY <- get_id_locations()
coords <- coordinates(XY)[10^5,,drop=FALSE]
rownames(coords) <- NULL
xy <- data.frame(coords[rep(1, length(veg)),])
coordinates(xy) <- ~ POINT_X + POINT_Y
proj4string(xy) <- proj4string(XY)
```

Now we are ready to make the predictions:

``` r
pred <- predict(object, xy=xy, veg=veg)
summary(pred)
```

The `predict` function returns a data frame with columns `veg`, `soil`,
and `comb` (combines `veg` and `soil` based on aspen probability of
occurrence using `combine_veg_soil` as a weighted average based on
probability of aspen occurrence).

For some species, either the `veg` or `soil` based estimates are
unavailable: `predict` returns `NA` for these and the combined results
will be `NA` as well.

The next line is a more succinct version that loads the species data as
well, but we can’t reuse the species data after:

``` r
pred <- custom_predict(species, xy=xy, veg=veg)
```

Another was of making predictions is to define a spatial grid, and
quantify land cover as proportion of the land cover types in each grid
cell. This is how we can use multivariate input data in a spatial grid
(totally unrealistic data set just for illustration, but user has to
make sure the numbers are meaningful):

``` r
xy <- xy[1:10,]
mveg <- matrix(0, 10, 8)
colnames(mveg) <- veg[c(1:8 * 10)]
mveg[] <- rpois(80, 10) * rbinom(80, 1, 0.2)
mveg[rowSums(mveg)==0,1] <- 1 # avoid 0 row sum
mveg

msoil <- matrix(0, 10, 6)
colnames(msoil) <- get_levels()$soil[1:6]
msoil[] <- rpois(60, 10) * rbinom(60, 1, 0.4)
msoil[rowSums(msoil)==0,1] <- 1 # avoid 0 row sum
msoil
```

Because we used areas (not proportions) we get the output as two
matrices containing abundances (density times area) corresdonding to the
vegetation and soil matrices:

``` r
(prmat1 <- predict_mat(object, xy, mveg, msoil))
```

Row sums give the total abundance at each location, column sums give the
total abundance in a land cover type over all locations:

``` r
rowSums(prmat1$veg)
colSums(prmat1$veg)
```

Using proportions in the input matrices gives mean abundance per spatial
unit as output:

``` r
(prmat2 <- predict_mat(object, xy, mveg/rowSums(mveg), msoil/rowSums(msoil)))
```

Combining vegetation and soil based predictions returns a vector,
i.e. the aspen probability weighted average of the vegetation and soil
based total abundances:

``` r
combine_veg_soil(xy, rowSums(prmat2$veg), rowSums(prmat2$soil))
```

#### Visualize land cover associations

See the following [R markdown](http://rmarkdown.rstudio.com/) file for a
worked example of visualizations available in the package:

``` r
file.show(system.file("doc/example-species-report.Rmd", package="cure4insect"))
```

It is possible to render the R markdown file with a species ID argument,
thus programmatically producing reports for multiple species:

``` r
library(rmarkdown)
render(system.file("doc/example-species-report.Rmd",
    package="cure4insect"),
    params = list(species = "Ovenbird"))
```

Habitat associations as shown on the
[science.abmi.ca](http://science.abmi.ca/) website:

``` r
load_common_data()
plot_abundance("Achillea.millefolium", "veg_coef")
plot_abundance("Achillea.millefolium", "soil_coef", paspen=1)
plot_abundance("Achillea.millefolium", "veg_lin")
plot_abundance("Achillea.millefolium", "soil_lin")
```

Web API
-------

The web app sits
[here](http://science.abmi.ca/ocpu/apps/ABbiodiversity/cure4insect/www/).
To get more control over the results, use the
[API](https://www.opencpu.org/api.html#api-formats).

Make a request using the `custom_report` function:

``` shell
curl http://science.abmi.ca/ocpu/apps/ABbiodiversity/cure4insect/R/custom_report/csv \
-H "Content-Type: application/json" -d \
'{"id":["182_362", "182_363"], "species":["AlderFlycatcher", "Achillea.millefolium"]}'
```

Access spatially explicit and land cover specific prediction for a
species using the `custom_predict` function:

``` shell
curl http://science.abmi.ca/ocpu/apps/ABbiodiversity/cure4insect/R/custom_predict/json \
-H "Content-Type: application/json" -d \
'{"species":"AlderFlycatcher", "xy":[[-114.4493,58.4651]], "veg":"Mixedwood80"}'
```

Explore single and multi-species results
----------------------------------------

To get similar output to
[this](https://abbiodiversity.github.io/cure4insect/site/), run script
from this file:

``` r
file.show(system.file("doc/custom-report.R", package="cure4insect"))
```
