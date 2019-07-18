\name{custom_report}
\alias{cure4insect}
\alias{cure4insect-package}
\alias{load_common_data}
\alias{subset_common_data}
\alias{load_species_data}
\alias{clear_common_data}
\alias{clear_subset_data}
\alias{calculate_results}
\alias{flatten}
\alias{flatten.c4iraw}
\alias{report_all}
\alias{custom_report}
\alias{custom_predict}
\alias{set_options}
\alias{get_all_id}
\alias{get_all_species}
\alias{get_id_locations}
\alias{get_species_table}
\alias{is_loaded}
\alias{overlay_polygon}
\alias{rasterize_results}
\alias{load_spclim_data}
\alias{predict.c4ispclim}
\alias{predict_mat}
\alias{combine_veg_soil}
\alias{predict_mat.c4ispclim}
\alias{get_levels}
\alias{get_version_info}
\alias{make_multispecies_map}
\alias{get_all_qsid}
\alias{qs2km}
\alias{make_subset_map}
\alias{get_subset_info}
\alias{get_subset_id}
\alias{get_subset_species}
\alias{get_id_table}
\title{
Core Functions for Custom Reporting
}
\description{
These functions load pre-processed data and
calculate intactness and sector effects
for custom regions and sets of species.
}
\usage{
load_common_data(path=NULL, version=NULL)
is_loaded()
clear_common_data()
subset_common_data(id=NULL, species="all")
clear_subset_data()

load_species_data(species, boot=NULL, path=NULL, version=NULL)
calculate_results(y, level=0.9)
rasterize_results(y)

report_all(boot=NULL, path=NULL, version=NULL, level=0.9, cores=NULL)
flatten(x, ...)
\method{flatten}{c4iraw}(x, raw_boot=FALSE, limit=NULL, ...)

custom_report(id=NULL, species="all",
    path=NULL, version=NULL, address=NULL,
    boot=NULL, level=0.9, cores=NULL,
    raw_boot=FALSE, limit=NULL)
custom_predict(species, xy, veg, soil,
    path=NULL, version=NULL, ...)

set_options(...)
overlay_polygon(ply)
get_all_id(mregion="both", nr=NULL, nsr=NULL, luf=NULL)
get_all_species(taxon = "all", mregion="both", habitat, status)
get_id_locations()
get_id_table(mregion="both", nr=NULL, nsr=NULL, luf=NULL)
get_species_table(taxon = "all", mregion="both", habitat, status)
get_version_info()
get_all_qsid()
qs2km(qsid)
make_subset_map()
get_subset_info()
get_subset_id()
get_subset_species()

load_spclim_data(species, path=NULL, version=NULL)
get_levels()
\method{predict}{c4ispclim}(object, xy, veg, soil,
    method="simple", ...)
predict_mat(object, ...)
\method{predict_mat}{c4ispclim}(object, xy, veg, soil,
    method="simple"...)
combine_veg_soil(xy, veg, soil, method="simple")
make_multispecies_map(type=c("richness", "intactness"),
    path=NULL, version=NULL, clip=TRUE, limit=NULL)
}
\arguments{
  \item{path}{
path to a local copy of results or \code{NULL} (default, the
value of \code{getOption("cure4insect")$baseurl}).
}
  \item{version}{
version of the results or \code{NULL} \code{NULL} (default, the
value of \code{getOption("cure4insect")$version}).
}
  \item{id}{
character, IDs of the 1km x 1km spatial pixel units to be used for the custom summaries.
The Row_Col field defines the IDs and links the raster cells in the geodatabase
(\url{http://sc-dev.abmi.ca/reports/2017/grids/Grid1km_working.gdb.zip})
or CSV
(\url{http://sc-dev.abmi.ca/reports/2017/grids/Grid1km_working.csv.zip};
with latitude/longitude in NAD_1983_10TM_AEP_Forest projection
\url{http://spatialreference.org/ref/epsg/3402/}).
If \code{id} is a matrix-like object, values of the 1st column are taken.
\code{id} can be of class 'SpatialPolygons' defined by the 'sp' package.
The supplied values are turned into character internally.
Use \code{get_all_id} to see all possible values.
}
  \item{qsid}{
character, quarter section (QS) IDs in the
\code{"MER-RGE-TWP-SEC-QS"} format, e.g. \code{"4-12-1-2-SE"}.
The CSV file \url{http://sc-dev.abmi.ca/reports/2017/grids/Grid1km_working.csv.zip}
has the corresponding coordinates in NAD_1983_10TM_AEP_Forest projection
\url{http://spatialreference.org/ref/epsg/3402/}).
}
  \item{species}{
A list of species defined by the field SpeciesID in the table
\url{http://sc-dev.abmi.ca/reports/2017/data/species-info.csv}.
If \code{species} is a matrix-like object, values of the 1st column are taken.
The following values are also accepted to define groups of species:
\code{"all"} (all groups/species),
or one of the taxonomic (\code{"birds"}, \code{"lichens"}, \code{"mammals"},
\code{"mites"}, \code{"mosses"}, \code{"vplants"}) or other
(\code{"upland"}, \code{"lowland"}, \code{"native"}, \code{"nonnative"}) groups.
For a combination of filters, see \code{get_all_species}.
}
  \item{boot}{
logical or \code{NULL}, if confidence intervals for abundance and intactness are desired.
Defaults to \code{getOption("cure4insect")$boot} when \code{NULL}.
}
  \item{level}{
numeric, level for confidence interval, defaults to 90\%.
}
  \item{cores}{
integer, number of cores used in forking (used on Unix/Linux OS)
or number of cluster workers (on Windows).
Defaults to \code{getOption("cure4insect")$cores} when \code{NULL}.
}
  \item{y}{
and input object from call to \code{load_species_data}.
}
  \item{x}{
and input object from call to \code{calculate_results}.
}
  \item{raw_boot}{
logical, if raw bootstrap abundance results should be returned.
}
  \item{limit}{
numeric (0-1), species with mean abundance less than 100 * \code{limit}
percent of the maximum abundance (current and reference combined)
are flagged and should not be used when calculating multi-species metrics
because the species is not effectively present in the region.
Defaults to \code{getOption("cure4insect")$limit} when \code{NULL}.
}
  \item{address}{
character, optional email address to send finished results to
in the format \code{"name@domain.org"}.
The default (\code{NULL}) is not to send an email.
}
  \item{object}{
and input object from call to \code{load_spclim_data}.
}
  \item{xy}{
a 'SpatialPoints' object defined by the 'sp' package
with geographic coordinates corresponding to \code{veg} and \code{soil}
(i.e. centroids of polygons).
}
  \item{veg, soil}{
factor, vegetation/soil classes. One of the two or both must be provided.
}
  \item{method}{
character, the method argument for raster value \code{\link[raster]{extract}}ion.
\code{"simple"}: values for the cell a point falls in,
\code{"bilinear"}: interpolated from the values of the four nearest raster cells.
}
  \item{taxon}{
character, return \code{"all"} species, or a subset
(one or more of \code{"birds"}, \code{"lichens"}, \code{"mammals"},
\code{"mites"}, \code{"mosses"}, \code{"vplants"}).
}
  \item{habitat}{
character, one of \code{"upland"} or \code{"lowland"} to indicate
expert based habitat associations in Alberta.
}
  \item{status}{
character, one of \code{"native"} or \code{"nonnative"} to indicate
expert based status in Alberta.
}
  \item{mregion}{
character, modeling region (\code{"both"}, \code{"north"}, or \code{"south"}).
}
  \item{nr, nsr, luf}{
character vector, narural regions (nr), natural subregions (nsr), and
land use framework regions (luf) of Alberta.
}
  \item{ply}{
an object of class 'SpatialPolygons' defined by the 'sp' package.
}
  \item{clip}{
logical, if the multi-species map needs to be clipped to the region bounds
defined by the spatial subset.
}
  \item{type}{
character, the type of multi-species map to produce.
\code{"intactness"} is the raster cell level average intactness across species,
\code{"richness"} is probability of >0 observation in a 1 ha plot
summed across species (expected species richness).
}
  \item{\dots}{
Arguments in \code{tag = value} form, or a list of tagged values.
The tags are configuration settings as described below.
}
}
\details{
Configuration is stored in the file
\code{system.file("config/defaults.conf", package="cure4insect")}.
Current options are:
\code{path} (path to results),
\code{version} (version of results),
\code{verbose} (default is 1, value 0 suppresses the messages),
\code{limit} (abundance threshold for regional intactness calculations),
\code{cores} (number of cores to use in parallel calculations,
uses forking on Linux/Unix/Mac and socket clusters on Windows, default is 1),
\code{sender} (email of sender),
\code{subject} (subject of the email),
and \code{body} (body of the email).

The multicore processing performance (not on Windows, where shared memory forked
processes are not available) might be limited by memory and
network speed/bandwidth. Local copy of the data is the
surest way to boost performance.
}
\value{
\code{load_common_data} loads common data to memory of the R session.
\code{is_loaded} check if common data has been loaded previously.

\code{subset_common_data} subsets the common data and makes the information
available in the R session.
\code{make_subset_map} makes a raster map of the spatial selection,
\code{get_subset_info} counts the number of species and 1 square km
spatial pixel units in the selection. \code{get_subset_id} and
\code{get_subset_species} returns the species and spatial IDs,
respectively, based on the selection.

\code{load_species_data} load data of a single species
and returns it.

\code{clear_common_data}, and \code{clear_subset_data}
clears the respective environments.

\code{calculate_results} calculates intactness and sector effects
results based on spatial subset definitions and
single species data. The output is a list of class 'c4iraw'.
The \code{flatten} method arranges the results from \code{calculate_results}
into a 1-liner data frame.

\code{report_all} calculate results based on sequential calls
to \code{load_species_data} for all species defined in the subset.
Returns a list (class 'c4ilist') output from \code{calculate_results}.

\code{custom_report} wrapper function to load common data,
subset spatial units and species list, calculate results for all species,
and optionally send results as attachment of a email.
Returns an object of class 'c4idf' (inheriting from data frame),
rows representing species (flattened results, the Comment field
flagging possible issues).

\code{overlay_polygon} selects spatial IDs based on a 'SpatialPolygons' object.

\code{rasterize_results} takes the single species data
without any spatial subset. The output is a raster \code{\link[raster]{stack}}
object with the following layers:
NC (current abundance),
NR (reference abundance),
SI (intactness),
SI2 (two-sided intactness),
SE, and CV (bootstrap based standard error and coefficient of variation
estimates for current abundance).

\code{make_multispecies_map} calculated multi-species maps of richness
(1-ha based probability of occurrence summed across species)
or intactness (mean pixel level intactness of species whose
mean abundance in the subset region is at least 100 * \code{limit}
percent of the provincial maximum abundance).

\code{set_options} sets the options and return previous values invisibly.

\code{get_all_id} gets all possible spatial IDs.
\code{get_all_species} gets \code{"all"} possible species IDs,
or a subset of those IDs for a selected \code{taxon}.
\code{get_id_locations} gets the 'SpatialPoints' object
with geographic coordinates of the spatial IDs, \code{get_species_table}
gets the lookup table for species, or a group of species as defined by the
\code{taxon} argument.
\code{get_all_qsid} gets all possible quarter section (QS) IDs,
\code{qs2km} finds the 1 square km units corresponding to the
quarter section IDs.
All these require to run \code{load_common_data()} first.

\code{load_spclim_data} is used to load raster data for spatially
explicit predictions by \code{predict.c4ispclim}.
\code{predict.c4ispclim} returns a data frame with columns
'veg', 'soil', and 'comb' (combines 'veg' and 'soil' based on
aspen probability of occurrence using \code{combine_veg_soil}
as a weighted average based on probability of aspen occurrence).
Note: for some species, either the 'veg' or 'soil' based estimates are
unavailable. \code{predict.c4ispclim} returns \code{NA} for these
and the combined results will be \code{NA} as well.

\code{get_version_info} returns detailed version info for each taxa,
depending on the \code{version} set through options or
\code{load_common_data}, including the following variables:
\code{taxon},
\code{version} (year of production),
1st and last year of field sampling (\code{yr_first}, \code{yr_last}),
field \code{method}ology,
base year and version of human footprint inventory (\code{hf}),
version of the backfilled vegetation layer (\code{veg}),
the type of model used (\code{model}),  and the number of
\code{species} by taxon. \code{load_common_data} prints
these info upon successfully loading common data objects.

\code{get_levels} returns a list
with elements 'veg' for vegetation and 'soil' for soil class levels
expected by \code{predict.c4ispclim}. The \code{predict_clim}
method is similar, but expects \code{veg} and \code{soil}
to be a composition matrix with rows corresponding to
points in space (e.g. grid cell centroids) and columns
to correspond to vegetation/soil classes, with cell values
as areas or proportions (row standardized). The return value
is a list with elements \code{veg} and \code{soil},
each containing either a matrix with same dimensions
as the corresponding input, or \code{NULL} when the input is missing.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\seealso{
\code{\link{plot_sector}} and \code{\link{plot_abundance}} for plots.
}
\examples{
\dontrun{
## workflow with 1 species --------------------
## ID is a vector of Row_Col IDs of 1km pixels
## species is a vector if species IDs
load_common_data()
is_loaded()
get_version_info() # important details about versions
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
x <- calculate_results(y)
x
flatten(x)

## using quarter sections
Spp <- "Ovenbird"
QSID <- c("4-12-1-2-SE", "4-12-1-2-SW", "4-12-1-3-SE", "4-12-1-3-SW")
qs2km(QSID) # corresponding Row_Col IDs
subset_common_data(id=QSID, species=Spp)
y <- load_species_data(Spp)
flatten(calculate_results(y))

## using pre-defined planning/management regions
#ID <- get_all_id(nr=c("Boreal", "Foothills"))
ID <- get_all_id(luf="North Saskatchewan")
subset_common_data(id=ID)
plot(make_subset_map())

## workflow with multiple species ----------------
load_common_data() # use as before
## id and species can be defined using text files
Spp <- read.table(system.file("extdata/species.txt", package="cure4insect"))
ID <- read.table(system.file("extdata/pixels.txt", package="cure4insect"))
subset_common_data(id=ID, species=Spp)
xx <- report_all()
str(xx)
do.call(rbind, lapply(xx, flatten))

## ID can also be a SpatialPolygons object based on GeoJSON for example
#library(rgdal)
#dsn <- system.file("extdata/polygon.geojson", package="cure4insect")
#ply <- readOGR(dsn=dsn)
#subset_common_data(id=ply, species=Spp)
#xx2 <- report_all()

## wrapper function ----------------------
## species="all" runs all species
## species="mites" runs all mite species
## sender="you@example.org" will send an email with the results attached
## increase cores to allow parallel processing
z <- custom_report(id=ID,
    species=c("AlderFlycatcher", "Achillea.millefolium"),
    address=NULL, cores=1)
z

## making of the file raw_all.rda
opar <- set_options(path = "w:/reports")
getOption("cure4insect")
load_common_data()
SPP <- get_all_species()
subset_common_data(id=get_all_id(), species=SPP)
res <- list()
for (i in 1:length(SPP)) {
    cat("processing species:", SPP[i], i, "/", length(SPP), "\n")
    flush.console()
    y <- load_species_data(SPP[i])
    res[[i]] <- calculate_results(y)
}
names(res) <- SPP

## spatial maps
y <- load_species_data("Ovenbird")
r <- rasterize_results(y)
plot(r, "NC") # current abundance map
plot(r, "SE") # standadr errors for current abundance

## making multi-species richness and intactness maps for birds
subset_common_data(species=get_all_species(taxon="birds"))
r1 <- make_multispecies_map("richness")
r2 <- make_multispecies_map("intactness")
}

## working with a local copy of the results is much faster
## set path via function arguments or the options:
getOption("cure4insect")
(opar <- set_options())
set_options(path = "/your/path/to/local/copy")
(set_options(opar)) # reset options

## change configs in this file to make it permanent for a given installation
as.list(drop(read.dcf(file=system.file("config/defaults.conf",
package="cure4insect"))))

\dontrun{
## spatially explicit prediction
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
}
}
\keyword{ utilities }
