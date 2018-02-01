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

load_species_data(species, boot=TRUE, path=NULL, version=NULL)
calculate_results(y, level=0.9)
rasterize_results(y)

report_all(boot=TRUE, path=NULL, version=NULL,
    level=0.9, cores=NULL)
flatten(x, ...)
\method{flatten}{c4iraw}(x, raw_boot=FALSE, limit=0.01, ...)

custom_report(id=NULL, species="all",
    path=NULL, version=NULL, address=NULL,
    boot=TRUE, level=0.9, cores=NULL,
    raw_boot=FALSE, limit=0.01)

set_options(...)
overlay_polygon(ply)
get_all_id()
get_all_species()
get_id_locations()
get_species_table()

load_spclim_data(species, boot=TRUE, path=NULL, version=NULL)
get_levels()
\method{predict}{c4ispclim}(object, xy, veg, soil, ...)
predict_mat(object, ...)
\method{predict_mat}{c4ispclim}(object, xy, veg, soil, ...)
combine_veg_soil(xy, veg, soil)
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
IDs of the 1km x 1km spatial pixel units to be used for the custom summaries.
The Row_Col field defines the IDs and links the raster cells in the geodatabase
(\url{http://ftp.public.abmi.ca/species.abmi.ca/gis/Grid1km_working.gdb.zip})
or CSV
(\url{http://ftp.public.abmi.ca/species.abmi.ca/gis/Grid1km_working.csv.zip};
with latitude/longitude in NAD_1983_10TM_AEP_Forest projection
\url{http://spatialreference.org/ref/epsg/3402/}).
If \code{id} is a matrix-like object, values of the 1st column are taken.
\code{id} can be of class 'SpatialPolygons' defined by the 'sp' package.
}
  \item{species}{
A list of species defined by the field SpeciesID in the table
\url{http://ftp.public.abmi.ca/species.abmi.ca/reports/2017/data/species-info.csv}.
If \code{species} is a matrix-like object, values of the 1st column are taken.
}
  \item{boot}{
logical, if confidence intervals for abundance and intactness are desired.
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
numeric (0-1), species stats in a region are set to \code{NA} when the mean
abundance of the species is less than 100 * \code{limit} percent of the maximum
abundance (current and reference combined).
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
  \item{ply}{
an object of class 'SpatialPolygons' defined by the 'sp' package.
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

\code{set_options} sets the options and return previous values invisibly.

\code{get_all_id} and \code{get_all_species} gets all possible spatial and
species IDs. \code{get_id_locations} gets the 'SpatialPoints' object
with geographic coordinates of the spatial IDs, \code{get_species_table}
gets the lookup table for species.
All these require to run \code{load_common_data()} first.

\code{load_spclim_data} is used to load raster data for spatially
explicit predictions by \code{predict.c4ispclim}.
\code{predict.c4ispclim} returns a data frame with columns
'veg', 'soil', and 'comb' (combines 'veg' and 'soil' based on
aspen probability of occurrence using \code{combine_veg_soil}
as a weighted average based on probability of aspen occurrence).
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
