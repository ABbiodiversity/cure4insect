\name{custom_report}
\alias{cure4string}
\alias{cure4string-package}
\alias{load_common_data}
\alias{subset_common_data}
\alias{load_species_data}
\alias{clear_common_data}
\alias{clear_subset_data}
\alias{clear_species_data}
\alias{calculate_results}
\alias{flatten_results}
\alias{report_all}
\alias{custom_report}
\alias{set_options}
\alias{get_all_id}
\alias{get_all_species}
\alias{get_id_locations}
\alias{get_species_table}
\alias{overlay_polygon}
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
subset_common_data(id=NULL, species="all")
load_species_data(species, boot=TRUE, path=NULL, version=NULL)

clear_common_data()
clear_subset_data()
clear_species_data()

calculate_results(level=0.9)
report_all(boot=TRUE, path=NULL, version=NULL, level=0.9)
flatten_results(x, raw_boot=FALSE, limit=0.01)

custom_report(id=NULL, species="all",
    path=NULL, version=NULL, address=NULL,
    boot=TRUE, level=0.9, raw_boot=FALSE, limit=0.01)

set_options(...)
overlay_polygon(ply)
get_all_id()
get_all_species()
get_id_locations()
get_species_table()
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
\code{id} can be a GeoJSON file for the function \code{custom_report}
when \code{geojson=TRUE}
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
  \item{ply}{
an object of class SpatialPolygons defined by the 'sp' package.
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
\code{sender} (email of sender),
\code{subject} (subject of the email),
and \code{body} (body of the email).
}
\value{
\code{load_common_data} loads common data to memory of the R session.

\code{subset_common_data} subsets the common data and makes the information
available in the R session.

\code{load_species_data} load data of a single species to the
memory of the R session.

\code{clear_common_data}, \code{clear_subset_data},
and \code{clear_species_data} clear the respective environments.

\code{calculate_results} calculates intactness and sector effects
results based on spatial subset definitions and
single species data in the memory.

\code{flatten_results} arranges the results from \code{calculate_results}
into a 1-liner data frame.

\code{report_all} calculate results based on sequential calls
to \code{load_species_data} for all species defined in the subset.
Returns a list output from \code{calculate_results}.

\code{custom_report} wrapper function to load common data,
subset spatial units and species list, calculate results for all species,
and optionally send results as attachment of a email.
Returns a data frame, rows representing species (flattened results).

\code{overlay_polygon} selects spatial IDs based on a SpatialPolygons object.

\code{set_options} sets the options and return previous values invisibly.

\code{get_all_id} and \code{get_all_species} gets all possible spatial and
species IDs. \code{get_id_locations} gets the SpatialPoints object
with geographic coordinates of the spatial IDs, \code{get_species_table}
gets the lookup table for species.
All these require to run \code{load_common_data()} first.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\examples{
\dontrun{
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
}

## working with a local copy of the results is much faster
## set path via function arguments or the options:
getOption("cure4insect")
(opar <- set_options())
set_options(baseurl = "/your/path/to/local/copy")
(set_options(opar)) # reset options

## change configs in this file to make it permanent for a given installation
as.list(drop(read.dcf(file=system.file("config/defaults.conf",
package="cure4insect"))))
}
\keyword{ utilities }
