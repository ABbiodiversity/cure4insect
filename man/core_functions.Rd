\name{custom_report}
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
flatten_results(x, raw_boot=FALSE)

custom_report(id=NULL, species="all",
    path=NULL, version=NULL,
    address=NULL, sender=NULL, boot=TRUE,
    level=0.9, raw_boot=FALSE)

set_options(...)
overlay_polygon(ply)
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
  \item{address}{
character, optional email address to send finished results to
in the format \code{name@domain.org}.
The default (\code{NULL}) is not to send an email.
}
  \item{sender}{
character, optional email address to send finished results to
in the format \code{name@domain.org}.
The default (\code{NULL}) is not to use
\code{getOption("cure4insect")$sender}.
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
Current options are: \code{baseurl}, \code{version}, and \code{sender}.
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

\code{set_options} sets the options and return previous values invisibly.
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
## ID can also be a SpatialPolygons object based on GeoJSON for example
#library(rgdal)
#dsn <- system.file("extdata/polygon.geojson", package="cure4insect")
#ID <- readOGR(dsn=dsn, layer="OGRGeoJSON")
subset_common_data(id=ID, species=Spp)
xx <- report_all()
str(xx)
do.call(rbind, lapply(xx, flatten_results))

## wrapper function ----------------------
## species="all" runs all species
## species="mites" runs all mite species
## sender=you@example.org will send an email with the results attached
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
