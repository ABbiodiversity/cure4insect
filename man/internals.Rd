\name{internals}
\alias{app_read_csv}
\alias{app_test}
\alias{.load_species_data}
\alias{.calculate_results}
\alias{.report_all_by1}
\alias{.read_raster_template}
\alias{.make_raster}
\alias{.rasterize_multi}
\alias{.calculate_limit}
\alias{.verbose}
\alias{.get_cores}
\alias{.check}
\alias{.combine_veg_soil}
\alias{.validate_id}
\alias{.verbose}
\title{
App Specific Functions and Internals
}
\description{
These functions are needed by OpenCPU apps in the package,
or needed to be exported by the package but not intended to be
used by users.
}
\usage{
app_read_csv(...)
app_test(...)

.load_species_data(species, boot=TRUE, path=NULL, version=NULL, taxon)
.calculate_results(y, level=0.9, .c4is)
.calculate_limit(y, limit=NULL)
.report_all_by1(boot=TRUE, path=NULL, version=NULL, level=0.9, cores=NULL)
.read_raster_template()
.make_raster(value, rc, rt)
.rasterize_multi(y, type=c("richness", "intactness"), rt)
.verbose()
.get_cores(cores=NULL)
.check(x, ref)
.combine_veg_soil(ipa, veg, soil)
.validate_id(id, type=c("km", "qs"))
.verbose()
}
\arguments{
  \item{species, boot, path, version}{arguments passed to
    \code{\link{load_species_data}} and other functions.}
  \item{taxon}{taxonomic group required for the correct path.}
  \item{y, level}{arguments passed to \code{\link{calculate_results}}
    and other functions.}
  \item{.c4is}{the subset environment \code{as.list(.c4is)}.}
  \item{cores}{desired number of cores to use.}
  \item{value, rc, rt}{call \code{value},
    \code{rc} is data frame with \code{Row} and \code{Col} indices for
    \code{value}, \code{rt} is raster template from
    \code{.read_raster_template}.}
  \item{id}{spatial IDs.}
  \item{type}{type of multi-species map (richness or intactness)
    or spatial ID (km or QS).}
  \item{limit}{abundance threshold for multi-species intactness reporting.}
  \item{x, ref}{checking the validity of land cover classes in \code{x}
    against the \code{ref}erence list of possible values.}
  \item{ipa, veg, soil}{calculates a weighted average of \code{veg}
    and \code{soil} based models based on probability of aspen (\code{ipa}).}
  \item{\dots}{arguments passed to underlying functions.}
}
\details{
\code{app_read_csv} wraps \code{\link{read.csv}}.

\code{app_test} mimics \code{\link{custom_report}}.

Interface for internal functions might change and usage is not recommended.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\keyword{ utils }
