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
\alias{.select_id}
\alias{p_bird}
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

.load_species_data(species, boot=NULL, path=NULL, version=NULL,
    taxon, model_north, model_south)
.calculate_results(y, level=0.9, .c4is)
.calculate_limit(y, limit=NULL)
.report_all_by1(boot=NULL, path=NULL, version=NULL, level=0.9, cores=NULL)
.read_raster_template()
.make_raster(value, rc, rt)
.rasterize_multi(y, type=c("richness", "intactness"), rt)
.verbose()
.get_cores(cores=NULL)
.check(x, ref)
.combine_veg_soil(ipa, veg, soil)
.validate_id(id, type=c("km", "qs"))
.verbose()
.select_id(mregion="both", nr=NULL, nsr=NULL, luf=NULL)
p_bird(D, area=c("ha", "km"), pair_adj=2)
}
\arguments{
  \item{species, boot, path, version}{arguments passed to
    \code{\link{load_species_data}} and other functions.}
  \item{taxon, model_north, model_south}{taxonomic group required for the correct
    path, and logical values indicating north and south model results.}
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
  \item{mregion, nr, nsr, luf}{regions.}
  \item{D, area, pair_adj}{density, scale, and pair adjustment
    to turn bird densities into probability of observing non-zero count.}
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
