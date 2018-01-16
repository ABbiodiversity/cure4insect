\name{internals}
\alias{app_read_csv}
\alias{app_test}
\alias{.load_species_data}
\alias{.calculate_results}
\alias{.report_all_by1}
\alias{.read_raster_template}
\alias{.make_raster}
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

.load_species_data(species, boot=TRUE, path=NULL, version=NULL, .c4is)
.calculate_results(y, level=0.9, .c4is)
.report_all_by1(boot=TRUE, path=NULL, version=NULL, level=0.9, cores=NULL)
.read_raster_template()
.make_raster(value, rc, rt)
}
\arguments{
  \item{species, boot, path, version}{arguments passed to
    \code{\link{load_species_data}} and other functions.}
  \item{y, level}{arguments passed to \code{\link{calculate_results}}
    and other functions.}
  \item{.c4is}{the subset environment \code{as.list(.c4is)}.}
  \item{cores}{desired number of cores to use.}
  \item{value, rc, rt}{call \code{value},
    \code{rc} is data frame with \code{Row} and \code{Col} indices for
    \code{value}, \code{rt} is raster template from
    \code{.read_raster_template}.}
  \item{\dots}{arguments passed to underlying functions.}
}
\details{
\code{app_read_csv} wraps \code{\link{read.csv}}.

\code{app_test} mimics \code{\link{custom_report}}.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\keyword{ utils }
