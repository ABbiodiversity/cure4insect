\name{sector_plot}
\alias{sector_plot}
\alias{sector_plot.c4iraw}
\alias{sector_plot.data.frame}
\title{
Sector Efects Plots
}
\description{
Plots the sector effects for a single or a group of species.
}
\usage{
sector_plot(x, ...)
\method{sector_plot}{c4iraw}(x,
    type=c("unit", "regional", "underhf"), main, ...)
\method{sector_plot}{data.frame}(x,
    type=c("unit", "regional", "underhf"), main, ...)
}
\arguments{
  \item{x}{
an object of class 'c4iraw' (from \code{\link{calculate_results}})
or a data frame with flattened (1-row) results per species.
}
  \item{type}{
number of bins to be used in kernel density estimation.
}
  \item{main}{
title for the plot, if single species results are displayed
the default is to use the species ID.
}
  \item{\dots}{
other possible arguments passed to underlying functions,
e.g. \code{ylim}, \code{xlab}, \code{ylab},
\code{col} (for sector colors),
or \code{method} (one of \code{"kde"}, \code{"fft"}, or \code{"hist"}).
}
}
\details{
Unit type sector effects are based on regional current and reference abundances,
and the regional sector effects are standardized by footprint area.

Regional sector effects includes native and disturbed habitats when
comparing regional abundance under current and reference conditions.

Under HF (Hunam Footprint) type sector effects consider only the abundance
that us 'under the footprint', meaning that the current designation
is disturbed.

The single species plots are different kinds of bar plots.
The multi-species plot represents violin (carrot, vase) plots
based on kernel density, fast Fourier transform, or binning (histogram).
}
\value{
Returns the plotted data invisibly.
}
\author{
Peter Solymos <solymos@ualberta.ca>
}
\seealso{
\code{\link{calculate_results}}
}
\examples{
## *res*ults from calculate_results, all province, all species
load(system.file("extdata/raw_all.rda", package="cure4insect"))

sector_plot(res[["CanadaWarbler"]], "unit")
sector_plot(res[["CanadaWarbler"]], "regional")
sector_plot(res[["CanadaWarbler"]], "underhf")

z <- do.call(rbind, lapply(res, flatten_results))
sector_plot(z, "unit") # all species
sector_plot(z[1:100,], "regional") # use a subset
sector_plot(z, "underhf", method="hist") # binned version
}
\keyword{ aplot }
