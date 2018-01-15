\name{internals}
\alias{app_read_csv}
\alias{app_test}
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

.calculate_results(y, level=0.9, .c4is)
}
\arguments{
  \item{y, level, .c4is}{arguments passed to \code{\link{calculate_results}}
    plus the subset environment \code{as.list(.c4is)}.}
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
