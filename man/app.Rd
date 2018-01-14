\name{app_read_csv}
\alias{app_read_csv}
\title{
App Specific Functions
}
\description{
These functions are needed by OpenCPU apps in the package.
}
\usage{
app_read_csv(...)
app_test(...)
}
\arguments{
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
