% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cascade.R
\name{as.matrix.cascade}
\alias{as.matrix.cascade}
\title{Convert a cascade object to a matrix}
\usage{
\method{as.matrix}{cascade}(x, ...)
}
\arguments{
\item{x}{cascade object to convert.}

\item{...}{additional arguments to be passed to or from methods. 
(Currently not supported.)}
}
\value{
A matrix containing all cascade information in wide format. That is,
each row of the matrix corresponds to a node and each column to a cascade. 
Cell entries are event times. Censored nodes have \code{NA} for their entry.
}
\description{
Generates a \code{\link{matrix}} containing the cascade information in the 
cascade object in wide format. Missing values are used for nodes that do not 
experience an event in a cascade.
}
\examples{

data(cascades) 
as.matrix(cascades)

}
