% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flog.R
\name{flog}
\alias{flog}
\title{Transform proportion to Tukey's Folded Log
converts from proportion to odds-like continuous scale.
Useful for non-bounded parameters.
Presented as curtailed flog, where -Inf and Inf
(corresponding to 0 and 1) are replaced with the
minimum real value - interquartile range and
maximum real value + interquartile range respectively}
\usage{
flog(p)
}
\arguments{
\item{p}{numeric vector in range (0, 1)}
}
\value{
numeric vector
}
\description{
Transform proportion to Tukey's Folded Log
converts from proportion to odds-like continuous scale.
Useful for non-bounded parameters.
Presented as curtailed flog, where -Inf and Inf
(corresponding to 0 and 1) are replaced with the
minimum real value - interquartile range and
maximum real value + interquartile range respectively
}
\examples{
flog(p = c(0, 0.017, 0.029, 0.036, 0.045, 0.531, 1))
}
\references{
http://www.sumsar.net/blog/2013/09/going-to-plot-some-proportions/
}
