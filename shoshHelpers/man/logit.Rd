% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logit.R
\name{logit}
\alias{logit}
\title{Logit Function}
\usage{
logit(x)
}
\arguments{
\item{x}{numeric between 0 and 1}
}
\value{
numeric on the real line
}
\description{
This function maps a number x between 0 and 1 to logit(x) = log(x/(1-x))
}
\examples{
logit(.3)
-0.8472979
}
\keyword{logit}
