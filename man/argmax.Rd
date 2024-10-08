% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{argmax}
\alias{argmax}
\title{Return index and value of maximum}
\usage{
argmax(x, decreasing = TRUE)
}
\arguments{
\item{x}{Object to have its maximum found}

\item{decreasing}{Logical value indicating whether \code{x} should be
ordered in decreasing order. Default is \code{TRUE}. Setting this to
\code{FALSE} would find the minimum.}
}
\value{
The maximum value and its index.
}
\description{
Something similar to Python's argmax.
}
\examples{
library(tsgc)
data(gauteng,package="tsgc")
argmax(gauteng)
}
