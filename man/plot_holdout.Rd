% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_holdout}
\alias{plot_holdout}
\title{Plots the forecast of new cases (the difference of the cumulated
variable) over a holdout sample.}
\usage{
plot_holdout(
  res,
  Y,
  Y.eval,
  confidence.level = 0.68,
  date_format = "\%Y-\%m-\%d",
  series.name = NULL,
  title = NULL,
  caption = NULL
)
}
\arguments{
\item{res}{Results object estimated using the \code{estimate()} method.}

\item{Y}{Values of the cumulated variable to be used in the estimation
window.}

\item{Y.eval}{Values of the cumulated variable to be used in the holdout
sample (i.e. to which the forecasts should be compared to).}

\item{confidence.level}{Width of prediction interval for \eqn{\ln(g_t)} to
use in forecasts of \eqn{y_t = \Delta Y_t}. Default is 0.68, which is
approximately one standard deviation for a Normal distribution.}

\item{date_format}{Date format, e.g. \code{'\%Y-\%m-\%d'}, which is the
default.}

\item{series.name}{Name of the variable you are forecasting for the purposes
of a $y$-axis label. E.g. if \code{series.name = "Cases"} the \eqn{y}-axis
will show "New Cases".}

\item{title}{Title for forecast plot. Enter as text string. \code{NULL}
(i.e. no title) by default.}

\item{caption}{Caption for forecast plot. Enter as text string. \code{NULL}
(i.e. no caption) by default.}
}
\value{
A \code{ggplot2} plot.
}
\description{
Plots actual values of the difference in the cumulated variable,
the forecasts of the cumulated variable (both including and excluding the
seasonal component, where a seasonal is specified) and forecast intervals
around the forecasts, plus the actual outcomes from the holdout sample. The
forecast intervals are based on the prediction intervals for \eqn{\ln(g_t)}.
Also reports the mean absolute percentage prediction error over the holdout
sample.
}
\examples{
library(tsgc)
data(gauteng,package="tsgc")
idx.est <- zoo::index(gauteng) <= as.Date("2020-07-20")
idx.eval <- (zoo::index(gauteng) >= as.Date("2020-07-20")) &
     zoo::index(gauteng) <= as.Date("2020-07-27")

# Specify a model
model <- SSModelDynamicGompertz$new(Y = gauteng[idx.est], q = 0.005)
# Estimate a specified model
res <- model$estimate()

# Plot forecasts and outcomes over evaluation period
plot_holdout(res = res, Y = gauteng[idx.est], Y.eval = gauteng[idx.eval])

}
