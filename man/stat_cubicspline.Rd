% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_functions.R
\name{stat_cubicspline}
\alias{stat_cubicspline}
\title{Plot the cubic spline density}
\usage{
stat_cubicspline(mapping = NULL, data = NULL, geom = "line",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE, minval = 0, maxval = 100, break_size = 10,
  adjust_to_zero = FALSE, to_density = TRUE, ...)
}
\arguments{
\item{minval}{The minimum value in a range of values}

\item{maxval}{The maximum value in a range of values}

\item{break_size}{The size of each break}

\item{adjust_to_zero}{If minimum derivative is < 0 update derivatives to derivatives + min(derivatives)}

\item{to_density}{Adjust derivatives so that AUC = 1}
}
\description{
This function is to be added as a stat_ to a ggplot2 plot.
It creates a density plot calculated from the cubic spline
of the cumulative frequency of a variable.
}
