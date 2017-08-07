#' Find Function using Cubic Spline Interpolation
#'
#' This performs cubic spline interpolation over the cumulative
#' sum of frequencies of a variable.
#' @param x The variable to perform the spline over
#' @param break_size The width of each break for the cumulative sum of frequencies applied to x
#' @param min The minimum value for the break sequence
#' @param max The maximum value for the break sequence
#' @return a cubic spline interpolation function of x
#' @export
cubic_spline <- function(x,break_size=10,min=0,max=100) {

  duration = x
  breaks = seq(min,max, by = break_size)
  duration.cut = cut(duration,breaks, right = FALSE)
  duration.freq = table(duration.cut)

  duration.cumfreq = cumsum(duration.freq)
  cumfreq0 = c(0,cumsum(duration.freq))

  f_of_x = stats::splinefun(breaks,cumfreq0)

  return(f_of_x)
}


#' Adjust the splines derivative
#'
#' This function adjusts the derivative of a cubic spline so that
#' when plotted against another variable, the AUC is equal to 1
#' @param x The variable to plot the derivative against
#' @param deriv The derivative of the cubic spline
#' @return the adjusted derivative
#' @export
spline_adjustment <- function(x,deriv) {
  id = order(x)
  AUC = sum(diff(x[id])*zoo::rollmean(deriv[id],2))
  return(deriv/AUC)
}
