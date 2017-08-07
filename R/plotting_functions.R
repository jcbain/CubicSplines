#' @rdname stat_cubic_spline
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Stat
#' @export

StatCubicSpline <- ggplot2::ggproto("StatCubicSpline", ggplot2::Stat,
                                    required_aes = c("x"),
                                    compute_group = function(data, scales, minval, maxval, break_size) {
                                      grid <- data.frame(x = data$x)

                                      spline_funct <- cubicsplines::cubic_spline(grid$x,min=minval, max = maxval, break_size = break_size )
                                      grid$y <- cubicsplines::spline_adjustment(grid$x, spline_funct(grid$x,deriv = 1))

                                      grid
                                    })

#' Plot the cubic spline density
#'
#' This function is to be added as a stat_ to a ggplot2 plot.
#' It creates a density plot calculated from the cubic spline
#' of the cumulative frequency of a variable.
#' @param minval The minimum value in a range of values
#' @param maxval The maximum value in a range of values
#' @param break_size The size of each break
#' @rdname stat_cubicspline
#' @importFrom ggplot2 layer
#' @export
stat_cubicspline <- function(mapping = NULL, data = NULL, geom = "line",
                             position = "identity", na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, minval = 0, maxval = 100, break_size = 10, ...) {
  ggplot2::layer(
    stat = StatCubicSpline, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(minval = minval, maxval = maxval, break_size = break_size, na.rm = na.rm, ...)
  )
}
