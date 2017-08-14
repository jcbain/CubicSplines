#' Default plot theme for Oberd in ggplot2
#'
#' @export

theme_oberd <- function() {

  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- RColorBrewer::brewer.pal("Greys", n=9) # create a palette of shades of grey
  color.background = palette[1] # background plot color
  color.grid.major = palette[3]
  color.grid.minor = palette[2]
  color.axis.text = palette[6] # text color
  color.axis.title = palette[7]
  color.title = palette[9]

  # Begin construction of chart
  ggplot2::theme_bw(base_size=9) +

    # Set the entire chart region to a white color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border = element_blank()) +
    theme(axis.line = element_line(colour = "black")) +

    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +

    # Format the legend, but hide by default
    theme(legend.position="bottom") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +

    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color="#165E85", size=18, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +

    # Set facet attributes
    theme(strip.background = element_rect(fill=color.background, colour = color.background)) +
    theme(strip.text = element_text(size = 15, colour = color.axis.text)) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

#' Dark plot theme for Oberd in ggplot2
#'
#' @export
theme_oberd_dark <- function() {

  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- RColorBrewer::brewer.pal("Greys", n=9) # create a palette of shades of grey
  color.background = "#5B677F" # background plot color
  color.grid.major = palette[4]
  color.grid.minor = palette[4]
  color.axis.text = palette[4] # text color
  color.axis.title = palette[4]
  color.title = palette[4]

  # Begin construction of chart
  ggplot2::theme_bw(base_size=9) +

    # Set the entire chart region to a white color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border = element_blank()) +
    theme(axis.line = element_line(colour = color.grid.major)) +

    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.1)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +

    # Format the legend, but hide by default
    theme(legend.position="bottom") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.grid.major)) +
    theme(legend.title = element_text(size=7,color=color.grid.major)) +

    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.grid.major, size=18, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.grid.major)) +
    theme(axis.text.y=element_text(size=7,color=color.grid.major)) +
    theme(axis.title.x=element_text(size=8,color=color.grid.major, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.grid.major, vjust=1.25)) +

    # Set facet attributes
    theme(strip.background = element_rect(fill=color.background, colour = color.background)) +
    theme(strip.text = element_text(size = 15, colour = color.axis.text)) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}


#' Default discrete geom colors for oberd
#'
#' @param theme the colors scheme to go with the theme
#' @param o_key list of different color schemes
#' @export
scale_color_oberd <- function(theme="dark", o_key = list(
  dark = c( "#165E85","#94979D", "#161D27","#5B677F"  ),
  light = c("#0093CA", "#DBF733", "#FFFFFF", "#94979D")
)) {

  ggplot2::scale_color_manual(values=o_key[[theme]])

}

#' Default continuous geom colors for oberd
#'
#' @param theme the colors scheme to go with the theme
#' @param o_key list of different color schemes
#' @export
scale_color_gradient_oberd <- function(theme = "dark", o_key = list(
  dark = c("#165E85","#DBF733"),
  light = c("#0093CA","#DBF733")

)) {
  ggplot2::scale_color_gradient(low = o_key[[theme]][1], high = o_key[[theme]][2])
}
