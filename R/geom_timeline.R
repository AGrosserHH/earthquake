#' Plotting the earthquake data along time axis
#'
#' This is a ggplot function that displays the data of earthquakes along an x-axis. The magnitude of the earthquake is displayed via the circle radius
#' of each single data point. The color of each circle is symbolide based on the number of total deaths per earthquake
#'
#' @return The geom \code{geom_timeline} used with the \code{ggplot} function, produce a time line plot of selected earthquakes.
#'
#' @inheritParams ggplot2::layer
#'
#' @param na.rm if set to 'FALSE' the missing values are removed with a warning.
#'    If set to `TRUE`, the values are removed silently.
#' @param ... These are other arguments passed to [layer()]. E.g. this can be 'color=red'.
#'
#' @examples
#' \dontrun{
#' data %>%
#'    mutate(EQ_PRIMARY = round(EQ_PRIMARY)) %>%
#'    filter(datetime>"2010-01-01" & datetime<"2012-01-01")%>%
#'    ggplot(aes(x=datetime, color=DEATHS, size = EQ_PRIMARY, label = LOCATION))+
#'    geom_timeline(alpha=.8)
#' }
#'
#' @export

geom_timeline<- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' GeomTimeline
#'
#' Coding of Geom GeomTimeline
#'
#' @importFrom ggplot2 aes ggproto Geom layer
#' @importFrom grid gList pointsGrob segmentsGrob gpar
#'
#' @export

GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                        required_aes = c("x"),
                        default_aes = ggplot2::aes(y = 0, colour = "grey", size = 1.0,
                                                   alpha = 0.6, shape = 21, fill = "grey", stroke = 1.0),
                        draw_key = ggplot2::draw_key_point,
                        draw_panel = function(data, panel_scales, coord) {

                          coords <- coord$transform(data, panel_scales)
                          grid::gList(
                            grid::pointsGrob(x = coords$x, y= coords$y,
                                             pch = coords$shape,
                                             gp = grid::gpar(col = alpha(coords$colour, coords$alpha),
                                                             fill = alpha(coords$fill, coords$alpha),
                                                             fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                                             lwd = coords$stroke * .stroke / 2)
                            ),
                            grid::segmentsGrob(x0=min(coords$x), y0= coords$y, x1 = max(coords$x), y1 = coords$y,
                                               gp = grid::gpar(col = "blue", lwd = 1))
                          )
                        }
)


