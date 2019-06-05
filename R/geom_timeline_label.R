#' Adding location of earthquake to display earthquakes
#'
#' This is a ggplot function that adds the location of earthquakes to each data point. By using the parameter n_max, only the
#' location with the n highest number of manitude are selected.
#'
#' @return The geom geom_timeline_label used with the ggplot function, produces the location of the earthquake
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
#'  mutate(EQ_PRIMARY = round(EQ_PRIMARY)) %>%
#'  filter(datetime>"2010-01-01" & datetime<"2012-01-01")%>%
#'  ggplot(aes(x=datetime, color=DEATHS, size = EQ_PRIMARY, label = LOCATION))+
#'  geom_timeline(alpha=.8)+geom_timeline_label(n_max=5)
#' }
#'
#' @export

geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' GeomTimelineLabel
#'
#' Coding of Geom GeomTimeline
#'
#' @importFrom ggplot2 aes ggproto Geom layer
#' @importFrom grid gList textGrob segmentsGrob gpar
#' @importFrom dplyr arrange slice
#'
#' @export

GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                             required_aes = c("x", "label"),
                             default_aes = ggplot2::aes(y = 0, y_length = 2.0, n_max = 0),
                             draw_key = ggplot2::draw_key_point,
                             draw_panel = function(data, panel_scales, coord) {

                               ## Transform data
                               if (data$n_max[1]>0){
                                 data <- data %>%
                                   dplyr::arrange(desc(size)) %>%
                                   dplyr::slice(1:data$n_max[1])
                               }

                               coords <- coord$transform(data, panel_scales)
                               grid::gList(
                                 grid::segmentsGrob(x0=coords$x, y0= coords$y, x1 = coords$x, y1 = (.2/coords$y_length),
                                                    gp = grid::gpar(col = "blue", lwd = 1)),
                                 grid::textGrob(coords$label, x = coords$x, y = (.2/coords$y_length),just = "left", rot = 45)
                               )
                             }
)


