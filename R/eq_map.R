#' Mapping of earthquake data
#'
#' This function plots the earthquake and adds some annotations to it
#' @param data a cleaned data file
#' @param annot_col name of column used for annotations
#'
#' @return map of the earthquakes including annotations
#'
#' @examples
#' \dontrun{
#' readr::read_delim("earthquakes_data.txt.zip", delim = "\t") %>%
#'  eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(datetime) >= 2000)%>%
#'   eq_map(annot_col = "datetime")
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom leaflet addTiles
#'
#' @export

eq_map <-function(data, annot_col){

  data%>%
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lat=~LATITUDE,lng=~LONGITUDE, popup=~data[[annot_col]], fillOpacity = 0.5, stroke = FALSE, radius = ~ EQ_PRIMARY)
}

#' More interesting annotations for mapping earthquake data
#'
#' @param data a cleaned data file
#'
#' @return an HTML label to be used in the map
#'
#' @examples
#'
#' \dontrun{
#'
#' data %>%
#'   dplyr::filter(!is.na(EQ_PRIMARY)) %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(datetime) >= 2000)   %>%
#'   dplyr::mutate(conte = eq_create_label(.)) %>%
#'   eq_map(annot_col = "conte")
#'
#' }
#'
#' @export

eq_create_label <- function(data){
  content <- paste(ifelse(is.na(data$LOCATION), "", paste("<b>LOCATION</b>: ",data$LOCATION, "<br>")),ifelse(is.na(data$EQ_PRIMARY), "", paste("<b>MAGNITUDE</b>: ",data$EQ_PRIMARY, "<br>")),ifelse(is.na(data$TOTAL_DEATHS), "", paste("<b>Total deaths</b>: ",data$TOTAL_DEATHS, "<br>")))
  content
}
