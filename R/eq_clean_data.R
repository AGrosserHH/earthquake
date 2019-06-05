#' Cleaning of earthquake data
#'
#' This is a function that reads the data from a the csv file containing the earthquake data. It create a new date column, cleans the names where the earthquake
#' occured and converts longitude and latitude values in to numeric types.
#'
#' @return This function returns the earthquake data as a list
#'
#' @param text name of data set as string
#'
#' @importFrom readr read_delim
#' @importFrom tidyr unite
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' data<-eq_clean_data("earthquakes_data.txt.zip")
#' }
#'
#' @export

eq_clean_data<-function(text){

  data_in <-readr::read_delim(text,col_names=T,delim = "\t",na = "-99")
  str(data_in)

  data_in <- data_in %>% tidyr::unite(datetime, YEAR,MONTH,DAY)%>% dplyr::mutate(datetime = ymd(datetime), date = format(datetime,"%b %d"))
  data <- data_in%>%dplyr::filter(!is.na(datetime))

  data$LATITUDE <- as.numeric(data$LATITUDE)
  data$LONGITUDE <- as.numeric(data$LONGITUDE)
  data$LOCATION <- gsub( ":.*$", "", data$LOCATION_NAME)
  data$LOCATION <- paste(data$LOCATION)

  data$EQ_PRIMARY <- as.numeric(data$EQ_PRIMARY)
  data$TOTAL_DEATHS = as.numeric(data$TOTAL_DEATHS)
  data
}

