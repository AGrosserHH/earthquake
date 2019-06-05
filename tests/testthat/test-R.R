context("Test Earthquake package")

library(readr)
library(dplyr)
library(leaflet)
library(lubridate)

setwd(system.file("extdata", package = "earthquake"))

filename <- "earthquakes_data.txt.zip"
data_test<-eq_clean_data(filename)
head(data_test)

# Testing of eq_clean_data
testthat::expect_that(data_test$datetime, testthat::is_a('Date'))
testthat::expect_that(data_test$LATITUDE, testthat::is_a('numeric'))
testthat::expect_that(data_test$LONGITUDE, testthat::is_a('numeric'))

# Testing of eq_map
testthat::expect_that(data_test %>%
                         dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(datetime) >= 2000)%>%
                         eq_map(annot_col = "datetime"), testthat::is_a('leaflet'))

# Testing of eq_map_label
testthat::expect_that(eq_create_label(data_test), testthat::is_a('character'))

testthat::expect_that(data_test %>%
                        dplyr::filter(!is.na(EQ_PRIMARY)) %>%
                        dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(datetime) >= 2000)   %>%
                        dplyr::mutate(conte = eq_create_label(.)) %>%
                        eq_map(annot_col = "conte"), testthat::is_a('leaflet'))


