% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq_map.R
\name{eq_create_label}
\alias{eq_create_label}
\title{More interesting annotations for mapping earthquake data}
\usage{
eq_create_label(data)
}
\arguments{
\item{data}{a cleaned data file}
}
\value{
an HTML label to be used in the map
}
\description{
More interesting annotations for mapping earthquake data
}
\examples{

\dontrun{

data \%>\%
  dplyr::filter(!is.na(EQ_PRIMARY)) \%>\%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(datetime) >= 2000)   \%>\%
  dplyr::mutate(conte = eq_create_label(.)) \%>\%
  eq_map(annot_col = "conte")

}

}
