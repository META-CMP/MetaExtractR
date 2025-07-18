% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_shock_size.R
\name{get_shock_size}
\alias{get_shock_size}
\title{Convert Shock Size to Basis Points}
\usage{
get_shock_size(d, study_data = study_data)
}
\arguments{
\item{d}{An observation (i.e., a row) from a dataframe obtained using MetaExtractR::join_irf_json().}

\item{study_data}{The full dataframe containing the study-related data, including d.}
}
\value{
The shock size in basis points.
}
\description{
This function takes an observation from a dataframe obtained using MetaExtractR::join_irf_json()
and converts the shock size to basis points based on different formats of the "size" entry
and applies  y-axis scaling if necessary.
}
\examples{
# Example usage:
shock_size <- get_shock_size(d, study_data)

}
