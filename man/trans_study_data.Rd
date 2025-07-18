% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trans_study_data.R
\name{trans_study_data}
\alias{trans_study_data}
\title{Standardization and Effect Size Transformation of Study Coding and Effect Size Data}
\usage{
trans_study_data(study_data)
}
\arguments{
\item{study_data}{A dataframe containing moderator variables and effect sizes. This data is typically created from JSON and CSV files via `MetaExtractR::join_irf_json()`.}
}
\value{
A dataframe where each observation has standardized effect sizes and adjusted periods. The structure of the returned dataframe is similar to the input, but with transformed values.
}
\description{
This function standardizes and transforms study-level data obtained from JSON 
and CSV files. It applies standardization to effect sizes and adjusts 
time periods to align quarterly and annual data to a monthly format. The function 
applies helper functions: `MetaExtractR::effect_trans_se_function()`, 
`MetaExtractR::adjust_period`.
}
\examples{
# Assuming 'raw_study_data' is a dataframe obtained from MetaExtractR::join_irf_json
transformed_data <- trans_study_data(raw_study_data)

}
