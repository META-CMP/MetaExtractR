% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/irf_csv_path.R
\name{irf_csv_path}
\alias{irf_csv_path}
\title{Generate IRF component CSV path for a specific outcome variable}
\usage{
irf_csv_path(
  key,
  folder_path = "data/effect_sizes/IRFs/",
  model,
  outcome_var,
  irf_part = c("lower", "mean", "upper")
)
}
\arguments{
\item{key}{A character string representing the unique key of the study from which the 
IRF was extracted.}

\item{folder_path}{A character string specifying the folder path of the IRF CSV files.}

\item{model}{A character string specifying the study-level model identifier.}

\item{outcome_var}{A character string indicating the name of the outcome variable.}

\item{irf_part}{A character string indicating the desired IRF component. It should be one of "lower", 
"mean", or "upper".}
}
\value{
A character string representing the generated file path for the IRF component CSV.
}
\description{
This function generates the file path for the CSV containing an impulse response 
function (IRF) component (mean effect, upper bound or lower bound) for a 
particular outcome variable.
}
\examples{
irf_csv_path(
  key = "QSKR5KKX",
  folder_path = 
    "/Users/franzprante/GitHub/MORPEP/META_CMP/toy_data_extraction_dev/data/effect_sizes/IRFs/", 
  model = "1", 
  outcome_var = "log_q_rgdp", 
  irf_part = "mean")

}
