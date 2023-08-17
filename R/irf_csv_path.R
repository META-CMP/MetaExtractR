#' Generate IRF component CSV path for a specific outcome variable
#'
#' This function generates the file path for the CSV containing an impulse response 
#' function (IRF) component (mean effect, upper bound or lower bound) for a 
#' particular outcome variable.
#'
#' @param key A character string representing the unique key of the study from which the 
#'  IRF was extracted.
#' @param folder_path A character string specifying the folder path of the IRF CSV files.
#' @param model A character string specifying the study-level model identifier.
#' @param outcome_var A character string indicating the name of the outcome variable.
#' @param irf_part A character string indicating the desired IRF component. It should be one of "lower", 
#'  "mean", or "upper".
#'
#' @return A character string representing the generated file path for the IRF component CSV.
#'
#' @examples
#' irf_csv_path(
#'   key = "QSKR5KKX",
#'   folder_path = 
#'     "/Users/franzprante/GitHub/MORPEP/META_CMP/toy_data_extraction_dev/data/effect_sizes/IRFs/", 
#'   model = "1", 
#'   outcome_var = "log_q_rgdp", 
#'   irf_part = "mean")
#'
#' @import here
#'
#' @export
irf_csv_path <- function (key,
                          folder_path = "data/effect_sizes/IRFs/",
                          model, 
                          outcome_var,
                          irf_part = c("lower", "mean", "upper")
) {
  csv_path <- paste0(folder_path, key, "/model_", model, "/", outcome_var, "/", irf_part, ".csv")
  csv_path <- here::here(csv_path)
  csv_path
}
