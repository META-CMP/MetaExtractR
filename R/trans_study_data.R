#' Standardization and Effect Size Transformation of Study Coding and Effect Size Data
#'
#' This function standardizes and transforms study-level data obtained from JSON 
#' and CSV files. It applies standardization to effect sizes and adjusts 
#' time periods to align quarterly and annual data to a monthly format. The function 
#' applies helper functions: `MetaExtractR::effect_trans_se_function()`, 
#' `MetaExtractR::adjust_period`.
#' 
#'
#' @param study_data A dataframe containing moderator variables and effect sizes. This data is typically created from JSON and CSV files via `MetaExtractR::join_irf_json()`.
#' @return A dataframe where each observation has standardized effect sizes and adjusted periods. The structure of the returned dataframe is similar to the input, but with transformed values.
#' @examples
#' # Assuming 'raw_study_data' is a dataframe obtained from MetaExtractR::join_irf_json
#' transformed_data <- trans_study_data(raw_study_data)
#'
#' @export

trans_study_data <- function (study_data) {
  
  trans.data <- data.frame(NULL)
  
  n <- nrow(study_data)
  
  for (i in 1:n) {
    
    # Extract a single observation
    d <- study_data[i,]
    
    # Calculate standardized effect sizes and standard errors
    d <- effect_trans_se_function(d)

    # Adjust the period to match quarterly and annual with monthly data.
    d$period.month <- adjust_period(d)

    # TO DO:
    # If necessary, adjust the outcome variables (including rate) for annualization
    # ... see the comment in adjust_shock_periodicity  (can this be done in the end?)

    trans.data <- rbind(trans.data, d)

  }
  
  # Return transformed data
  return(trans.data)
  
}







