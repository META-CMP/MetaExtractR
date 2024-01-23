#' Adjust shock_size based on the periodicity of interest rate.
#'
#' This function adjusts the shock size based on the periodicity of the interest 
#' rate. If the periodicity is monthly or quarterly, the function transforms it 
#' to annualized.
#'
#' @param d An observation (i.e., a row) from a dataframe obtained using MetaExtractR::join_irf_json().
#' @param shock_size A value for the shock size in basis points obtained with MetaExtractR::get_shock_size()
#'
#' @return Annualized shock size.
#'
#' @export
adjust_shock_periodicity <- function (d, shock_size) {
  
  periodicity <- d$inttype
  # Testing if "lev_" and periodicity are included in inttype entry.
  if (grepl("lev_", periodicity) == FALSE) {
    stop('No "lev_" found in inttype entry. Check JSON.', d$key, " model_", d$model_id, " Shock variable: ", d$inttype)
  }
  if (grepl("_a_", periodicity) == FALSE & grepl("_q_", periodicity) == FALSE & grepl("_m_", periodicity) == FALSE) {
    stop('No periodictiy index found in inttype entry. Check JSON. ', d$key, " model_", d$model_id, " Shock variable: ", d$inttype)
  }
  
  if (grepl("_a_", periodicity) == TRUE) {
    
    # If annualized interest rate, no transformation necessary
    message("Annualized interest rate. No periodicity transformation necessary for shock size. ", d$key, " model_", d$model_id, " Shock variable: ", d$inttype, " Outcome: ", d$outcome_var)
    shock_size <- shock_size
    
  } else if (grepl("_q_", periodicity) == TRUE) { 
    # If quarterly periodicity, transform to annualized
    message("Quarterly interest rate. Transformation to annualized. ", d$key, " model_", d$model_id, " Shock variable: ", d$inttype, " Outcome: ", d$outcome_var)
    shock_size <- ( (1 + (shock_size/10000) )^4 - 1)*10000
    
  } else if (grepl("_m_", periodicity) == TRUE) {
    
    # If monthly periodicity, transform to annualized
    message("Monthly interest rate. Transformation to annualized. ", d$key, " model_", d$model_id, " Shock variable: ", d$inttype, " Outcome: ", d$outcome_var)
    shock_size <- ( (1 + (shock_size/10000) )^12 - 1)*10000
    
  }
  
  return(shock_size)
  
}