#' Adjust shock_size based on the periodicity of interest rate.
#'
#' This function adjusts the shock size based on the periodicity of the interest rate.
#'
#' @param d An observation (i.e., a row) from a dataframe obtained using MetaExtractR::join_irf_json().
#'
#' @return Annualized shock size.
#'
#' @export
adjust_shock_periodicity <- function (d, shock_size) {
  
  periodicity <- d$inttype
  
  if (grepl("_a_", periodicity) == TRUE) {
    
    # No transformation necessary
    message("Annualized interest rate. No periodicity transformation necessary for shock size.")
    shock_size <- shock_size
    
  } else if (grepl("_q_", periodicity) == TRUE) {
    
    # If quarterly periodicity, transform to annualized
    shock_size <- ( (1 + (shock_size/10000) )^4 - 1)*10000
    
  } else if (grepl("_m_", periodicity) == TRUE) {
    
    # If monthly periodicity, transform to annualized
    shock_size <- ((1+(shock_size/10000))^12-1)*10000
    
  }
  
  return(shock_size)
  
}
  
# TO DO: 
  # Comment ME: 
  # but how to handle the interest rate response since this one should be
  # multiplied by the same factor and not divided?
  # -------------------------------------------------------------------------------
    