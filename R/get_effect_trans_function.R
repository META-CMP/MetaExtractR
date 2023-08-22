#' Create Effect Size Transformation and SE Functions for the Respective Case
#'
#' This function creates a function called `trans_function()` that represents 
#' the correct transformation function and a function called `se_function()` that
#' represents the correct standard error approximation based on the provided 
#' observation d from a dataframe created via the `join_irf_json()` function.
#'
#' @param d An observation (i.e. a row) from a dataframe obtained using `join_irf_json()`.
#'
#' @return A list containing a function called `trans_function()` an a function 
#'  called `se_function()` as elements.
#'   
#' @examples
#' # Assuming d is given:
#' trans_function <- get_effect_trans_function(d)$trans_function
#' se_function <- get_effect_trans_function(d)$se_function
#' 
#' @export
get_effect_trans_function <- function (d) {

  # Get specifications of shock and outcome variable
  shock_code <- d$inttype
  dep_code <- d$outcome_var
  cum <- d$cum
  
  if (dep_code == "rate") {
    
    # If outcome variable is "rate": TO DO: Which transformation in this case???
    trans_function <- function (x, shock_size) x # PLACEHOLDER 
    se_up_function <- function (mean, up, crit_val) mean # PLACEHOLDER 
    se_low_function <- function (mean, low, crit_val) mean # PLACEHOLDER 
    
  } else {
    
    if (grepl("log_", dep_code) & grepl("lev_", shock_code) & cum == FALSE) {
      
      # If case 1: Log-level of response variable and level of shock variable, non-cumulative
      trans_function <- function (x, shock_size) x / shock_size * 100 # case 1 function
      se_up_function <- function (mean, up, crit_val) abs(up - mean) / crit_val
      se_low_function <- function (mean, low, crit_val) abs(low - mean) / crit_val
      
    } else if (grepl("gr_", dep_code) & grepl("lev_", shock_code) & cum == FALSE) {
      
      # If case 2: Growth rate of response variable and level of shock variable, non-cumulative
      trans_function <- function (x, shock_size) x # PLACEHOLDER TO DO: replace with case 2 function
      se_up_function <- function (mean, up, crit_val) x # PLACEHOLDER TO DO: replace with case 2 SE function 
      se_low_function <- function (mean, low, crit_val) x # PLACEHOLDER TO DO: replace with case 2 SE function 
      
    } else if (grepl("logdiff_", dep_code) & grepl("lev_", shock_code)  & cum == FALSE) {
      
      # If case 3: Log-difference of response variable and level of shock variable, non-cumulative
      trans_function <- function (x, shock_size) x # PLACEHOLDER TO DO: replace with case 3 function 
      se_up_function <- function (mean, up, crit_val) mean # PLACEHOLDER TO DO: replace with case 3 SE function 
      se_low_function <- function (mean, low, crit_val) mean # PLACEHOLDER TO DO: replace with case 3 SE function 
      
    } else if (grepl("logdiff_", dep_code) | grepl("gr_", dep_code) & grepl("lev_", shock_code)  & cum == TRUE) {
      
      # If case 5: Log-difference or growth rate of response variable and level of shock variable, cumulative
      trans_function <- function (x, shock_size) x # PLACEHOLDER TO DO: replace with case 5 function 
      se_up_function <- function (mean, up, crit_val) x # PLACEHOLDER TO DO: replace with case 5 SE function 
      se_low_function <- function (mean, low, crit_val) x # PLACEHOLDER TO DO: replace with case 5 SE function 
    }
    
  }
  
  l <- list(
    "trans_function" = trans_function,
    "se_up_function" = se_up_function,
    "se_low_function" = se_low_function
  )
  
  return(l)
  
}
