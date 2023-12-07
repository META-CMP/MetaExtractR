#' Get Critical value
#'
#' This function computes the critical value given a confidence level.
#' 
#' @param confidence_level A numeric value between 0 and 1, representing the confidence level.
#' 
#' @return A numeric value representing the critical value.
#' 
#' @examples
#' get_crit_val(0.68)
#' get_crit_val(0.95)
#' 
#' @export
get_crit_val <- function(confidence_level) {
  
  # Check that the confidence_level is between 0 and 1
  if (confidence_level <= 0 | confidence_level >= 1) {
    stop("confidence_level should be between 0 and 1.")
  }
  
  # Calculate the critical value
  crit_val <- abs( qnorm((1 - confidence_level) / 2) )
  crit_val <- round(crit_val, digits = 3)
  return(crit_val)
}
