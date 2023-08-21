#' Adjust Period to Monthly Equivalent
#'
#' This function adjusts a given period value to its equivalent in months based on the provided frequency indicators from the coding.
#'
#' @param d A data frame containing the following logical indicators: \code{month}, \code{quarter}, \code{annual}.
#'
#' @return An integer vector representing the adjusted period in months.
#'
#' @examples
#' d$period.month <- adjust_period(d)
#'
#' @export
adjust_period <- function (d) {
  if (d$month == TRUE & d$quarter == FALSE & d$annual == FALSE) {
    
    # If IRF data is already monthly, keep it:
    period.month <- d$period
    
  } else if (d$month == FALSE & d$quarter == TRUE & d$annual == FALSE) {
    
    # If IRF data is quarterly:
    period.month <- d$period * 3
    
  } else if (d$month == FALSE & d$quarter == FALSE & d$annual == TRUE) {
    
    # If data is yearly:
    period.month <- d$period * 12
    
  } else {
    
    stop("Check frequency entry (month, quarter, annual in JSON) in coding data.")
    
  }
  
  return(period.month)
  
}