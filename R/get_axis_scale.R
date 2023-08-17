#' Get y-axis scaling function
#'
#' This helper function extracts the y-axis scaling value from an observation (i.e., a row) in a dataframe
#' obtained using MetaExtractR::join_irf_json() and returns a function that can be used to apply the axis 
#' scaling to the IRF data (mean effect and confidence bounds).
#'
#' @param d An observation (i.e., a row) from a dataframe obtained using MetaExtractR::join_irf_json().
#'
#' @return The function creates an R function object called "axis_scale()" and a parameter "y_scale" representing 
#' the y-axis scaling. The axis_scale() function can then be used in the scaling functions.
#'
#' @examples
#' # Assuming d is a line from a dataframe obtained from MetaExtractR::join_irf_json()
#' axis_scaling <- get_axis_scale(d)$axis_scaling_function # Stores the function
#' y_scale <- get_axis_scale(d)$scaling_parameter # Stores the parameter (only for inspection, not necessary)
#'
#' @export
get_axis_scale <- function (d) {
  
  axis_scale <- d$axis_trans
  
  if (axis_scale == FALSE) {
    
    y_scale <- 1
    
  } else if (grepl("y\\*", axis_scale)) {
    
    y_scale <- as.double(sub("y\\*", "", axis_scale))
    
  } else if (grepl("y/", axis_scale)) {
    
    y_scale <- 1 / as.double(sub("y/", "", axis_scale))
    
  } else {
    
    stop("Axis scaling unclear. Check axis_trans entry in JSON file for typo.")
    
  }
  
  # Create the axis scaling function 
  axis_scale <- function (x) x * y_scale
  
  # Return the function and the parameter in a list
  l <- list("axis_scaling_function" = axis_scale,
            "scaling_parameter" = y_scale)
  return(l)
}
