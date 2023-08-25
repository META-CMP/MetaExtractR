#' Parse Axis Scale String for Scaling Function
#'
#' This function parses a given axis scaling string (created within the get_axis_scale 
#' function) to extract the numerical value and operator for scaling purposes. 
#' It returns a list containing a scaling function and the associated scaling parameter.
#'
#' @param axis_scale A character string specifying the axis scaling, e.g., "y*100", "y/100".
#'   The string should contain a numerical value and an operator (* or /). The function
#'   can also handle some forms of erroneous syntax, such as "y*-1" or "y* / 100".
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{axis_scaling_function}: A function to apply the desired scaling on y-values.
#'   \item \code{scaling_parameter}: A numeric value indicating the scaling factor or multiplier.
#' }
#' 
#' @examples
#' \dontrun{
#' # General scaling for all variables
#' parse_axis_scale_string("y*100")
#' parse_axis_scale_string("y/100")
#' parse_axis_scale_string("y*(-1)")
#' parse_axis_scale_string("y*-1") # Even some erroneous coding syntax (no parenthesis around "-1") is correctly parsed, but "y*- 1" would not work, for example.
#' parse_axis_scale_string("y*0.01")
#' parse_axis_scale_string("y* / 100") # Even some erroneous coding syntax (spaces around "/") is correctly parsed
#' # Specific scaling for one variable
#' parse_axis_scale_string("log_rgdp_y*100")
#' parse_axis_scale_string("log_cpi_y/100")
#' parse_axis_scale_string("log_rgdp_y*(-1)")
#' parse_axis_scale_string("logdiff_ip_y*0.1")
#' parse_axis_scale_string("log_rgdpy*100") # Even some erroneous coding syntax ( no "_" left of "y") is correctly parsed
#' }
#'
#' @export
#' @importFrom stringr str_extract
parse_axis_scale_string <- function (axis_scale) {
  
  # Extract the numerical value of the multiplier or divisor
  parameter <- as.numeric(stringr::str_extract(axis_scale, "-?\\d+(\\.\\d+)?"))
  
  # Extract the operator (*, /, -)
  operator <- stringr::str_extract(axis_scale, "[*/]")
  
  # Now you can apply these values as needed
  if (!is.na(parameter)) {
    if (operator == "*") {
      y_scale <- parameter
    } else if (operator == "/") {
      y_scale <-  1 / parameter
    }
  } else {
    y_scale <- 1
  }
  
  axis_scale_fun <- function (y) y * y_scale
  
  # Store and return the axis_scale_fun function and the parameter in a list
  parsed <- list("axis_scaling_function" = axis_scale_fun,
                 "scaling_parameter" = y_scale)
  
  return(parsed)
  
}



