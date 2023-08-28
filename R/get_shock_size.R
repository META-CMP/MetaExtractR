#' Convert Shock Size to Basis Points
#'
#' This function takes an observation from a dataframe obtained using MetaExtractR::join_irf_json()
#' and converts the shock size to basis points based on different formats of the "size" entry
#' and applies  y-axis scaling if necessary.
#'
#' @param d An observation (i.e., a row) from a dataframe obtained using MetaExtractR::join_irf_json().
#' @param study_data The full dataframe containing the study-related data, including d.
#' 
#' @return The shock size in basis points.
#'
#' @examples
#' # Example usage:
#' shock_size <- get_shock_size(d, study_data)
#'
#' @export
get_shock_size <- function(d, study_data) {
  
  # Get shock entry for observation
  shock_size <- d$size
  
  if (grepl("bp", shock_size)) { 
    
    # If basis point shock is directly available.
    shock_size <- as.double(sub("bp", "", shock_size))
    
  } else if (grepl("SD", shock_size) | is.na(shock_size)) { 
    
    # If we need to approximate the shock by the initial value of the IRF 
    # of the corresponding shock variable ("rate")
    
    # Get raw shock size from rate IRF
    shock_size <- study_data[
      study_data$key == d$key & 
        study_data$model_id == d$model_id & 
        study_data$outcome_var == "rate" & 
        study_data$period == 1, "mean.effect.raw"]
    
    # TO DO: (maybe) 
    # Effect size transformation necessary for shock size? 
    # At the moment, we never code studies where the shock variable is not in levels. 
    # Before final analyses of data, do final check if we changed this rule and add 
    # transformation if so.
    
    # y-axis scaling function for the shock variable
    # Get the data for shock observation
    shock_row <- study_data[
      study_data$key == d$key & 
        study_data$model_id == d$model_id & 
        study_data$outcome_var == "rate" & 
        study_data$period == 1, ]
    # Store the scaling function
    axis_scaling_shock <- get_axis_scale(shock_row)$axis_scaling_function 
    # Apply axis scaling to shock size (to percentage points)
    shock_size <- axis_scaling_shock(shock_size)
    # Get basis points
    shock_size <- shock_size*100
    
  }
  
  return(shock_size)
  
}

  