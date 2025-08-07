#' Get y-axis scaling function and parameter
#'
#' Using another helper function (parse_axis_scale_string()), this helper function 
#' extracts the y-axis scaling value from an observation (i.e., a row) in a 
#' dataframe obtained using MetaExtractR::join_irf_json() and returns a function 
#' that can be used to apply the axis scaling to the IRF data (mean effect and 
#' confidence bounds). The aim is to transform the axis for any variable into percent (if not already the case).
#'
#' @param d An observation (i.e., a row) from a dataframe obtained using MetaExtractR::join_irf_json().
#'
#' @return The function creates a list including a function object called "axis_scaling_function()" and a parameter called "scaling_parameter" representing 
#' the y-axis scaling. The axis_scaling_function() function can then be used in the transformation function.
#'
#'#' @examples
#' # Examples 
#' # Example 1: General transformation
# dep <- "log_q_rgdp"
# axis_trans <- "y*100"
# d <- data.frame(
#   dep = dep,
#   axis_trans = axis_trans
# )
# get_axis_scale(d)
#' # Example 2: Specific transformation for current dep
# axis_trans <- "log_q_rgdp_y*(-1)"
# d <- data.frame(
#   dep = dep,
#   axis_trans = axis_trans
# )
# get_axis_scale(d)
#' # Example 3: General transformation including current dep and specific transformation for one other "dep"
#' axis_trans <- "y*100 log_q_cpi_y/20"
#' d <- data.frame(
#'   dep = dep,
#'   axis_trans = axis_trans
#' )
#' get_axis_scale(d)
#' # Example 4: General transformation excluding current dep and specific transformation for current dep
# axis_trans <- "y*100 log_q_rgdp_y/20"
# d <- data.frame(
#   dep = dep,
#   axis_trans = axis_trans
# )
# get_axis_scale(d)
# Example 5: Specific transformations for other "dep" entries, excluding current dep
# axis_trans <- "log_q_cpi_y*0.1 logdiff_a_fed_funds_y/(-1)"
# d <- data.frame(
#   dep = dep,
#   axis_trans = axis_trans
# )
# get_axis_scale(d)
# Example 6: General transformation excluding current dep and NO transformation for current dep
# axis_trans <- "y*100 log_q_rgdp_false"
# d <- data.frame(
#   dep = dep,
#   axis_trans = axis_trans
# )
# get_axis_scale(d)
#' 
#' @export
get_axis_scale <- function (d) {
  
  if (d$outcome_var == "rate") {
    dep <- d$inttype
  } else {
    dep <- d$outcome_var
  }
  axis_scale <- d$axis_trans
  
  if (axis_scale == "FALSE") { # Tests if axis_trans implies no scaling
    
    message("No transformation: ", d$key, " model_", d$model_id, " Shock variable: ", d$inttype, " Outcome: ", d$outcome_var, " axis_trans: ", axis_scale)
    l <- parse_axis_scale_string(axis_scale) # Endpoint 
    
  } else if (grepl("_", axis_scale) == FALSE) { # Tests if axis_trans implies general scaling for all "dep" entries
    
    message("General axis transformation: ", d$key, " model_", d$model_id, " Shock variable: ", d$inttype, " Outcome: ", d$outcome_var, " axis_trans: ", axis_scale)
    # Parse the sub-entry and store scaling function and parameter in list l
    l <- parse_axis_scale_string(axis_scale) # Endpoint 
    
  } else if (grepl("_", axis_scale) == TRUE) { # Test if any specific "dep" entries mentioned in axis_trans
    
    if (grepl(dep, axis_scale) == FALSE) { # Test if name of current dep NOT specifically mentioned
      
      axis_scale <- unlist(strsplit(axis_scale, " ")) # Split axis_trans in sub-entries 
      
      if (grepl("_", axis_scale[1]) == TRUE) { # Test if any other specific "dep" entry is mentioned in first sub-entry of axis_trans, which would imply no general transformation for the other "dep" entries, including the current dep
        
        message("No transformation: ", d$key, " model_", d$model_id, " Shock variable: ", d$inttype, " Outcome: ", d$outcome_var, " axis_trans: ", axis_scale)
        l <- parse_axis_scale_string("FALSE") # Endpoint 
        
      } else if (grepl("_", axis_scale[1]) == FALSE) { # Test if the first sub-entry of axis_trans does NOT mention any other specific "dep" entry, which implies general transformation for non-mentioned "dep" entries, including the current dep.
        
        message("General transformation with exceptions: ", d$key, " model_", d$model_id, " Shock variable: ", d$inttype, " Outcome: ", d$outcome_var, " axis_trans: ", axis_scale)
        # Parse the first sub-entry and store scaling function and parameter in list l
        axis_scale <- axis_scale[1]
        l <- parse_axis_scale_string(axis_scale) # Endpoint 
        
      }
      
    } else if (grepl(dep, axis_scale) == TRUE) { # Test if name of current dep IS specifically mentioned
      
      (axis_scale <- unlist(strsplit(axis_scale, " "))) # Split axis_trans in sub-entries 
      
      item <- which(grepl(dep, axis_scale)) # Which sub-entry mentions current dep
      
      axis_scale <- axis_scale[item] # Get the dep sub-entry
      
      if (grepl("_false", axis_scale) == TRUE) { # Test if dep sub-entry includes "_false", which would imply no transformation for current dep
        
        message("Exception from general transformation: ", d$key, " model_", d$model_id, " Shock variable: ", d$inttype, " Outcome: ", d$outcome_var, " axis_trans: ", axis_scale)
        l <- parse_axis_scale_string("FALSE") # Endpoint 
        
      } else { # Otherwise, parse the sub-entry and store scaling function and parameter in list l
        
        message("Specific transformation for this variable: ", d$key, " model_", d$model_id, " Shock variable: ", d$inttype, " Outcome: ", d$outcome_var, " axis_trans: ", axis_scale)
        l <- parse_axis_scale_string(axis_scale) # Endpoint
        
      }
      
    }
    
  } else { # Otherwise, stop with error
    
    stop("Axis transformation unclear or specific case not included in MetaExtractR::get_axis_scale function. Check JSON file for typos and/or the function for unspecified cases: ", d$key, " model_", d$model_id, " Shock variable: ", d$inttype, " Outcome: ", d$outcome_var)
    
  }
  
  # Return scaling function and parameter as list l
  return(l)
  
}

# WARNING: For this to work for every dep, it is extremely important that the correct dep or inttype codes have been used in the entries in axis_trans variable. A typo here will mean that the axis_trans will be wrong. 
# Potential for improvement: 
# How to test this? Probably, we need a test that all of elements of the axis_trans string are an "allowed" code, so we need to define the allowed codes first, based on the existing dep and inttype entries. 
# Here are the unique entries in dep
# all.dep <- unique(all.json.test$dep)
# all.dep <- as.character(all.dep)
# all.dep <- unique(unlist(strsplit(all.dep, " ")))
# Here are the unique entries in inttype 
# all.inttype <- unique(all.json.test$inttype)
# One example: short_term vs. short_term_rate
