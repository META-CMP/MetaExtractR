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
get_shock_size <- function(d, study_data = study_data) {
  
  # Get shock entry for observation
  shock_size <- d$size
  
  if (grepl("bp", shock_size)) { 
    
    # If basis point shock is directly available.
    shock_size <- as.double(sub("bp", "", shock_size))
    
  } else if (grepl("%", shock_size)) {
    
    # If % shock is available, transform to basis points
    shock_size <- 100 * as.double(sub("%", "", shock_size))
    
  } else if (grepl("SD", shock_size) | is.na(shock_size)) { 
    
    # If we need to approximate the shock by the initial value of the IRF 
    # of the corresponding shock variable ("rate")
    
    # Get raw shock size from rate IRF
    shock_size <- study_data[
      study_data$key == d$key & 
        study_data$model_id == d$model_id & 
        study_data$outcome_var == "rate" & 
        study_data$period == 1, "mean.effect.raw"]
    
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
    
  } else {
    stop('Shock size unclear. Check "size" in JSON')
  }
  
  return(shock_size)
  
}

# TO DO 
all.size <- as.data.frame(unique(all.json.test$size))
# Problematic size entries create issue - create issue for this 
# 15                         25
# 63                         100bps
# 45                         gr_q_rgdp

# TO DO
# all.inttype <- as.data.frame(unique(all.json.test$inttype))
# Problematic inttype entries (no lev_ or no periodicity) - create issue for this 
# 8                          lev_fed_funds
# 11                        lev_short_rate
# 13        12_month_government_bond_yield
# 19                               lev_ssr
# 27                             fed_funds
# 29                        lev_1_year_gov
# 51                          1_year_eonia
# 52                         2_year_gov_de
# 53                            1_year_gov
# 57                       log_a_fed_funds
# 64                           3_month_gov
# 71                       short_term_rate
# 81                             call_rate
# 82                    overnight_callrate
# 83                     3_month_interbank
# 84                      commercial_paper
# 85                           14_day_repo
# 93                    3_month_eurodollar
# 97                   lev_short_term_rate
# 106                   lev_3_month_gov_us

# TO DO
# It is important that inttype and, if relevant, axis_trans in the JSON use the exact same inttype codes without typos. Otherwise, the axis transformation for the rate will fail or wrongly applied. See also the comment in get_axis_scale()
