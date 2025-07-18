#' Get Confidence Level from Observation
#'
#' Extract and convert the confidence level from an observation obtained using `join_irf_json()`.
#'
#' @param d An observation (a row) from a dataframe obtained using `join_irf_json()`.
#'
#' @return numerical value of the confidence level. For instance, a value of "1_SE" would return 68, and "2_SE" would return 95. 
#'  Otherwise, the function will attempt to convert the "conf" entry of d to a numeric value.
#' 
#' @export
#'
#' @examples
#' # Assuming d is a row of a dataframe from join_irf_json():
#' # conf_level <- get_conf_level(d)
get_conf_level <- function (d) {
  
  conf_level <- d$conf
  
  if (grepl("1_SE", conf_level)) {
    
    # If conf_level matches "1_SE", set it to 68  
    conf_level <- 68
    
  } else if (grepl("2_SE", conf_level)) {
    
    # If conf_level matches "2_SE", set it to 95
    conf_level <- 95
    
  } else if (grepl("3_SE", conf_level)) {
    
    # If conf_level matches "3_SE", set it to 99.7
    conf_level <- 99.7
    
  } else {
    
    # If conf_level does not match the above patterns, convert it to a numeric value
    conf_level <- as.double(conf_level)
    
  }
  
  # Decimal scaling
  conf_level <- conf_level / 100
  
  return(conf_level)
  
}

# TO DO create issue
# all.conf <- as.data.frame(unique(all.json.test$conf))

# Problem entries: 
# 6                     68 & 90
# 10                        1SD
# 11                    68 & 95
# 12                       3_SE
# 13                1_SE & 2_SE
# 15                    80 & 90
# 17                   1SE & 90
# 18                     0.2_SE
# 23                68, 90 & 95
# 24                  90 & 1_SE
