# dev of standardization function and helper functions that is applied to a dataframe created with join_irf_json.R

# Search for TO DO: in all related files. 


trans_study_data <- function (study_data) {
  
  trans.data <- data.frame(NULL)
  
  n <- nrow(study_data)
  
  for (i in 1:n) {
    
    # Extract a single observation
    d <- study_data[i,]
    
    # Calculate standardized effect sizes and standard errors
    d <- effect_trans_se_function(d)

    # Adjust the period to match quarterly and annual with monthly data.
    d$period.month <- adjust_period(d)

    # TO DO:
    # If necessary, adjust the outcome variables (including rate) for annualization
    # ... see the comment in adjust_shock_periodicity  (can this be done in the end?)

    trans.data <- rbind(trans.data, d)

  }
  
  # Return transformed data
  return(trans.data)
  
}
# 
# # dev test
# # # # Load helper functions (only necessary in dev)
# source("R/adjust_period.R")
# source("R/adjust_shock_periodicity.R")
# source("R/create_json_file.R")
# source("R/get_axis_scale.R")
# source("R/get_conf_level.R")
# source("R/get_crit_val.R")
# source("R/effect_trans_se_function.R")
# source("R/get_shock_size.R")
# source("R/irf_csv_path.R")
# source("R/join_irf_json.R")
# source("R/parse_axis_scale_string.R")
# source("R/parse_study_json.R")
# 
# study_data <- MetaExtractR::join_irf_json(
#   key = "with_IRF",
#   jsonpath = "data/JSON_files",
#   codebook_file = "codebook.csv",
#   irf_folder_path = "data/effect_sizes/IRFs/"
# )
# 
# test <- trans_study_data(study_data)










