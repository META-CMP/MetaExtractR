# dev of standardization function and helper functions that is applied to a dataframe created with join_irf_json.R

# Search for TO DO: in all related files. 


trans_study_data <- function (study_data) {
  
  trans.data <- data.frame(NULL)
  
  # Dev here for one observation of study_data
  # begin loop
  n <- nrow(study_data)
  for (i in 1:n) {
    
    # Extract a single observation
    d <- study_data[i,]
    
    # Get y-axis transformation function and parameter
    # Create function:
    axis_scaling <- get_axis_scale(d)$axis_scaling_function
    # Store scaling parameter for inspection:
    # (this is only for inspection - the axis_scaling function can access the parameter from it's own environment)
    y_scale <- get_axis_scale(d)$scaling_parameter 
    
    # Specify shock size
    shock_size <- get_shock_size(d, study_data)
    
    # Adjust periodicity of shock_size (if necessary)
    shock_size <- adjust_shock_periodicity(d, shock_size = shock_size)
    
    # Get confidence level and critical value
    conf_level <- get_conf_level(d)
    crit_val <- get_crit_val(conf_level)

    # Determine Transformation Case and create standardization function and SE function
    trans_function <- get_effect_trans_function(d)$trans_function # TO DO: Placeholders in the function
    se_up_function <- get_effect_trans_function(d)$se_up_function # TO DO: Placeholders in the function
    se_low_function <- get_effect_trans_function(d)$se_low_function # TO DO: Placeholders in the function

    # Standardize to effect of 1 percentage point shock
    d$CI.upper <- trans_function(d$CI.upper.raw, shock_size)
    d$mean.effect <- trans_function(d$mean.effect.raw, shock_size)
    d$CI.lower <- trans_function(d$CI.lower.raw, shock_size)

    # Apply axis scaling function - standardizes results to show percentage changes
    d$CI.upper <- axis_scaling(d$CI.upper)
    d$mean.effect <- axis_scaling(d$mean.effect)
    d$CI.lower <- axis_scaling(d$CI.lower)

    # Calculate the standard errors
    d$SE.upper <- se_up_function(mean = d$mean.effect, up = d$CI.upper, crit_val = crit_val)
    d$SE.lower <- se_low_function(mean = d$mean.effect, low = d$CI.lower, crit_val = crit_val)

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

# dev test
# Load helper functions (only necessary in dev)
source("R/adjust_shock_periodicity.R")
source("R/get_axis_scale.R")
source("R/get_shock_size.R")
source("R/get_conf_level.R")
source("R/get_crit_val.R")
source("R/get_effect_trans_function.R")
source("R/adjust_period.R")

study_data <- MetaExtractR::join_irf_json(
  key = "with_IRF",
  jsonpath = "~/GitHub/MORPEP/META_CMP/toy_data_extraction_dev/data/JSON_files",
  codebook_file = "~/GitHub/MORPEP/META_CMP/data/codebook.csv",
  irf_folder_path = "~/GitHub/MORPEP/META_CMP/toy_data_extraction_dev/data/effect_sizes/IRFs/"
)

test <- trans_study_data(study_data)



