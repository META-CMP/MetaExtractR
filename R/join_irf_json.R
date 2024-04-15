#' Join effect sizes from IRFs and JSON data for a study
#'
#' This function joins effect size data in the form of impulse response functions 
#' from CSV files with the coding from a JSON file for a given study.
#'
#' @param key A study key identifying the study for which IRF data needs to be joined.
#' @param jsonpath A character string specifying the path to the folder that contains 
#'  the JSON files.
#' @param codebook_file A character string specifiying the path to the codebook.
#' @param irf_folder_path A character string specifying the path to the folder 
#'  containing the IRF CSV files.
#' @param ignore_failed_tests A logical. If TRUE, the function will parse the 
#'  JSON despite failing consistency tests.
#' 
#' @return A data frame containing the joined and processed coding and IRF data.
#'
#' @importFrom data.table fread
#' @importFrom here here
#'
#' @export
#'
#' @examples
#' join_irf_json(
#'   key = "QSKR5KKX",
#'   jsonpath = "/Users/franzprante/GitHub/MORPEP/META_CMP/toy_data_extraction_dev/data/JSON_files",
#'   codebook_file = "/Users/franzprante/GitHub/MORPEP/META_CMP/toy_data_extraction_dev/codebook.csv",
#'   irf_folder_path = "/Users/franzprante/GitHub/MORPEP/META_CMP/toy_data_extraction_dev/data/effect_sizes/IRFs/",
#'   ignore_failed_tests = FALSE)
join_irf_json <- function(key,
                          jsonpath = "data/full_text_screening/JSON_files",
                          codebook_file = "codebook.csv",
                          irf_folder_path = "data/effect_sizes/IRFs/",
                          ignore_failed_tests = FALSE
                          ) {
  
  # Loading the coding data 
  d_coding <- parse_study_json(json_file = paste0(key, ".json"), 
                                              json_path = jsonpath,
                                              codebook = codebook_file,
                                              view_data = F,
                                              ignore_failed_tests = ignore_failed_tests)
  
  # Create empty list for model level data
  model_data <- list()
  
  # Identify the number of models in the study
  models <- max(d_coding$model_id)
  
  # Loop through the models of the study 
  for (i in 1:models) {
    
    # Select the model
    model <- i 
    
    # Select model coding data
    d_m_coding <- d_coding[d_coding$model_id == model, ]
    row.names(d_m_coding) <- NULL
    
    # Extracting the list of dependent variables for the model
    dep <- d_m_coding$dep
    # Replace accidentally coded multiple spaces with a single space
    dep <- gsub("\\s+", " ", dep)
    dep <- strsplit(dep, split = " ")[[1]] 
    
    # Check if the subfolder "rate" exists in the model folder
    model_folder <- paste0(irf_folder_path, key, "/model_", model)
    rate_irf <- dir.exists(file.path(model_folder, "rate"))
    # Add "rate" as an outcome variable if it is available
    if (rate_irf) {
      dep[length(dep) + 1] <- "rate"
    }
    
    # Create empty list for storing of dep level data
    data_list <- list()
    
    # dep variable loop
    for (j in 1:length(dep)) {
      
      message("Trying to merge IRF data of ", key, " model_", model, " ", dep[j])
      
      # Select outcome variable
      outcome_var <- dep[j]
      
      # Load mean response
      csv_path <- irf_csv_path(key, irf_folder_path, model, outcome_var, irf_part = "mean")
      mean <- data.table::fread(csv_path, 
                                sep = ";", 
                                dec = ",",
                                col.names = c("period", "mean"))
      order_mean <- order(mean$period)
      mean <- mean[order_mean, ]
      # Load lower confidence band
      csv_path <- irf_csv_path(key, irf_folder_path, model, outcome_var, irf_part = "lower")
      lower <- data.table::fread(csv_path, 
                                 sep = ";", 
                                 dec = ",",
                                 col.names = c("period", "lower"))
      order_lower <- order(lower$period)
      lower <- lower[order_lower, ]
      # Load upper confidence band
      csv_path <- irf_csv_path(key, irf_folder_path, model, outcome_var, irf_part = "upper")
      upper <- data.table::fread(csv_path, 
                                 sep = ";", 
                                 dec = ",",
                                 col.names = c("period", "upper"))
      order_upper <- order(upper$period)
      upper <- upper[order_upper, ]
      
      # Test that mean, upper, lower have same number of observations
      n_obs_mean <- nrow(mean)
      n_obs_upper <- nrow(upper)
      n_obs_lower <- nrow(lower)
      if ((n_obs_mean == n_obs_upper && n_obs_upper == n_obs_lower) == FALSE) {
        warning("Check IRF data extraction in WebPlotDigitizer. Number of observations inconsistent: mean:", n_obs_mean, " upper:", n_obs_upper, " lower:", n_obs_lower)
      }
      
      # Create IRF dataframe for the dep variable
      dep_data <- data.frame(
        period = 1:nrow(mean), # This standardizes all periods for all studies such that the first period is always period 1. TO DO: discuss if we should use 0
        CI.upper.raw = upper$upper,
        mean.effect.raw = mean$mean,
        CI.lower.raw = lower$lower,
        outcome_var = outcome_var
      )
      
      # Store in data_list
      data_list[[j]] <- cbind(d_m_coding[,-which(names(d_m_coding) == "dep")],dep_data)
      
      # Clean up
      rm(upper, mean, lower, dep_data)
      
      message("Done.")
    }
    
    # rbind the dep-level dataframes
    model_data[[i]] <- do.call(rbind, data_list)
    
    # Automatic IRF checks
    # IRF test: Check that for a specific model, the IRFs for all dependent variables have the same number of observations
      # Step 1: Split the dataframe by 'outcome_var'
      split_data <- split(model_data[[i]], model_data[[i]]$outcome_var)
      # Step 2: Get the number of rows (i.e. periods) for each 'outcome_var'
      periods_per_outcome_var <- lapply(split_data, nrow)
      # Step 3: Check if all 'outcome_var' have the same number of periods
      if (length(unique(periods_per_outcome_var)) != 1) {
        warning("IRF test failed: Not all dep IRFs have the same number of periods. Check IRF extraction. ", key, " model_", i)
      }
    # IRF test: Check if less than 8 observations for quarterly data or less than 16 for monthly data
      # Check for quarterly
      if (unique(model_data[[i]]$quarter)) {
        if (all(periods_per_outcome_var < 8)) {
          warning("IRF test failed: Quarterly data with less than 8 observations. Check IRF extraction. ", key, " model_", i)
        }
      }
      # Check for monthly
      if (unique(model_data[[i]]$month)) {
        if (all(periods_per_outcome_var < 16)) {
          warning("IRF test failed: Monthly data with less than 16 observations. Check IRF extraction. ", key, " model_", i)
        }
      }
    # IRF test: Check if upper>mean>lower for all periods of the IRF except the first period.
      irf_upper_mean_lower_test <- model_data[[i]][model_data[[i]]$period != 1,]
      if (!all(irf_upper_mean_lower_test$CI.lower.raw < irf_upper_mean_lower_test$mean.effect.raw & irf_upper_mean_lower_test$mean.effect.raw < irf_upper_mean_lower_test$CI.upper.raw)) {
        warning("IRF test failed: lower < mean < upper not always true. Check IRF extraction. ", key, " model_", i)
      }
  }
  
  # rbind the model-level dataframes
  study_data <- do.call(rbind, model_data)
  
  study_data
  
}
