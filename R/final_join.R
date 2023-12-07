
# Dev of function that builds dataframe for final analysis across all studies:

final_join <- function (json_path = "data/JSON_files", irf_path = "data/effect_sizes/IRFs/") {
  
  # Get the keys of the files
  # List all JSON files in the folder
  file_names <- list.files(json_path, pattern = "\\.json$")
  # Remove the .json extension
  keys <- sub("\\.json$", "", file_names)
  # Number of studies
  n_studies <- length(keys)
  # Empty list to store the final study-level data
  final.data.list <- vector("list", n_studies)
  
  # for each key
  for (i in 1:n_studies) {
    
    # Parse JSON and IRF data
    key_study_data <- join_irf_json(
      key = keys[i],
      jsonpath = json_path,
      codebook_file = "codebook.csv",
      irf_folder_path = irf_path
    )
    
    # Standardize data
    key_study_data <- trans_study_data(key_study_data)
    
    # Add to final.data.list
    final.data.list[[i]] <- key_study_data
  }
  
  # Combine all study-level dataframes in final.data.list into the final dataframe
  final.data <- do.call(rbind, final.data.list)
  return(final.data)
  
}

final.test <- final_join(json_path = "data/JSON_files/Titus_test", irf_path = "data/effect_sizes/Titus_test/")
