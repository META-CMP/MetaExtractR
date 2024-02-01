#' Final Join Function
#'
#' This function builds a dataframe across all studies for final analysis. It reads JSON files and, optionally, IRF (Impulse Response Functions) files from specified directories, processes them, and combines them into a single dataframe.
#'
#' @param json_path A string specifying the path to the folder containing JSON files. Defaults to "data/JSON_files".
#' @param irf_path A string specifying the path to the folder containing IRF files. This parameter is used only if `only_json` is FALSE. Defaults to "data/effect_sizes/IRFs/".
#' @param only_json A logical value. If TRUE, the function will process only the JSON files. If FALSE, it will also process the IRF files. Defaults to FALSE.
#' @param ignore An optional vector of strings. Each string should be the key of a study that should be ignored during processing. If NULL, no studies are ignored. Defaults to NULL.
#' @param starting_check_at An optional string of a specific study key. This study is the starting point of studies that should be looked at during processing. If NULL, no specific study is selected. Defaults to NULL. It is only possible to use study keys which are not part of the "ignore" vector. Otherwise the package will throw an error message.
#' @param investigate An optional vector of strings. Each string should be the key of a study that should be looked at during processing. If NULL, no specific studies are selected. Defaults to NULL. It is only possible to use study keys which are not part of the "ignore" vector and are ranked below the potential "starting_check_at" study. Otherwise the package will throw an error message.
#'
#' @return A dataframe that consolidates information on study moderator variables, effect sizes and standard errors from all the processed studies.
#'
#' @export

final_join <- function (json_path = "data/JSON_files", irf_path = "data/effect_sizes/IRFs/", only_json = FALSE, ignore = NULL, starting_check_at=NULL, investigate = NULL) {
  
  # Get the keys of the files
  # List all JSON files in the folder
  file_names <- list.files(json_path, pattern = "\\.json$")
  # Remove the .json extension
  keys <- sub("\\.json$", "", file_names)
  # Option to ignore specific studies
  if (is.null(ignore) == FALSE) {
    keys <- keys[!(keys %in% ignore)]
  }
  # Option to only look at studies starting from study x onward
  if (is.null(starting_check_at) == FALSE) {
    # create index for the starting element
    starting_index <- which(keys == starting_check_at)
    # Exclude elements until the starting element
    keys <- keys[(starting_index):length(keys)]
  }
  # Option to investigate specific studies
  if (is.null(investigate) == FALSE) {
    keys <- keys[keys %in% investigate]
  }
  # Number of studies
  n_studies <- length(keys)
  # Empty list to store the final study-level data
  final.data.list <- vector("list", n_studies)
  
  # for each key
  for (i in 1:n_studies) {
    
    if (only_json == FALSE) {
      
      # Parse JSON and IRF data
      study_data <<- join_irf_json(
        key = keys[i],
        jsonpath = json_path,
        codebook_file = "codebook.csv",
        irf_folder_path = irf_path
      )
      
      # Standardize data
      study_data <<- trans_study_data(study_data)
      
    } else if (only_json == TRUE) { # Option to only join all the JSON data without IRFs
      # Parse JSON
      study_data <<- parse_study_json(json_file = paste0(keys[i], ".json"), 
                                      json_path = json_path,
                                      codebook = "codebook.csv",
                                      view_data = FALSE,
                                      ignore_failed_tests = FALSE)
    }
    
    # Add to final.data.list
    final.data.list[[i]] <- study_data
  }
  
  # Remove study_data
  rm(study_data)
  # Combine all study-level dataframes in final.data.list into the final dataframe
  final.data <- do.call(rbind, final.data.list)
  return(final.data)
  
}
