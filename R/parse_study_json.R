#' Parse a JSON File Containing Study Data into a Dataframe
#'
#' This function is designed to import a JSON file that contains study data,
#' cross-check this data with a specified codebook, and return the parsed data
#' as a dataframe. The function will stop and throw an error message if it 
#' encounters inconsistencies between the JSON data and the codebook, or 
#' inconsistencies in the number of models per variable in the JSON data.
#'
#' @param json_file The name of the JSON file to be parsed.
#' @param json_path The path to the JSON file to be parsed.
#' @param codebook The path to the codebook file, with default value 'codebook.xlsx'.
#' @param view_data Logical indicating whether to view the resulting dataframe, with default TRUE.
#'
#' @return The function returns a dataframe with study data. Variables in the dataframe 
#'    correspond to variables in the JSON file and align with the codebook. Each row 
#'    in the dataframe corresponds to a model from the JSON file. Values from the JSON 
#'    file that are "null" are converted to NA in the dataframe.
#'
#' @importFrom here here
#' @importFrom jsonlite fromJSON
#' @importFrom openxlsx read.xlsx
#' @importFrom janitor clean_names
#' @importFrom utils View
#' 
#' @examples 
#' # parse_study_json("study_data.json", "/path/to/study_data", "/path/to/codebook_file.xlsx", T)
#'
#' @export
parse_study_json <- 
  function (
    json_file,
    json_path,
    codebook = "codebook.xlsx",
    view_data = T
    ) {
    
    # Import the JSON file into R
    message("Attempting to parse the following file: ", (file <- here::here(json_path, json_file)))
    data <- jsonlite::fromJSON(file)
    
    # Import and test the codebook
    codebook <- openxlsx::read.xlsx(here::here(codebook))
    # Lower case variable names
    codebook <- janitor::clean_names(codebook)
    for (i in c("category", "variable")) {
      codebook[[i]] <- tolower(codebook[[i]])
    }
    # TO DO ADD: Step to exclude those variables/categories that we do not have in JSON... 
    # e.g. external controls, but maybe also rather keep them, if a study contains information on them, we would like to extract that. 
    # We could then run test on these variables, whether they already include values or are "NA" or "null". In this case, we would be prompted to get the data from an external source.
    
    # General tests/checks of the JSON study data
    # Check: Correct variable categories in JSON data?
    variable_categories <- unique(codebook$category)
    if (all(names(data) == variable_categories) != TRUE) {
      stop("Corrupted variable category names in JSON data. Check for typos. Variable categories should align with the codebook.")
    }
    # Check: Correct variable names in JSON data?
    variable_names <- list()
    for (category in variable_categories) {
      variable_names[[category]] <- codebook[codebook$category == category,"variable"]
    }
    for (category in variable_categories) {
      if (all(names(data[[category]]) == variable_names[[category]]) != TRUE) {
        error_vars <- names(data[[category]]) != variable_names[[category]]
        error_vars <- names( data[[category]][error_vars] )
        message("Check these variable names for typos:")
        print(error_vars)
        stop("Corrupted variable names in JSON data.")
      }
    }
    
    # TO DO ADD: Transformations (such as "null" or "NA" or is.null(...) to NA etc)
  
    # Determine the number of models in the study data.
    number_of_models <- function (data) {
      var_lengths <- unlist(lapply(data, lengths))
      max(var_lengths)
    }
    num_models <- number_of_models(data)
    
    # Initialize data frame with one empty row for each model
    study_df <- data.frame(
      key = rep(NA,num_models),
      model_id = NA
      )
    for (i in codebook$variable) {
      study_df[[i]] <- NA
    }
    # Fill data frame with study data
    for (model_id in 1:num_models) { # For each model
      for (section in 1:length(data)) { # For each variable category 
        section_data <- data[[section]]   # Create sub-list of the JSON list
        for (variable in names(section_data)) { # For each variable
          variable_data <- section_data[[variable]] # Select the variable
          if (all(names(variable_data) == paste0("model_", 1:num_models)) == FALSE) { # Test if numbers of models do not correspond for each variable.
            rm(study_df)
            stop("Inconsistency in number of models or model_id in JSON data.")
          }
          if (length(variable_data) > 1) {
            study_df[[variable]][model_id] <- variable_data[[model_id]] # Select the model specific value
          } else {
            study_df[[variable]][model_id] <- variable_data # Select the globally true value
          }
        }
      }
      study_df[["model_id"]][model_id] <- model_id
    }
    # Replace "null" values with NA
    study_df[study_df == "null"] <- NA
    # Optionally view the data frame
    if (view_data == T) {
      View(study_df)
    }
    study_df
  }

# parse_study_json(
#   json_file = "t4i3d3y_filled.json",
#   json_path = "data/JSON_files",
#   codebook = "codebook.xlsx",
#   view_data = T
# )

##### Dev

# library(jsonlite)
# library(here)

# Importing the JSON
# Here, we need to add how to deal with NA values or maybe null values
# data <- fromJSON(here("data/JSON files/global_and_local_dev/254gg.json"))
# data <- fromJSON(here("data/JSON files/global_and_local_dev/362ff.json"))
# data <- fromJSON(here("data/JSON files/global_and_local_dev/362ff_error.json"))
# data <- fromJSON(here("data/JSON files/global_and_local_dev/4f43f.json"))
# data <- jsonlite::fromJSON(here::here("data/JSON files/global_and_local_dev/t4i3d3y.json"))

## General tests/checks on the list.
# ... such as category and variable names and formatts (maybe also reformat here, if necessary)
# Tests
# str(data)
# 
# variable_categories <- c("general", "irf_place", "identification_strategy", "estimation_method")
# if (all(names(data) == variable_categories) != TRUE) {
#   stop("Corrupted variable category names in JSON data. Check for typos. Variable categories should be ... ")
# }
# 
# variable_names <- list(
#   general = c("key", "rid1", "rid2"),
#   irf_place = c("figure_id", "page", "figure_notes"),
#   identification_strategy = c("iv", "nr", "event", "notes", "idother", "chol", "svar", "nonrec", "signr", "signroth", "hf", "heteroskedas", "longrun", "taylor"),
#   estimation_method=c("var", "lp", "vecm", "dyn_ols", "fvar", "tvar", "gvar", "bayes", "varother")
# )
# for (category in variable_categories) {
#   if (all(names(data[[category]]) == variable_names[[category]]) != TRUE) {
#     stop("Corrupted variable names in JSON data. Check for typos.")
#   }
# }

# Transformations
# 
# ## Determine the number of models in the list.
# number_of_models <- function (data) {
#   var_lengths <- unlist(lapply(data, lengths))
#   max(var_lengths)
# }
# num_models <- number_of_models(data)
# 
# # Initialize data frame with one empty row for each model
# study_df <- data.frame(
#   key = rep(NA,num_models),
#   model_id = NA
# )
# for (i in codebook$variable) {
#   study_df[[i]] <- NA
# }
# # Fill data frame with study data
# for (model_id in 1:num_models) { # For each model
#   for (section in 1:length(data)) { # For each variable category 
#     section_data <- data[[section]]   # Create sub-list of the JSON list
#     for (variable in names(section_data)) { # For each variable
#       variable_data <- section_data[[variable]] # Select the variable
#       if (all(names(variable_data) == paste0("model_", 1:num_models)) == FALSE) { # Test if numbers of models do not correspond for each variable.
#         rm(study_df)
#         stop("Inconsistency in number of models or model_id in JSON data.")
#       }
#       if (length(variable_data) > 1) {
#         study_df[[variable]][model_id] <- variable_data[[model_id]] # Select the model specific value
#       } else {
#         study_df[[variable]][model_id] <- variable_data # Select the globally true value
#       }
#     }
#   }
#   study_df[["model_id"]][model_id] <- model_id
# }
# study_df[study_df == "null"] <- NA # Does this work to replace all "NA" with NA (disregarding the length and structure of study_df? Test this!)
# View(study_df)

