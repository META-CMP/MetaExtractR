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
#' @param codebook The path to the codebook csv file, with default value 'codebook.csv'.
#' @param view_data Logical indicating whether to view the resulting dataframe, with default TRUE.
#'
#' @return The function returns a dataframe with study data. Variables in the dataframe 
#'    correspond to variables in the JSON file and align with the codebook. Each row 
#'    in the dataframe corresponds to a model from the JSON file. Values from the JSON 
#'    file that are "null" are converted to NA in the dataframe.
#'
#' @importFrom here here
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.csv
#' @importFrom janitor clean_names
#' @importFrom utils View
#' 
#' @examples 
#' parse_study_json(
#'  json_file = "dev_filled.json", 
#'  json_path = "/Users/franzprante/GitHub/MORPEP/META_CMP/toy_data_extraction_dev/data/JSON_files", 
#'  codebook = "/Users/franzprante/GitHub/MORPEP/META_CMP/data/codebook.csv", 
#'  view_data = FALSE)
#'
#' @export
parse_study_json <- 
  function (
    json_file,
    json_path,
    codebook = "codebook.csv",
    view_data = TRUE
  ) {
    
    # Import the JSON file into R
    message("Attempting to parse the following file: ", (file <- here::here(json_path, json_file)))
    data <- jsonlite::fromJSON(file)
    
    # Import and test the codebook
    codebook <- read.csv(here::here(codebook))
    # Exclude variables that are not relevant for the JSON file
    codebook <- codebook[codebook$json == TRUE,]
    # Lower case variable names
    codebook <- janitor::clean_names(codebook)
    for (i in c("category", "variable")) {
      codebook[[i]] <- tolower(codebook[[i]])
    }
    
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
    
    # TO DO ADD: Transformations
    
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
    if (view_data == TRUE) {
      View(study_df)
    }
    study_df
  }

# parse_study_json(
#   json_file = "dev.json",
#   json_path = json_path,
#   codebook = codebook,
#   view_data = TRUE
# )