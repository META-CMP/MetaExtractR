#' Create JSON File
#'
#' This function generates a JSON file based on a codebook with variable 
#' names and variable categories, a study key, and a researcher ID.
#'
#' @param key Study key (character): A unique identifier for the study.
#' @param rid Researcher ID (character): A unique identifier for the researcher.
#' @param codebook File path (character, default = "codebook.xlsx"): The path to the codebook file in Excel format (.xlsx).
#' @param folder_path Folder path (character, default = "data/JSON_files"): The folder path where the JSON file will be saved.
#'
#' @details This function reads the codebook from the specified Excel file, 
#' checks for uniqueness of variable names, and converts the variable names 
#' to lowercase. It then creates a JSON file with variable categories as the 
#' main keys and variable names as subkeys, assigning NA values to each variable.
#' Additionally, it adds the study key and researcher ID to the generated JSON file, 
#' as well as placeholder options for categorical variables. Please note that the 
#' placeholder options are a temporary workaround until they can be listed in 
#' the codebook itself.
#'
#' @examples
#' create_json_file(key = "J4L4L3", rid = "fp")
#'
#' @import openxlsx read.xlsx
#' @import janitor clean_names
#' @import jsonlite prettify
#' @import jsonlite toJSON
#' @importFrom here here
#'
#' @export
create_json_file <- 
  function(key,
           rid, 
           codebook = "codebook.xlsx",
           folder_path = "data/JSON_files"
           ) {
    file_path = here::here(folder_path, paste0(key, ".json"))
    
    # Check if the file already exists
    if (file.exists(file_path)) {
      stop("JSON file already exists for the given key.")
    }
    
    # Import the codebook
    codebook <- openxlsx::read.xlsx(here::here(codebook))
    # Test uniqueness of variable names in codebook
    if (length(unique(codebook$variable)) != length(codebook$variable)) {
      stop("Variables names in codebook are not unique.")
    }
    # Lower case variable names
    codebook <- janitor::clean_names(codebook)
    for (i in c("category", "variable")) {
      codebook[[i]] <- tolower(codebook[[i]])
    }
    
    # ADD Step to filter out variables that we do not need in the JSON (e.g. external controls)
    
    df <- codebook
  
    # Initialize an empty list
    data <- list()
    
    # Iterate over unique categories
    for (category in unique(df$category)) {
      # Filter the dataframe for the current category
      category_df <- df[df$category == category, ]
      
      # Create a sublist for the current category
      sublist <- list()
      
      # Iterate over the rows of the filtered dataframe
      for (i in 1:nrow(category_df)) {
        # Get the variable for the current row
        variable <- category_df$variable[i]
  
        # Add the variable and assign NA value to the sublist
        sublist[[variable]] <- NA
      }
      
      # Add the sublist to the main list
      data[[category]] <- sublist
    }
    
    # Add key and rid1
    data$general$key <- key
    data$general$rid1 <- rid
    
    # Add placeholder options for categorical variables
    # TO DO: This is only a workaround for now. Eventually, we want these options to be listed in the codebook and categorical variables marked and then automatically create these entries.
    data$impulse_and_response_variables$dep <- "rGDP employ ..."
    
    # Create the JSON string
    json_data <- jsonlite::prettify(
      jsonlite::toJSON(data, auto_unbox = TRUE)
    )
    
    # Write the JSON file
    write(json_data, file = file_path)
    
    # Print success message
    cat("JSON file", paste0(key, ".json"), "has been created.")
    }

# create_json_file(key = "cat_test", rid = "fp", codebook = "codebook.xlsx")


# We also need a function that edits or deletes or transforms a specifc key, maybe also value, for all existing JSON files (e.g. change of variable name) (maybe simply gsub ...)

# When filled out for a study, the JSON file can be imported back into R and tested on consistency and then the study dataframe (containing all model specifications) can be created.

# The study dataframe then needs to be connected to the IRF values

# Think about how best to correct values in double checking (again nested? or arrays?)

# We should also create some convienience functions for data exploring, for example, to quickly build the dataframe for some studies, based on titles or keys or so. Also to only select few variables.
