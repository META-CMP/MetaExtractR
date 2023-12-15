#' Assign doublechecks to a Person
#'
#' This function assigns a specified number of studies 
#' to a person for doublechecks. It updates the list of assigned doublechecks by adding the new 
#' assignments and saves the updated list in a CSV file.
#'
#' @param name The researcher ID of the person to whom the doublechecks will be assigned. 
#' If not specified, the function will stop with an error message. Choose one of "fp", "ph", "me", "dr", "sg".
#' @param n_doublechecks The number of doublechecks to assign to the person. Default is 10.
#' @param weight The weight the random draw should give to studies for which primary coders did not ask for a doublecheck. Thus, the lower this weight the faster the random draw assigns studies which should be doublechecked according to the primary researcher.
#' 
#' @details The function first loads a list of already assigned doublechecks from a CSV file. 
#' It then retrieves the list of all included studies and their corresponding JSON files. 
#' The function checks if each included study has a corresponding JSON file and warns if any are missing.
#' Next, it identifies unassigned studies and assigns the specified number of these studies 
#' to the provided person's ID. The assignments are then added to the existing list of assignments, 
#' and the updated list is saved back to the CSV file.
#' 
#' @return The function does not return a value but updates a CSV file with new assignments 
#' and prints a message upon completion.
#'
#' @examples
#' # Assign 5 IRF folders to a person with the ID "fp"
#' assign_irfs(name = "fp", n_doublechecks = 10, weight=.001)
#'
#' @export
assign_doublechecks <- function (name = NULL, n_doublechecks = 10, weight=.001) {
  
  # Load list of assigned IRFs:
  library(readr)
  library(dplyr)
  library(tidyr)
  assigned_doublechecks <- read_csv("data/doublechecks/assigned_doublechecks.csv")
  assigned_codes <- assigned_doublechecks$key
  
  # Get list of all included studies and JSON files
  df_all_studies<-readRDS("data/doublechecks/df_full_text")
  all_studies <- df_all_studies$key
  all_JSONs <- unlist(strsplit(list.files("~/data/data/full_text_screening/JSON_files", full.names = FALSE, recursive = TRUE),".json"))
  
  
  # Check keys of IRF folders are also in JSON folder
  if (sum(all_studies %in% all_JSONs) != length(all_studies)) {
    warning("Some included studies have no corresponding JSON files.")
  }
  
  # Create weight for studies without doublecheck_request
  df_all_studies$weight<-ifelse(df_all_studies$doublecheck==TRUE,1,weight)
  
  # draw random sample of unassigned doublechecks
  random_sample<-df_all_studies %>% filter(key %in% all_JSONs & !key %in% assigned_codes & rid1!=name & is.na(rid2)) %>% # filter to only consider relevant entries
    slice_sample(n=n_doublechecks, replace = FALSE, weight_by = weight) # drawn n observations
  
  # check if ID is provided
  if (is.null(name) == TRUE) {
    stop("Name not specified. Please specify a person ID to assign to.")
  }
  
  # select relevant columns of the big file
  random_sample<-random_sample %>% select(key, `publication title`,BibtexKey, rid1,rid2,doublecheck,reason_for_doublecheck)
  
  # Assign the draw to the research ID
  random_sample$rid2<-name
  # set check completed to NA
  random_sample$doublecheck_completed<-NA
  
  # rename columns
  colnames(random_sample)[6:7]<-c("doublecheck_request","reason_for_doublecheck_request")
  
  
  
  # create updated list of unassigned doublechecks
  new <- rbind(assigned_doublechecks, random_sample)
  
  
  
  # check if all doublechecks are uniquely assigned
  if (length(unique(new$key))!=nrow(new)) {
    stop("Error: duplicated assignment, check code.")
  }
  
  
  # update csv file with the newly assigned doublechecks. 
  write_csv(new, "data/doublechecks/assigned_doublechecks.csv")
  
  
  message("Done. Don't forget to push a commit and merge into main branch!")
  
}