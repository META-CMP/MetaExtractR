#' Assign IRF Folders to a Person
#'
#' This function assigns a specified number of Impulse Response Function (IRF) folders 
#' to a person for data extraction. It updates the list of assigned IRFs by adding the new 
#' assignments and saves the updated list in a CSV file.
#'
#' @param name The name of the person to whom the IRF folders will be assigned. 
#' If not specified, the function will stop with an error message.
#' @param n The number of IRF folders to assign to the person. Default is 5.
#' 
#' @details The function first loads a list of already assigned IRFs from a CSV file. 
#' It then retrieves the list of all current IRF folders and their corresponding JSON files. 
#' The function checks if each IRF folder has a corresponding JSON file and warns if any are missing.
#' Next, it identifies unassigned IRF folders and assigns the specified number of these folders 
#' to the provided person's name. The assignments are then added to the existing list of assignments, 
#' and the updated list is saved back to the CSV file.
#' 
#' @return The function does not return a value but updates a CSV file with new assignments 
#' and prints a message upon completion.
#'
#' @examples
#' # Assign 5 IRF folders to a person named "John Doe"
#' assign_irfs(name = "John Doe", n = 5)
#'
#' @export
assign_irfs <- function (name = NULL, n = 5) {
  
  # Load list of assigned IRFs:
  library(readr)
  assigned_irfs <- read_csv("data/effect_sizes/assigned_irfs.csv")
  assigned_codes <- assigned_irfs$irf
  
  # Get list of all current IRF folders and JSON files
  all_IRFs <- list.files("data/effect_sizes/IRFs", recursive = F, full.names = F)
  all_JSONs <- list.files("data/full_text_screening/JSON_files", recursive = F, full.names = F)
  all_JSONs <- sub("\\.json$", "", all_JSONs)
  
  # Check keys of IRF folders are also in JSON folder
  if (sum(all_IRFs %in% all_JSONs) != length(all_IRFs)) {
    warning("Some IRFs have no JSON files.")
  }
  
  # Get unassigned IRFs 
  unassigned <- all_IRFs[!(all_IRFs %in% assigned_codes)]
  
  # Assign new IRFs
  if (is.null(name) == TRUE) {
    stop("Name not specified. Please specify a person to assign to.")
  }
  person <- name # Who to assign to?
  n_irfs <- 5 # How many new IRFs?
  new_assigned <- unassigned[1:n_irfs]
  d <- data.frame(
    irf = new_assigned,
    included_in_dataset = NA,
    assigned_to = person 
  )
  
  # Update list of assigned IRFs
  new <- rbind(assigned_irfs, d)
  write_csv(new, "data/effect_sizes/assigned_irfs.csv")
  
  message("Done. Don't forget to push a commit and merge into main branch!")
  
}