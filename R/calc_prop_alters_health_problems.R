#' Calculate the proportion of alters with a specific health problem
#'
#' Computes the proportion of alters with a specified health problem in a personal network row.
#'
#' @param persnet_row A single row of a personal network data frame.
#' @param health_problem One of 'general', 'pain', 'mental', 'cardiac', 'healthy', or 'dont_know'.
#'
#' @return The proportion of alters with the specified health problem.
#'         Returns NA if the network is an isolate.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' calc_prop_alters_health_problems(persnet_row, "general")
#' calc_prop_alters_health_problems(persnet_row, "pain")
#' calc_prop_alters_health_problems(persnet_row, "mental")
#' calc_prop_alters_health_problems(persnet_row, "cardiac")
#' calc_prop_alters_health_problems(persnet_row, "healthy")
#' calc_prop_alters_health_problems(persnet_row, "dont_know")
calc_prop_alters_health_problems <- function(persnet_row, health_problem) {
  ##########
  # Function: Computes the proportion of alters with a specified health  
  #           problem in a personal network row.
  # Inputs:  
  #   persnet_row   = A single row of a personal network data frame
  #   health_problem = One of 'general', 'pain', 'mental', 'cardiac',  
  #                   'healthy', or 'dont_know'
  # Outputs:  
  #   Proportion of alters with the specified health problem
  ##########
  
  # Health problem types mapped to numeric codes
  health_prob_map <- c(
    general = 1,
    pain = 2,
    mental = 3,
    cardiac = 4,
    healthy = 0,
    dont_know = 99
  )
  
  # Validate the health_problem input
  if (!(health_problem %in% names(health_prob_map))) {
    stop("Error: Choose one of 'general' (general health problems), 'pain', 
         'mental', 'cardiac', 'healthy', or 'dont_know'.")
  }
  
  # Convert row to a tidygraph object and check if the network is an isolate
  tg_graph <- organize_row_to_tidygraph(persnet_row)
  if (igraph::vcount(tg_graph) == 1) {
    return(NA)
  }
  
  # Identify health problem-related columns
  health_columns <- grep("^name\\d+health___\\d+$", colnames(persnet_row), value = TRUE)
  
  # Handle missing health problem columns
  if (length(health_columns) == 0) {
    return("Error: Health problem columns not present in this version of persnet.")
  }
  
  # Filter for the relevant health problem type
  relevant_columns <- grep(paste0("___", health_prob_map[[health_problem]]), health_columns, value = TRUE)
  if (length(relevant_columns) == 0) {
    return(0) 
  }
  
  # Select the relevant health problem columns
  health_problem_selected <- persnet_row %>% dplyr::select(dplyr::all_of(relevant_columns))
  
  # Calculate and return the proportion of alters with the selected health problem
  return(
    length(which(health_problem_selected == 1)) /
    sum(persnet_row %>% dplyr::select(tie1:tie15) != 0, na.rm = TRUE)
  )
}