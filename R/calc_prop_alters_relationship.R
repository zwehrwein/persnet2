#' Proportion of alters by relationship type
#'
#' Calculates the proportion of alters in a personal network who have a specified relationship type with the ego.
#'
#' @param persnet_row A single row of a personal network data frame.
#' @param relationship_type One of 'spouse', 'family', 'friend', 'advice', 'coworker', or 'other'.
#'
#' @return The proportion of alters with the specified relationship type.
#'         Returns NA if the network is an isolate.
#' @export
#'
#' @examples
#' calc_prop_alters_relationship(persnet_row, "spouse")
#' calc_prop_alters_relationship(persnet_row, "family")
#' calc_prop_alters_relationship(persnet_row, "friend")
#' calc_prop_alters_relationship(persnet_row, "advice")
#' calc_prop_alters_relationship(persnet_row, "coworker")
#' calc_prop_alters_relationship(persnet_row, "other")
calc_prop_alters_relationship <- function(persnet_row, relationship_type) {
  ##########
  # Function: Computes the proportion of alters with a specified relationship  
  #           type in a personal network row.
  # Inputs:  
  #   persnet_row       = A single row of a personal network data frame
  #   relationship_type = One of 'spouse', 'family', 'friend', 'advice',  
  #                      'coworker', or 'other'
  # Outputs:  
  #   Proportion of alters with the specified relationship type
  ##########
  
  # Relationship types mapped to numeric codes
  relationship_map <- c(
    spouse = 1,
    family = 2,
    friend = 3,
    advice = 4,
    coworker = 5,
    other = 77
  )
  
  # Validate the relationship_type input
  if (!(relationship_type %in% names(relationship_map))) {
    stop("Error: Choose one of 'spouse', 'family', 'friend', 'advice', 'coworker', or 'other'.")
  }
  
  # Convert row to a tidygraph object and check if the network is an isolate
  tg_graph <- organize_row_to_tidygraph(persnet_row)
  if (vcount(tg_graph) == 1) {
    return(NA)
  }
  
  # Identify relationship-related columns
  relationship_columns <- grep("^name\\d+relat___\\d+$", colnames(persnet_row), value = TRUE)
  
  # Handle missing relationship columns
  if (length(relationship_columns) == 0) {
    return("Error: Relationship columns not present in this version of persnet.")
  }
  
  # Filter for the relevant relationship type
  relevant_columns <- grep(paste0("___", relationship_map[[relationship_type]]), relationship_columns, value = TRUE)
  if (length(relevant_columns) == 0) {
    return(0)  # If no relevant columns exist, proportion is 0
  }
  
  # Select the relevant relationship type columns
  relationship_selected <- persnet_row %>% select(all_of(relevant_columns))
  
  # Calculate and return the proportion of alters with the selected relationship type
  return(
    length(which(relationship_selected == 1)) /
    sum(persnet_row %>% select(tie1:tie15) != 0, na.rm = TRUE)
  )
}