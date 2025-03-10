#' Proportion of alters providing a specific type of support
#'
#' Calculates the proportion of alters in a personal network who provide a specified type of support.
#'
#' @param persnet_row A single row of a personal network data frame.
#' @param support_type One of 'emotional', 'advice', 'finance', 'health', or 'camaraderie'.
#'
#' @return The proportion of alters providing the specified type of support.
#'         Returns NA if the network is an isolate.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' calc_prop_alters_support(persnet_row, "emotional")
#' calc_prop_alters_support(persnet_row, "advice")
#' calc_prop_alters_support(persnet_row, "finance")
#' calc_prop_alters_support(persnet_row, "health")
#' calc_prop_alters_support(persnet_row, "camaraderie")
calc_prop_alters_support <- function(persnet_row, support_type) {
  ##########
  # Function: Computes the proportion of alters providing a specified type  
  #           of support in a personal network row.
  # Inputs:  
  #   persnet_row  = A single row of a personal network data frame
  #   support_type = One of 'emotional', 'advice', 'finance', 'health', 'camaraderie'
  # Outputs:  
  #   Proportion of alters providing the specified type of support
  ##########
  
  # Support types mapped to numeric codes
  support_map <- c(
    emotional = 1,
    advice = 2,
    finance = 3,
    health = 4,
    camaraderie = 5
  )
  
  # Validate the support_type input
  if (!(support_type %in% names(support_map))) {
    stop("Error: Choose one of 'emotional', 'advice', 'finance', 'health', or 'camaraderie'.")
  }
  
  # Convert row to a tidygraph object and check if the network is an isolate
  tg_graph <- organize_row_to_tidygraph(persnet_row)
  if (igraph::vcount(tg_graph) == 1) {
    return(NA)
  }
  
  # Identify support-related columns
  support_columns <- grep("^name\\d+supptype___\\d+$", colnames(persnet_row), value = TRUE)
  
  # Handle missing support columns
  if (length(support_columns) == 0) {
    return("Error: Support columns not present in this version of persnet.")
  }
  
  # Filter for the relevant support type
  relevant_columns <- grep(paste0("___", support_map[[support_type]]), support_columns, value = TRUE)
  if (length(relevant_columns) == 0) {
    return(0)  # if no relevant columns exist, proportion is 0
  }
  
  # Select the relevant support type columns
  support_selected <- persnet_row %>% dplyr::select(dplyr::all_of(relevant_columns))
  
  # Calculate and return the proportion of alters providing the selected type of support
  return(
    length(which(support_selected == 1)) /
    sum(persnet_row %>% dplyr::select(tie1:tie15) != 0, na.rm = TRUE)
  )
}