#' Calculate the proportion of alters based on the length of time known
#'
#' Computes the proportion of alters who have known the ego for a specified length of time.
#'
#' @param persnet_row A single row of a personal network data frame.
#' @param time_category One of 'less_than_3years', 'three_to_6years', 'more_than_6years', or 'unknown'.
#'
#' @return The proportion of alters within the specified time category.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' calc_prop_alters_known_length(persnet_row, "less_than_3years")
#' calc_prop_alters_known_length(persnet_row, "three_to_6years")
#' calc_prop_alters_known_length(persnet_row, "more_than_6years")
#' calc_prop_alters_known_length(persnet_row, "unknown")
calc_prop_alters_known_length <- function(persnet_row, time_category) {
  ##########
  # Function: Computes the proportion of alters who have known the ego  
  #           for a specified length of time.
  # Inputs:  
  #   persnet_row   = A single row of a personal network data frame
  #   time_category = One of 'less_than_3years', 'three_to_6years',  
  #                   'more_than_6years', or 'unknown'
  # Outputs:  
  #   Proportion of alters within the specified time category
  ##########
  
  # Map time categories to numeric codes
  time_map <- c(
    less_than_3years = 1,
    three_to_6years = 2,
    more_than_6years = 3,
    unknown = 99
  )
  
  # Validate the time_category input
  if (!(time_category %in% names(time_map))) {
    stop("Error: Choose one of 'less_than_3years', 'three_to_6years', 'more_than_6years', or 'unknown'.")
  }
  
  # Identify length-of-relationship-related columns
  length_cols_string <- grep("^name\\d+length$", colnames(persnet_row), value = TRUE)
  
  # Handle missing columns
  if (length(length_cols_string) == 0) {
    stop("Error: Cannot find columns related to how long alters have known the ego.")
  }
  
  # Select length-of-relationship columns
  length_cols <- persnet_row %>% dplyr::select(name1length:name15length)
  
  # Calculate and return the proportion of alters within the specified time category
  return(
    length(which(length_cols == time_map[[time_category]])) /
    sum(persnet_row %>% dplyr::select(tie1:tie15) != 0, na.rm = TRUE)
  )
}