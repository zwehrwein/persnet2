#' Proportion of alters by racial identity
#'
#' Calculates the proportion of alters in a personal network who belong to a specified racial category.
#'
#' @param persnet_row A single row of a personal network data frame.
#' @param race_category One of 'black', 'white', 'native_american', 'asian', 'pacific_islander', or 'unknown'.
#'
#' @return The proportion of alters within the specified racial category.
#' @importFrom magrittr %>%
#' @export
#'

calc_prop_alters_race_identity <- function(persnet_row, race_category) {
  ##########
  # Function: Computes the proportion of alters who belong to a specified  
  #           racial category.
  # Inputs:  
  #   persnet_row  = A single row of a personal network data frame
  #   race_category = One of 'black', 'white', 'native_american',  
  #                   'asian', 'pacific_islander', or 'unknown'
  # Outputs:  
  #   Proportion of alters within the specified racial category
  ##########
  
  # Map race categories to numeric codes
  race_map <- c(
    black = 1,
    white = 2,
    native_american = 3,
    asian = 4,
    pacific_islander = 5,
    unknown = 77
  )
  
  # Validate the race_category input
  if (!(race_category %in% names(race_map))) {
    stop("Error: Choose one of 'black', 'white', 'native_american', 'asian', 'pacific_islander', or 'unknown'.")
  }
  
  # Identify race-related columns
  race_cols_string <- grep("^name\\d+race$", colnames(persnet_row), value = TRUE)
  
  # Handle missing columns
  if (length(race_cols_string) == 0) {
    stop("Error: Cannot find columns related to race.")
  }
  
  # Select race columns
  race_cols <- persnet_row %>% dplyr::select(name1race:name15race)
  
  # Calculate and return the proportion of alters within the specified race category
  return(
    length(which(race_cols == race_map[[race_category]])) /
    sum(persnet_row %>% dplyr::select(tie1:tie15) != 0, na.rm = TRUE)
  )
}