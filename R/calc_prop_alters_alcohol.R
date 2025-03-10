#' Calculate the proportion of alters who who likely drank heavily in the past 3 months.
#'
#' Computes the proportion of alters who who likely drank heavily in the past 3 months.
#'
#' @param persnet_row A single row of a personal network data frame.
#'
#' @return The proportion of an ego's alters who likely drank heavily in the past 3 months.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' calc_prop_alters_alcohol(persnet_row)
calc_prop_alters_alcohol <- function(persnet_row) {
  ##########
  # Function: Computes the proportion of alters who either do not drink heavily  
  #           or whose drinking habits are unknown.
  # Inputs:  
  #   persnet_row = A single row of a personal network data frame
  # Outputs:  
  #   Proportion of alters for whom alter answered 1 or 0 to drink questions
  ##########
  
  # Identify alcohol-related columns
  alcohol_cols_string <- grep("^name\\d+alcohol$", colnames(persnet_row), value = TRUE)
  
  # Handle missing columns
  if (length(alcohol_cols_string) == 0) {
    warning("Error: Cannot find variables related to whether alters drink alcohol regularly.")
    return(NA)
  }
  
  # Select alcohol-related columns
  alcohol_cols <- persnet_row %>% dplyr::select(name1alcohol:name15alcohol)
  
  # Calculate and return the proportion of alters who do not drink heavily or whose drinking habits are unknown
  return(
    sum(length(which(alcohol_cols == 0)),length(which(alcohol_cols == 1))) /
    sum(persnet_row %>% dplyr::select(tie1:tie15) != 0, na.rm = TRUE)
  )
}