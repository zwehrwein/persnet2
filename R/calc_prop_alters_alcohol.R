#' Calculate the proportion of alters who do not drink heavily or whose drinking habits are unknown
#'
#' Computes the proportion of alters who either do not drink heavily or whose drinking habits are unknown.
#'
#' @param persnet_row A single row of a personal network data frame.
#'
#' @return The proportion of alters who do not drink heavily or whose drinking habits are unknown.
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
  #   Proportion of alters who do not drink heavily or whose drinking habits are unknown
  ##########
  
  # Identify alcohol-related columns
  alcohol_cols_string <- grep("^name\\d+alcohol$", colnames(persnet_row), value = TRUE)
  
  # Handle missing columns
  if (length(alcohol_cols_string) == 0) {
    stop("Error: Cannot find variables related to whether alters drink alcohol regularly.")
  }
  
  # Select alcohol-related columns
  alcohol_cols <- persnet_row %>% select(name1alcohol:name15alcohol)
  
  # Calculate and return the proportion of alters who do not drink heavily or whose drinking habits are unknown
  return(
    length(which(alcohol_cols == 0 | alcohol_cols == 1)) /
    sum(persnet_row %>% select(tie1:tie15) != 0, na.rm = TRUE)
  )
}