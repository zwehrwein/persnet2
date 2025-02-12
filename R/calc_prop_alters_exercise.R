#' Calculate the proportion of alters who exercise regularly
#'
#' Computes the proportion of alters who exercise regularly.
#'
#' @param persnet_row A single row of a personal network data frame.
#'
#' @return The proportion of alters who exercise regularly.
#' @export
#'
#' @examples
#' calc_prop_alters_exercise(persnet_row)
calc_prop_alters_exercise <- function(persnet_row) {
  ##########
  # Function: Computes the proportion of alters who exercise regularly  
  # Inputs:  
  #   persnet_row = A single row of a personal network data frame
  # Outputs:  
  #   Proportion of alters who exercise regularly
  ##########
  
  # Identify exercise-related columns
  exer_cols_string <- grep("^name\\d+exer$", colnames(persnet_row), value = TRUE)
  
  # Handle missing columns
  if (length(exer_cols_string) == 0) {
    stop("Error: Cannot find variables related to whether alters exercised regularly.")
  }
  
  # Select exercise-related columns
  exer_cols <- persnet_row %>% select(name1exer:name15exer)
  
  # Calculate and return the proportion of alters who exercise regularly
  return(
    length(which(exer_cols == 0)) /
    sum(persnet_row %>% select(tie1:tie15) != 0, na.rm = TRUE)
  )
}