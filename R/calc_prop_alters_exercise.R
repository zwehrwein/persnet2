#' Calculate the proportion of alters who do not exercise regularly
#'
#' Computes the proportion of alters who do not exercise regularly.
#'
#' @param persnet_row A single row of a personal network data frame.
#'
#' @return The proportion of alters who do not exercise regularly.
#' @importFrom magrittr %>%
#' @export
#'

calc_prop_alters_exercise <- function(persnet_row) {
  ##########
  # Function: Computes the proportion of alters who do not exercise regularly  
  # Inputs:  
  #   persnet_row = A single row of a personal network data frame
  # Outputs:  
  #   Proportion of alters who do not exercise regularly
  ##########
  
  # Identify exercise-related columns
  exer_cols_string <- grep("^name\\d+exer$", colnames(persnet_row), value = TRUE)
  
  # Handle missing columns
  if (length(exer_cols_string) == 0) {
    stop("Error: Cannot find variables related to whether alters exercised regularly.")
  }
  
  # Select exercise-related columns
  exer_cols <- persnet_row %>% dplyr::select(name1exer:name15exer)
  
  # Calculate and 1 minus the proportion of alters who exercise regularly
  return(
    round((1 - length(which(exer_cols == 1)) /
    sum(persnet_row %>% dplyr::select(tie1:tie15) != 0, na.rm = TRUE)),2)
  )
}