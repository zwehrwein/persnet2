#' Calculate the proportion of alters with a good diet
#'
#' Computes the proportion of alters who have a good diet.
#'
#' @param persnet_row A single row of a personal network data frame.
#'
#' @return The proportion of alters who `ate a healthy diet regularly over the past 3 months'
#' @importFrom magrittr %>%
#' @export
#'

calc_prop_alters_diet <- function(persnet_row) {
  ##########
  # Function: Computes the proportion of alters who have a good diet  
  # Inputs:  
  #   persnet_row = A single row of a personal network data frame
  # Outputs:  
  #   Proportion of alters ego knew ``ate a healthy diet regularly over the past 3 months'
  ##########
  
  # Identify diet-related columns
  diet_cols_string <- grep("^name\\d+diet$", colnames(persnet_row), value = TRUE)
  
  # Handle missing columns
  if (length(diet_cols_string) == 0) {
    warning("Error: Cannot find variables related to whether alters had a good diet.")
    return(NA)
  }
  
  # Select diet-related columns
  diet_cols <- persnet_row %>% dplyr::select(name1diet:name15diet)
  
  # Calculate and return the proportion of alters with a good diet
  return(
    length(which(diet_cols == 0)) /
    sum(persnet_row %>% dplyr::select(tie1:tie15) != 0, na.rm = TRUE)
  )
}