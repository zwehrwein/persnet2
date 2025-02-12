#' Proportion of alters who do not smoke
#'
#' Calculates the proportion of alters in a personal network who do not smoke.
#'
#' @param persnet_row A single row of a personal network data frame.
#'
#' @return The proportion of alters who do not smoke.
#' @export
#'
#' @examples
#' calc_prop_alters_smoke(persnet_row)
calc_prop_alters_smoke <- function(persnet_row) {
  ##########
  # Function: Computes the proportion of alters who do not smoke  
  # Inputs:  
  #   persnet_row = A single row of a personal network data frame
  # Outputs:  
  #   Proportion of alters who do not smoke
  ##########
  
  # Identify smoking-related columns
  smoke_cols_string <- grep("^name\\d+smoke$", colnames(persnet_row), value = TRUE)
  
  # Handle missing columns
  if (length(smoke_cols_string) == 0) {
    stop("Error: Cannot find variables related to whether alters smoke.")
  }
  
  # Select smoking-related columns
  smoke_cols <- persnet_row %>% select(name1smoke:name15smoke)
  
  # Calculate and return the proportion of alters who do not smoke
  return(
    length(which(smoke_cols == 0)) /
    sum(persnet_row %>% select(tie1:tie15) != 0, na.rm = TRUE)
  )
}