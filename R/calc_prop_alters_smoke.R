#' Proportion of alters who smoke
#'
#' Calculates the proportion of alters in a personal network who smoke.
#'
#' @param persnet_row A single row of a personal network data frame.
#'
#' @return The proportion of an ego's alters who had likely smoked in the past 3 months.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' calc_prop_alters_smoke(persnet_row)
calc_prop_alters_smoke <- function(persnet_row) {
  ##########
  # Function: Computes the proportion of alters who do smoke  
  # Inputs:  
  #   persnet_row = A single row of a personal network data frame
  # Outputs:  
  #   Proportion of alters for whom alter answered 1 or 0 to smoking questions
  ##########
  
  # Identify smoking-related columns
  smoke_cols_string <- grep("^name\\d+smoke$", colnames(persnet_row), value = TRUE)
  
  # Test if any of smoking columns are missing
  if (length(smoke_cols_string) == 0) {
    warning("Error: Cannot find variables related to whether alters smoke.")
    return(NA)
  }
  
  # Select smoking-related columns
  smoke_cols <- persnet_row %>% dplyr::select(name1smoke:name15smoke)
  
  # Calculate and return the proportion of alters who do smoke
  prop_smoke_value <- sum(length(which(smoke_cols == 0)),length(which(smoke_cols == 1))) /
    sum(persnet_row %>% dplyr::select(tie1:tie15) != 0, na.rm = TRUE)
  return(
    round(prop_smoke_value,2)
  )
}