#' Proportion of alters with a negative influence on ego's health
#'
#' Calculates the proportion of alters in a personal network who negatively influence ego's health.
#'
#' @param persnet_row A single row of a personal network data frame.
#'
#' @return The proportion of alters who negatively influence ego's health.
#' @export
#'
#' @examples
#' calc_prop_alters_neg_health_influence(persnet_row)
calc_prop_alters_neg_health_influence <- function(persnet_row) {
  ##########
  # Function: Computes the proportion of alters who have a negative influence  
  #           on ego's health.
  # Inputs:  
  #   persnet_row = A single row of a personal network data frame
  # Outputs:  
  #   Proportion of alters who negatively influence ego's health
  ##########
  
  # Identify negative health influence-related columns
  neg_influence_cols_string <- grep("^name\\d+neg$", colnames(persnet_row), value = TRUE)
  
  # Handle missing columns
  if (length(neg_influence_cols_string) == 0) {
    stop("Error: Cannot find variables related to whether alters have a negative influence on ego's health.")
  }
  
  # Select negative health influence-related columns
  neg_influence_health_cols <- persnet_row %>% select(name1neg:name15neg)
  
  # Calculate and return the proportion of alters who negatively influence ego's health
  return(
    length(which(neg_influence_health_cols == 1)) /
    sum(persnet_row %>% select(tie1:tie15) != 0, na.rm = TRUE)
  )
}