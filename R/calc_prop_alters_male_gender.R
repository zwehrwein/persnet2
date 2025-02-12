#' Calculate the proportion of alters who are men
#'
#'
#' @param persnet_row A single row of a personal network data frame.
#'
#' @return The proportion of alters who are men.
#' @export
#'
#' @examples
#' calc_prop_alters_male_gender(persnet_row)
calc_prop_alters_male_gender <- function(persnet_row) {
  ##########
  # Function: Computes the proportion of alters who are male  
  # Inputs:  
  #   persnet_row = A single row of a personal network data frame
  # Outputs:  
  #   Proportion of alters who are male
  ##########
  
  # Identify gender-related columns
  gender_cols_string <- grep("^name\\d+sex$", colnames(persnet_row), value = TRUE)
  
  # Handle missing columns
  if (length(gender_cols_string) == 0) {
    stop("Error: Cannot find columns related to gender.")
  }
  
  # Select gender-related columns
  gender_cols <- persnet_row %>% select(name1sex:name15sex)
  
  # Calculate and return the proportion of alters who are male
  return(
    length(which(gender_cols == 1)) /
    sum(persnet_row %>% select(tie1:tie15) != 0, na.rm = TRUE)
  )
}