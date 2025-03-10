#' Calculate the proportion of alters based on their education level
#'
#' Computes the proportion of alters who fall into a specified education level category.
#'
#' @param persnet_row A single row of a personal network data frame.
#' @param education_level One of 'only_high_school', 'some_college', 'college_grad', or 'dont_know'.
#'
#' @return The proportion of alters within the specified education level.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' calc_prop_alters_education_level(persnet_row, "only_high_school")
#' calc_prop_alters_education_level(persnet_row, "some_college")
#' calc_prop_alters_education_level(persnet_row, "college_grad")
#' calc_prop_alters_education_level(persnet_row, "dont_know")
calc_prop_alters_education_level <- function(persnet_row, education_level) {
  ##########
  # Function: Computes the proportion of alters who fall into a specified  
  #           education level category.
  # Inputs:  
  #   persnet_row     = A single row of a personal network data frame
  #   education_level = One of 'only_high_school', 'some_college',  
  #                     'college_grad', or 'dont_know'
  # Outputs:  
  #   Proportion of alters within the specified education level
  ##########
  
  # Map education levels to numeric codes
  education_map <- list(
    only_high_school = c(1, 2),
    some_college = c(3, 4),
    college_grad = c(5, 6),
    dont_know = c(99)
  )
  
  # Validate the education_level input
  if (!(education_level %in% names(education_map))) {
    stop("Error: Choose one of 'only_high_school', 'some_college', 'college_grad', or 'dont_know'.")
  }
  
  # Identify education-related columns
  edu_cols_string <- grep("^name\\d+educ$", colnames(persnet_row), value = TRUE)
  
  # Handle missing columns
  if (length(edu_cols_string) == 0) {
    stop("Error: Cannot find columns related to education.")
  }
  
  # Select education columns
  edu_cols <- persnet_row %>% dplyr::select(name1educ:name15educ)
  
  # Calculate and return the proportion of alters within the specified education level
  return(
    length(which(edu_cols %in% education_map[[education_level]])) /
    sum(persnet_row %>% dplyr::select(tie1:tie15) != 0, na.rm = TRUE)
  )
}