#' Calculate proportion of alters with specific health problems
#'
#' The function identifies alter health problem-related columns (excluding those for "healthy" or missing values)
#' and calculates the ratio of the sum of these columns to the total number of alters (based on tie columns).
#' The result is rounded to two decimal places.
#'
#' @param persnet_row A single row of a persnet dataframe.
#'
#' @return A numeric value representing the proportion of alters with health problems.
#' @export
#'

alter_health_problems_row <- function(persnet_row) {
  alter_health_cols_string <- grep("^name\\d+health___\\d+$", colnames(persnet_row), value = TRUE)
  if (length(alter_health_cols_string) == 0) {
    stop("Error: cannot find variables related to alter health problems.")
  } else {
    alter_health_problem_cols <- persnet_row %>% 
      dplyr::select(name1health___1:name15health___99) %>%
      dplyr::select(!dplyr::contains("___99")) %>% 
      dplyr::select(!dplyr::contains("___0"))
    
    return(
      round(
        sum(alter_health_problem_cols) /
          sum(persnet_row %>% dplyr::select(tie1:tie15) != 0, na.rm = TRUE),
        2
      )
    )
  }
}