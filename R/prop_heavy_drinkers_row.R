#' Calculate proportion of heavy drinkers in a persnet dataframe
#'
#' The function searches for alcohol-related alter columns (e.g., "name1alcohol" to "name15alcohol") and,
#' if found, calculates the ratio of alters with heavy drinking (indicated by values of 0 or 1)
#' to the total number of alters (as determined by tie columns). The result is rounded to two decimal places.
#'
#' @param persnet_row A single row of a persnet dataframe.
#'
#' @return A numeric value representing the proportion of heavy drinkers among the alters.
#' @export
#'
#' @examples
#' # Assuming persnet_row is a row from your persnet dataframe:
#' heavy_drinkers <- heavy_drinkers_row(persnet_row)
prop_heavy_drinkers_row <- function(persnet_row) {
  alcohol_cols_string <- grep("^name\\d+alcohol$", colnames(persnet_row), value = TRUE)
  if (length(alcohol_cols_string) == 0) {
    warning("Error: cannot find variables related to whether alters drink alcohol regularly. Returning NA")
    return(NA)
  } else {
    alcohol_cols <- persnet_row %>% dplyr::select(name1alcohol:name15alcohol)
    return(
      round(  
        (length(which(alcohol_cols == 0)) + length(which(alcohol_cols == 1))) /
          sum(persnet_row %>% dplyr::select(tie1:tie15) != 0, na.rm = TRUE),
        2)
    )
  }
}