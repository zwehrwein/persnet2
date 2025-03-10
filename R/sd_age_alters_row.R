#' Calculate standard deviation of Ages for a persnet dataframe row
#'
#' Computes the standard deviation of the ages of the alters in an egocentric network
#' based on a single row of a persnet dataframe.
#'
#' @param persnet_row A single row of a personal network data frame.
#'
#' @return A numeric value representing the standard deviation of the alters' ages,
#'         rounded to two decimal places.
#'
#' @importFrom magrittr %>%
#' @export
#'

sd_age_alters_row <- function(persnet_row) {
  # Compute the standard deviation of the ages of the alters in the egonetwork
  return(
    round(stats::sd(persnet_row %>% dplyr::select(name1age:name15age) %>% unlist(), na.rm = TRUE), 2)
  )
}