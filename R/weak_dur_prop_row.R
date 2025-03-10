#' Calculate proportion of `weak' ties as defined as knowing ego less than six years
#'
#' Computes the proportion of alters who have known the ego for less than 6 years in a persnet dataframe row.
#' This is done by summing the proportions of alters who have known the ego for "less_than_3years" and 
#' "three_to_6years", and then rounding the result to two decimal places.
#'
#' @param persnet_row A single row of a persnet dataframe.
#'
#' @return A numeric value representing the proportion of alters who have known the ego for less than 6 years.
#' @export
#'
#' @examples
#' # Assuming persnet_row is a row from your persnet dataframe:
#' prop_less6years <- prop_alters_know_less6years_row(persnet_row)
weak_dur_prop_row <- function(persnet_row) {
  # Proportion of alters who have known the ego for less than 6 years
  return(
    round(
      calc_prop_alters_known_length(persnet_row, "less_than_3years") +
      calc_prop_alters_known_length(persnet_row, "three_to_6years"),
      2
    )
  )
}