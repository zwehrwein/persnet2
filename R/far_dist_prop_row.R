#' Calculate alters living far away from an ego 
#'
#' Computes the proportion of alters who live more than 15 miles away from the ego in a persnet dataframe row.
#' This is done by summing the proportions of alters categorized as "16_50miles" and "more50miles", and rounding the result to two decimal places.
#'
#' @param persnet_row A single row of a persnet dataframe.
#'
#' @return A numeric value representing the proportion of alters living more than 15 miles away.
#' @export
#'
#' @examples
#' # Assuming persnet_row is a row from your persnet dataframe:
#' far_distance_proportion <- far_dist_prop_row(persnet_row)
far_dist_prop_row <- function(persnet_row) {
  # Proportion of alters who live > 15 miles away
  return(
    round(
      calc_prop_alters_distance_away(persnet_row, "16_50miles") +
      calc_prop_alters_distance_away(persnet_row, "more50miles"),
      2
    )
  )
}