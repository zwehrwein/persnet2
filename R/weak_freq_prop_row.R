#' Calculate Proportion of Weak Frequency Alters for a persnet dataframe row
#'
#' Computes the proportion of alters who contact the ego either "monthly" or "more_monthly"
#' from a single row of a personal network data frame, and rounds the result to two decimal places.
#'
#' @param persnet_row A single row of a persnet dataframe row
#'
#' @return A numeric value representing the proportion of alters with weak contact frequency (contacted monthly or more than monthly)
#' @export
#'

weak_freq_prop_row <- function(persnet_row) {
  # Proportion of alters who contact ego monthly or less frequently
  return(
    round(
      calc_prop_alters_freq_speak(persnet_row, "monthly") +
      calc_prop_alters_freq_speak(persnet_row, "more_monthly"),
      2
    )
  )
}