#' Calculate Legacy Kin Proportion for a Personal Network Row
#'
#' Computes the legacy kin proportion for a given personal network row by summing the 
#' proportions of alters classified as "spouse" and "family".
#'
#' @param persnet_row A single row of a personal network dataframe.
#'
#' @return A numeric value representing the legacy kin proportion for the given row.
#' @export
#'

prop_kin_persnet_row <- function(persnet_row) {
  prop_kin <- calc_prop_alters_relationship(persnet_row, "spouse") +
         calc_prop_alters_relationship(persnet_row, "family")
  return(round(prop_kin,2))
}