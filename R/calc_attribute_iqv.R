#' Calculate the Index of Qualitative Variation (IQV) for a given alter attribute
#'
#' Computes the Index of Qualitative Variation (IQV) for a specified alter attribute.
#' The IQV measures diversity and variation within a personal network.
#'
#' @param persnet_row A single row of a personal network data frame.
#' @param attribute One of `gender` or `educ`.
#'
#' @return The IQV value for the specified attribute.
#' @export
calc_attribute_iqv <- function(persnet_row, attribute = NULL) {
  ##########
  # Function: Computes the Index of Qualitative Variation (IQV) for a specified
  #           alter attribute. The IQV measures diversity and variation within
  #           a personal network.
  # Inputs:
  #   persnet_row = A single row of a personal network data frame
  #   attribute   = One of 'gender', 'educ', 'relationships' (family vs non-family),
  #                 or 'support' (types of support)
  # Outputs:
  #   IQV value for the specified attribute
  ##########

  # define valid attributes for IQV calculation
  valid_attributes <- c(
    "race",
    "gender",
    "educ",
    "relationships",
    "support"
  )

  # validate the attribute input
  if (is.null(attribute) || is.na(attribute) || !(attribute %in% valid_attributes)) {
    warning("Error: Choose one of gender' or 'educ'")
    return(NA)
  }

  # calculate IQV using Blau heterophily index and corresponding normalization factor
  if (attribute == 'race') {
    return(
      calc_blau_alter_heterophily(persnet_row, 'race') / (1 - 1 / 5)
    )
  }
  if (attribute == 'gender') {
    return(
      calc_blau_alter_heterophily(persnet_row, 'gender') / (1 - 1 / 2)
    )
  }
  if (attribute == 'educ') {
    return(
      calc_blau_alter_heterophily(persnet_row, 'educ') / (1 - 1 / 4)
    )
  }
  if (attribute == 'relationships') {
    return(
      calc_blau_alter_heterophily(persnet_row, 'relationships') / (1 - 1 / 6)
    )
  }
  if (attribute == 'support') {
    return(
      calc_blau_alter_heterophily(persnet_row, 'support') / (1 - 1 / 5)
    )
  }
  # as an alter can be of mulitple `race', `relationships', or 'support', IQV is not defined, but code is included if
  ## a particular version of persnet happens to have mutually exclusive categories
}
