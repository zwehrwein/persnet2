#' Calculate the Index of Qualitative Variation (IQV) for a given alter attribute
#'
#' Computes the Index of Qualitative Variation (IQV) for a specified alter attribute.
#' The IQV measures diversity and variation within a personal network.
#'
#' @param persnet_row A single row of a personal network data frame.
#' @param attribute One of 'race', 'gender', 'educ', 'relationships' (family vs non-family),
#'        or 'support' (types of support).
#'
#' @return The IQV value for the specified attribute.
#' @export
#'
#' @examples
#' calc_attribute_iqv(persnet_row, "race")
#' calc_attribute_iqv(persnet_row, "gender")
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
  
  # Define valid attributes for IQV calculation
  valid_attributes <- c(
    #"race",
    "gender",
    "educ", 
    "relationships", 
    "support"
  )
  
  # Validate the attribute input
  if (is.null(attribute) || is.na(attribute) || !(attribute %in% valid_attributes)) {
    stop("Error: Choose one of gender', 'educ', 'relationships' (family vs non-family),
    or 'support' (types of support)")
  }
  
  # Calculate IQV using Blau heterophily index and corresponding normalization factor
  #if (attribute == 'race') {
  #  return(
  #    calc_blau_alter_heterophily(persnet_row, 'race') / (1 - 1 / 5)
  #  )
  #}
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
  #if (attribute == 'relationships') {
  #  return(
  #    calc_blau_alter_heterophily(persnet_row, 'relationships') / (1 - 1 / 6)
  #  )
  #}
  #if (attribute == 'support') {
  #  return(
  #    calc_blau_alter_heterophily(persnet_row, 'support') / (1 - 1 / 5)
  #  )
  #}
}