#' Calculate Blau heterophily index for a given alter attribute
#'
#' Computes the Blau heterophily index for a specified alter attribute.
#' The index measures diversity within a personal network.
#' n.b. as alters can be of multiple 'race' and offer several 'type of support', code is included
#' that can be modified/used if appropriate for users. Blau heterophily not defined for overlaping categories
#'
#' @param persnet_row A single row of a personal network data frame.
#' @param attribute One of 'gender', 'educ',
#'        'distance' (how far alters live from ego), 'length' (how long alters have
#'        known ego), or 'speak' (how often alters speak to ego).
#'
#' @return Blau heterophily index for the specified attribute.
#' @importFrom magrittr %>%
#' @export
calc_blau_alter_heterophily <- function(persnet_row, attribute = NULL) {
  ##########
  # Function: Computes the Blau heterophily index for a specified alter attribute.  
  #           The index measures diversity within a personal network.
  # Inputs:  
  #   persnet_row = A single row of a personal network data frame
  #   attribute   = One of 'gender', 'race', 'educ', 'support' (types of support),  
  #                 'distance' (how far alters live from ego), 'length'  
  #                 (how long alters have known ego), or 'speak'  
  #                 (how often alters speak to ego)
  # Outputs:  
  #   Blau heterophily index for the specified attribute
  ##########
  
  valid_attributes <- c(
    "gender", "educ", 
    "distance", "length", "speak",
    "support", "relationships_family", "race"
  ) #
  
  if (is.null(attribute) || is.na(attribute) || !(attribute %in% valid_attributes)) {
    warning("Error: Choose one of 'gender', 'educ', 
    'support' (types of support), 'distance' (how far alters live from ego),
    'length' (how long alters have known ego), or 'speak' (how often alters speak to ego).")
    return(NA)
  } #'relationships_family' (family vs non-family),
  
  # Calculate and return Blau heterophily index for gender; this is meaningless?
  if (attribute == 'gender') {
    gender_cols <- persnet_row %>% dplyr::select(name1sex:name15sex)
    prop_men <- sum(gender_cols, na.rm = TRUE) / sum(persnet_row %>% dplyr::select(tie1:tie15) != 0, na.rm = TRUE)
    prop_women <- length(which(gender_cols == 0)) / sum(persnet_row %>% dplyr::select(tie1:tie15) != 0, na.rm = TRUE)
    blau_index_gender <- 1 - (prop_men^2 + prop_women^2)
    return(blau_index_gender)                               
  }
  
  # Calculate and return Blau heterophily index for race; commented out as by default
  ## alters can be of multiple races, and Blau hterophily is not defined for overlapping categories
  if (attribute == 'race') {
    prop_black <- calc_prop_alters_race_identity(persnet_row,"black") 
    prop_white <- calc_prop_alters_race_identity(persnet_row,"white")  
    prop_native_american <- calc_prop_alters_race_identity(persnet_row,"native_american")  
    prop_asian <- calc_prop_alters_race_identity(persnet_row,"asian")
    prop_pacific_islanders <- calc_prop_alters_race_identity(persnet_row,"pacific_islander")  
    prop_unknown <- calc_prop_alters_race_identity(persnet_row,"unknown") 
    blau_index_race <- 1 - sum(prop_black^2,
                               prop_white^2,
                               prop_native_american^2,
                               prop_asian^2,
                               prop_pacific_islanders^2,
                               prop_unknown^2
                               )
    return(blau_index_race)                               
  }
  
  # Calculate and return Blau heterophily index for education
  if (attribute == 'educ') {
    prop_only_highschool <- calc_prop_alters_education_level(persnet_row,'only_high_school')  
    prop_some_college <- calc_prop_alters_education_level(persnet_row,'some_college')  
    prop_college_grad <- calc_prop_alters_education_level(persnet_row,'college_grad')  
    prop_dont_know_edu <- calc_prop_alters_education_level(persnet_row,'dont_know')
    blau_index_educ <- 1 - sum(prop_only_highschool^2, prop_some_college^2, prop_college_grad^2, prop_dont_know_edu^2)
    return(blau_index_educ)                               
  }
  
  # Blau heterophily assumes mutually exclusive categories,
  # but alters can be in multiple relationships and offer multiple types of support.
  
  # Blau index for relationships (commented out)
  # if (attribute == 'relationships_family') {
  #   prop_relationships_spouse <- prop_alters_relationship(persnet_row, 'spouse') 
  #   prop_relationships_family <- prop_alters_relationship(persnet_row, 'family') 
  #   prop_relationships_friend <- prop_alters_relationship(persnet_row, 'friend') 
  #   prop_relationships_advice <- prop_alters_relationship(persnet_row, 'advice') 
  #   prop_relationships_coworker <- prop_alters_relationship(persnet_row, 'coworker') 
  #   prop_relationships_other <- prop_alters_relationship(persnet_row, 'other') 
  
  #   blau_index_relationships <- 1 - sum(
  #                              prop_relationships_spouse^2,
  #                              prop_relationships_family^2,
  #                              prop_relationships_friend^2,
  #                              prop_relationships_advice^2,
  #                              prop_relationships_coworker^2,
  #                              prop_relationships_other^2
  #                              )
  #   return(blau_index_relationships)                               
  # }
  
  # Calculate and return Blau heterophily index for distance
  if (attribute == 'distance') {
    blau_index_distance <- 1 - sum(
      calc_prop_alters_distance_away(persnet_row,"same_house")^2,
      calc_prop_alters_distance_away(persnet_row,"1_5miles")^2,
      calc_prop_alters_distance_away(persnet_row,"6_15miles")^2,
      calc_prop_alters_distance_away(persnet_row,"16_50miles")^2,
      calc_prop_alters_distance_away(persnet_row,"more50miles")^2
    )
    return(blau_index_distance)
  }

  # Calculate and return Blau heterophily index for frequency of speaking
  if (attribute == 'speak') {
    blau_index_speak <- 1 - sum(
      calc_prop_alters_freq_speak(persnet_row,'daily')^2,
      calc_prop_alters_freq_speak(persnet_row,'weekly')^2,
      calc_prop_alters_freq_speak(persnet_row,'monthly')^2,
      calc_prop_alters_freq_speak(persnet_row,'more_monthly')^2,
      calc_prop_alters_freq_speak(persnet_row,'dont_know')^2
    )
    return(blau_index_speak)
  }
  
  # Calculate and return Blau heterophily index for length of knowing ego
  if (attribute == 'length') {
    blau_index_length <- 1 - sum(
      calc_prop_alters_known_length(persnet_row,"less_than_3years")^2,
      calc_prop_alters_known_length(persnet_row,"three_to_6years")^2,
      calc_prop_alters_known_length(persnet_row,"more_than_6years")^2,
      calc_prop_alters_known_length(persnet_row,"unknown")^2
    )
    return(blau_index_length)
  }
}