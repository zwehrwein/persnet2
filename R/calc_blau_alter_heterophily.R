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
#' @export
#'
#' @examples
#' calc_blau_alter_heterophily(persnet_row, "gender")
#' calc_blau_alter_heterophily(persnet_row, "educ")
#' calc_blau_alter_heterophily(persnet_row, "distance")
#' calc_blau_alter_heterophily(persnet_row, "speak")
#' calc_blau_alter_heterophily(persnet_row, "length")
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
    "support", "distance", "length", "speak"
  ) #"relationships_family", "race",
  
  if (is.null(attribute) || is.na(attribute) || !(attribute %in% valid_attributes)) {
    stop("Error: Choose one of 'gender', 'educ', 
    'support' (types of support), 'distance' (how far alters live from ego),
    'length' (how long alters have known ego), or 'speak' (how often alters speak to ego).")
  } #'relationships_family' (family vs non-family),
  
  # Calculate and return Blau heterophily index for gender; this is meaningless?
  if (attribute == 'gender') {
    gender_cols <- persnet_row %>% select(name1sex:name15sex)
    prop_men <- sum(gender_cols, na.rm = TRUE) / sum(persnet_row %>% select(tie1:tie15) != 0, na.rm = TRUE)
    prop_women <- length(which(gender_cols == 0)) / sum(persnet_row %>% select(tie1:tie15) != 0, na.rm = TRUE)
    blau_index_gender <- 1 - (prop_men^2 + prop_women^2)
    return(blau_index_gender)                               
  }
  
  # Calculate and return Blau heterophily index for race; commented out as by default
  ## alters can be of multiple races, and Blau hterophily is not defined for overlapping categories
  #if (attribute == 'race') {
  #  prop_black <- prop_alters_race_black(persnet_row) 
  #  prop_white <- prop_alters_race_white(persnet_row)  
  #  prop_native_american <- prop_alters_race_native_american(persnet_row)  
  #  prop_asian <- prop_alters_race_asian(persnet_row)
  #  prop_pacific_islanders <- prop_alters_race_pacific_islander(persnet_row)  
  #  #prop_unknown <- prop_alters_race_unknown(persnet_row) 
  #  blau_index_race <- 1 - sum(prop_black^2,
  #                             prop_white^2,
  #                             prop_native_american^2,
  #                             prop_asian^2,
  #                             prop_pacific_islanders^2
  #                             #,prop_unknown^2
  #                             )
  #  return(blau_index_race)                               
  #}
  
  # Calculate and return Blau heterophily index for education
  if (attribute == 'educ') {
    prop_only_highschool <- prop_alters_only_high_school_edu(persnet_row)  
    prop_some_college <- prop_alters_some_college_edu(persnet_row)  
    prop_college_grad <- prop_alters_college_grad_edu(persnet_row)  
    prop_dont_know_edu <- prop_alters_dont_know_educ(persnet_row)
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
      prop_alters_same_house(persnet_row)^2,
      prop_alters_1_5miles(persnet_row)^2,
      prop_alters_6_15miles(persnet_row)^2,
      prop_alters_16_50miles(persnet_row)^2,
      prop_alters_more50miles(persnet_row)^2
    )
    return(blau_index_distance)
  }

  # Calculate and return Blau heterophily index for frequency of speaking
  if (attribute == 'speak') {
    blau_index_speak <- 1 - sum(
      prop_alters_speak_daily(persnet_row)^2,
      prop_alters_speak_weekly(persnet_row)^2,
      prop_alters_speak_monthly(persnet_row)^2,
      prop_alters_speak_more_monthly(persnet_row)^2,
      prop_alters_speak_dont_know(persnet_row)^2
    )
    return(blau_index_speak)
  }
  
  # Calculate and return Blau heterophily index for length of knowing ego
  if (attribute == 'length') {
    blau_index_length <- 1 - sum(
      prop_alters_less_than_3years(persnet_row)^2,
      prop_alters_three_to_6years(persnet_row)^2,
      prop_alters_more_than_6years(persnet_row)^2,
      prop_alters_known_unknown(persnet_row)^2
    )
    return(blau_index_length)
  }
}