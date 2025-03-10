#' Calculate the proportion of alters living at a specified distance from ego
#'
#' Computes the proportion of alters who live within a specified distance category from the ego.
#'
#' @param persnet_row A single row of a personal network data frame.
#' @param distance_category One of 'same_house', '1_5miles', '6_15miles', '16_50miles', or 'more50miles'.
#'
#' @return The proportion of alters within the specified distance category.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' calc_prop_alters_distance_away(persnet_row, "same_house")
#' calc_prop_alters_distance_away(persnet_row, "1_5miles")
#' calc_prop_alters_distance_away(persnet_row, "6_15miles")
#' calc_prop_alters_distance_away(persnet_row, "16_50miles")
#' calc_prop_alters_distance_away(persnet_row, "more50miles")
calc_prop_alters_distance_away <- function(persnet_row, distance_category) {
  ##########
  # Function: Computes the proportion of alters who live within a specified  
  #           distance category from the ego.
  # Inputs:  
  #   persnet_row       = A single row of a personal network data frame
  #   distance_category = One of 'same_house', '1_5miles', '6_15miles',  
  #                       '16_50miles', or 'more50miles'
  # Outputs:  
  #   Proportion of alters within the specified distance category
  ##########
  
  # Map distance categories to numeric codes
  distance_map <- c(
    same_house = 1,
    "1_5miles" = 2,
    "6_15miles" = 3,
    "16_50miles" = 4,
    more50miles = 5
  )
  
  # Validate the distance_category input
  if (!(distance_category %in% names(distance_map))) {
    stop("Error: Choose one of 'same_house', '1_5miles', '6_15miles', '16_50miles', or 'more50miles'.")
  }
  
  # Identify distance-related columns
  dist_cols_string <- grep("^name\\d+dist$", colnames(persnet_row), value = TRUE)
  
  # Handle missing columns
  if (length(dist_cols_string) == 0) {
    stop("Error: Cannot find columns related to how far away alters live.")
  }
  
  # Select distance columns
  dist_cols <- persnet_row %>% dplyr::select(name1dist:name15dist)
  
  # Calculate and return the proportion of alters within the specified distance
  return(
    length(which(dist_cols == distance_map[[distance_category]])) /
    sum(persnet_row %>% dplyr::select(tie1:tie15) != 0, na.rm = TRUE)
  )
}