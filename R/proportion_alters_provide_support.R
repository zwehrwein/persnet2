#' Calculate alters who provide some kind of support to ego
#'
#' Computes the proportion of alters who live more than 15 miles away from the ego in a persnet dataframe row.
#' This is done by summing the proportions of alters categorized as "16_50miles" and "more50miles", and rounding the result to two decimal places.
#'
#' @param persnet_row A single row of a persnet dataframe.
#'
#' @return A numeric value representing the proportion of alters living more than 15 miles away.
#' @export
#'
proportion_alters_provide_support <- function(persnet_row){
  support_cols <- persnet_row %>% select(name1support:name15support)
  return(length(which(support_cols == 1))/sum(persnet_row %>% 
                                                dplyr::select(tie1:tie15) != 0, na.rm = TRUE))
}