#' Calculate alters who provide some kind of support to ego
#'
#' Proportion of alters ego answered yes to "which person or persons support you most often?
#' You can choose more than one person."
#'
#' @param persnet_row A single row of a persnet dataframe.
#'
#' @return A numeric value representing the proportion of alters who "support ego most often"
#' @export
#'
proportion_alters_provide_support <- function(persnet_row){
  support_cols <- persnet_row %>% select(name1support:name15support)
  return(length(which(support_cols == 1))/sum(persnet_row %>% 
                                                dplyr::select(tie1:tie15) != 0, na.rm = TRUE))
}