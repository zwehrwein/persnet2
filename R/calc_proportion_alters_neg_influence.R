#' Calculate proportion of alters ego answered had a negative influence
#' 
#' "Do any people in your network have a
#' negative in#uence on your health? For example, some people can passively or 
#' actively encourage you to smoke, not eat well, or not exercise."
#'
#' @param persnet_row A single row of a persnet dataframe.
#'
#' @return A numeric value representing the proportion of alters who negatively impact ego
#' @export
#'
proportion_alters_neg_influence <- function(persnet_row){
  neg_cols <- persnet_row %>% select(name1neg:name15neg)
  return(length(which(neg_cols == 1))/sum(persnet_row %>% 
                                            dplyr::select(tie1:tie15) != 0, na.rm = TRUE))
}