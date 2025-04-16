#' Calculate proportion of alters who are men
#' 
#'
#' @param persnet_row A single row of a persnet dataframe.
#'
#' @return A numeric value representing the proportion of alters who are men
#' @export
#'
proporton_alters_male <- function(persnet_row){
  sex_cols <- persnet_row %>% select(name1sex:name15sex)
  return(length(which(sex_cols == 1))/sum(persnet_row %>% 
                                            dplyr::select(tie1:tie15) != 0, na.rm = TRUE))
}