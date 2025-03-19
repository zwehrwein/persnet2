#' Calculate Egoless Density from a persnet dataframe
#'
#' Computes the density of each personal network after removing the ego node from each network 
#' contained in a persnet dataframe.
#'
#' @param persnet_df A dataframe containing personal network data.
#'
#' @return A vector containing the egoless density for each network.
#' @importFrom magrittr %>%
#' @export
#'

calc_egoless_density_df <- function(persnet_df) {
  tryCatch({
    # Organize a list of tidygraphs from the persnet dataframe
    gra_list <- organize_list_tidygraphs(persnet_df)
    # Calculate the density of each network after removing the ego node
    vector_egoless_density <- sapply(gra_list, calc_egoless_density)
    return(vector_egoless_density)  
  }, error = function(e) { 
    # If an error occurs, stop with a message indicating that the input is not a valid persnet dataframe
    stop("Error: not a valid persnet dataframe")  
  })
}