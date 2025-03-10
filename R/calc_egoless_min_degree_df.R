#' Calculate egoless minimum degree from a persnet dataframe
#'
#' Computes the minimum degree (i.e., the lowest number of connections)
#' for each personal network after removing the ego node, using a persnet dataframe as input.
#'
#' @param persnet_df A dataframe containing personal network data.
#'
#' @return A vector containing the egoless minimum degree for each network.
#' @importFrom magrittr %>%
#' @export
#'

calc_egoless_min_degree_df <- function(persnet_df) {
  tryCatch({
    # Organize a list of tidygraphs from the persnet dataframe
    gra_list <- organize_list_tidygraphs(persnet_df)
    # Calculate the minimum degree for each network after removing the ego node
    vector_min_degree <- sapply(gra_list, calc_egoless_min_degree)
    return(vector_min_degree)  
  }, error = function(e) { 
    # If an error occurs, stop with a message indicating that the input is not a valid persnet dataframe
    stop("Error: not a valid persnet dataframe")  
  })
}