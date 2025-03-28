#' Computes the mean degree (i.e., the lowest number of connections)
#' for each personal network after removing the ego node, using a persnet dataframe as input.
#'
#' @param persnet_df A dataframe containing personal network data.
#'
#' @return A vector containing the egoless mean degree for each network.
#' @importFrom magrittr %>%
#' @export
#'

calc_egoless_mean_degree_df <- function(persnet_df) {
  tryCatch({
    # Organize a list of tidygraphs from the persnet dataframe
    gra_list <- organize_list_tidygraphs(persnet_df)
    # Calculate the mean degree for each network after removing the ego 
    vector_mean_degree <- sapply(gra_list, calc_egoless_mean_degree)
    return(vector_mean_degree)  
  }, error = function(e) { 
    # If an error occurs, stop with a message indicating that the input is not a valid persnet dataframe
    stop("Error: not a valid persnet dataframe")  
  })
}