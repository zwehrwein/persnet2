#' Calculate Ego Effective Network Size (ENS) from a persnet dataframe
#'
#' Computes the effective network size (ENS) for a persnet dataframe
#'
#' @param persnet_df A dataframe containing personal network data.
#'
#' @return A named vector containing the effective network size (ENS) for each node in the network.
#' @importFrom magrittr %>%
#' @export
#'

calc_ego_ens_df <- function(persnet_df) {
  tryCatch({
    # take in a persnet dataframe and organize a list of tidygraphs
    gra_list <- organize_list_tidygraphs(persnet_df)
    # calculate ego's effective network size for each graph in the list
    vector_ego_ens <- sapply(gra_list, calc_node_ens)
    return(vector_ego_ens)  
  }, error = function(e) { 
    # if an error occurs, stop with a message indicating that the input is not a valid personal network dataframe
    stop("Error: not a persnet dataframe")  
  })
}