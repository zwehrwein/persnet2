#' Calculate the minimum degree of a network after removing the ego node
#'
#' Computes the minimum degree of nodes in a personal network after removing the ego node.
#'
#' @param tg_graph A tidygraph object representing a personal network.
#'
#' @return The minimum degree of nodes in the network (excluding ego).
#'         Returns 0 if the graph is empty and NA if an error occurs.
#' @export
#'
#' @examples
#' calc_egoless_min_degree(tg_graph)
calc_egoless_min_degree <- function(tg_graph) {
  ##########
  # Function: Computes the minimum degree of nodes in a personal  
  #           network after removing the ego node.
  # Inputs:  
  #   tg_graph = A tidygraph object representing a personal network
  # Outputs:  
  #   Minimum degree of nodes in the network (excluding ego)
  ##########
  
  tryCatch({
    # Remove ego
    egoless_graph <- remove_ego_from_igraph(tg_graph)
    
    # Check if the graph is empty (no nodes)
    if (igraph::vcount(egoless_graph) == 0) {
      return(0)
    }
    
    # Else calculate min degree
    return(min(igraph::degree(egoless_graph)))
  }, error = function(e) {
    # If an error occurs, return NA
    return(NA)
  })
}