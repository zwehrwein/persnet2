#' Calculate the maximum degree of a network after removing the ego node
#'
#' Computes the maximum degree of nodes in a personal network after removing the ego node.
#'
#' @param tg_graph A tidygraph object representing a personal network.
#'
#' @return The maximum degree of nodes in the network (excluding ego).
#'         Returns 0 if the graph is empty and NA if an error occurs.
#' @export
#'

calc_egoless_max_degree <- function(tg_graph) {
  ##########
  # Function: Computes the maximum degree of nodes in a personal  
  #           network after removing the ego node.
  # Inputs:  
  #   tg_graph = A tidygraph object representing a personal network
  # Outputs:  
  #   Maximum degree of nodes in the network (excluding ego)
  ##########
  
  tryCatch({
    # Remove ego
    egoless_graph <- remove_ego_from_igraph(tg_graph)
    
    # Check if the graph is empty, if so return 0
    if (igraph::vcount(egoless_graph) == 0) {
      return(0)
    }
    
    # Else calculate max degree
    return(max(igraph::degree(egoless_graph)))
  }, error = function(e) {
    # If an error occurs, return NA
    return(NA)
  })
}