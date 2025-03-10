#' Remove the ego node from a tidygraph object
#'
#' Removes the ego node from a tidygraph object, returning a graph without the ego.
#' This function is useful for analyzing the structure of an individual's network 
#' without the central ego node.
#'
#' @param tg_graph A tidygraph object representing a personal network.
#'
#' @return A tidygraph object without the ego node.
#' @export
#'
#' @examples
#' remove_ego_from_igraph(tg_graph)
remove_ego_from_igraph <- function(tg_graph) {
  ##########
  # Function: Removes the ego node from a tidygraph object, returning  
  #           a graph without the ego.
  # Inputs:  
  #   tg_graph = A tidygraph object representing a personal network
  # Outputs:  
  #   A tidygraph object without the ego node
  ##########
  
  tg_graph_egoless <- tg_graph %>%
    tidygraph::activate(nodes) %>%
    dplyr::filter(name != "ego")
  
  return(tg_graph_egoless)
}