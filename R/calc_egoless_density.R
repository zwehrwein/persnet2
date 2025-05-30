#' Calculate the density of a network after removing the ego node
#'
#' Computes the density of a personal network graph after removing the ego node.
#'
#' @param tg_graph A tidygraph object representing a personal network.
#'
#' @return The density of the network without the ego node.
#' @export
#'

calc_egoless_density <- function(tg_graph) {
  ##########
  # Function: Computes the density of a personal network graph after  
  #           removing the ego node.
  # Inputs:  
  #   tg_graph = A tidygraph object representing a personal network
  # Outputs:  
  #   Density of the network without the ego node (rounded to 2 decimal places)
  ##########
  
  return(round(igraph::edge_density(remove_ego_from_igraph(tg_graph)), 2))
}