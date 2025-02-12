#' Calculate Effective Network Size (ENS) for all nodes
#'
#' Computes the Effective Network Size (ENS) for all nodes in a given
#' tidygraph object, returning a named vector of ENS values.
#'
#' @param tidygra A tidygraph object representing a personal network
#' @return A named vector containing ENS values for all nodes
#' @export
#'
#' @references
#' Burt, R. S. (1995). *Structural Holes: The Social Structure of Competition*.
#' Harvard University Press.
#'
#' @examples
#' \dontrun{
#' library(tidygraph)
#' graph <- play_erdos_renyi(10, 0.3, directed = FALSE) # Example graph
#' calc_all_nodes_ens(graph)
#' }
calc_all_nodes_ens <- function(tidygra) {
  ##########
  # Function: Computes the Effective Network Size (ENS) for all nodes in a given  
  #           tidygraph object, returning a named vector of ENS values.
  # Inputs:  
  #   tidygra = A tidygraph object representing a personal network
  # Outputs:  
  #   A named vector containing ENS values for all nodes
  ##########
  
  # Test if the input is valid
  if (is.null(tidygra) || !"tbl_graph" %in% class(tidygra)) {
    stop("The input graph must be a tidygraph graph object.")
  }
  
  # Extract node names from the graph
  node_names <- V(tidygra)$name  
  
  # Compute ENS for each node
  list_ens_scores <- sapply(node_names, function(node_name) {
    calc_node_ens(tidygra, node_name)  # Apply calc_node_ens function
  })
  
  # Assign node names to the ENS scores
  names(list_ens_scores) <- node_names
  
  return(list_ens_scores) 
}