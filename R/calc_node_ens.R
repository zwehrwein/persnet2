#' Calculate the Effective Network Size (ENS) for a given node in a network
#'
#' Computes the Effective Network Size (ENS) for a specified node in a tidygraph object,
#' considering tie strengths. ENS is a measure of structural holes in a network.
#'
#' @param tidygra A tidygraph object representing a personal network.
#' @param node_index (Optional) The node for which ENS is calculated (default = "ego").
#'
#' @return The Effective Network Size (ENS) value for the specified node.
#'         Returns NA if the network is an isolate.
#' @importFrom magrittr %>%
#' @export
#'
#' @references
#' Burt, R. S. (1995). *Structural Holes: The Social Structure of Competition*.
#' Harvard University Press.
#'
#' @examples
#' calc_node_ens(tidygra, "ego")
#' calc_node_ens(tidygra, "burt")
calc_node_ens <- function(tidygra, node_index = NULL) {
  ##########
  # Function: Computes the Effective Network Size (ENS) for a specified node 
  #           in a tidygraph object, considering tie strengths.
  # Inputs:  
  #   tidygra    = A tidygraph object representing a personal network
  #   node_index = (Optional) The node for which ENS is calculated (default = "ego")
  # Outputs:  
  #   Effective Network Size (ENS) value for the specified node
  ##########
  
  # Test if the input is valid
  if (is.null(tidygra) || !"tbl_graph" %in% class(tidygra)) {
    stop("The input graph must be a tidygraph graph object.")
  }
  
  # Check if the network is an isolate and return NA if so
  if (dim(igraph::as_adjacency_matrix(tidygra))[1] < 2) return(NA)
  
  # Check if the edge attribute 'weight' is missing
  edge_attrs <- tidygra %>% tidygraph::activate(edges) %>% tibble::as_tibble()
  if (!"weight" %in% colnames(edge_attrs)) {
    warning("Warning: Network has no weights. Effective Network Size typically includes weight. Calculating with all tie weights set to 1.")
    tidygra <- tidygra %>%
      tidygraph::activate(edges) %>%
      dplyr::mutate(weight = 1)  # Add default weight of 1
  }
  
  # Convert to adjacency matrix
  adj_matrix <- igraph::as_adjacency_matrix(tidygra, attr = "weight", sparse = FALSE)
  node_names <- igraph::V(tidygra)$name  # Extract node names from graph
  
  # Determine the node index for calculation
  if (is.null(node_index)) {
    if (!"ego" %in% node_names) {
      stop("No node labeled 'ego' exists in the graph.")
    }
    ego_index <- which(node_names == "ego")
  } else {
    # Validate that the specified node exists
    if (!node_index %in% node_names) {
      stop("The specified node does not exist.")
    }
    ego_index <- which(node_names == node_index)
  }
  
  # Number of nodes in the network
  num_nodes <- dim(adj_matrix)[1]
  
  # Compute total tie strength for ego
  total_tie_strength <- sum(adj_matrix[ego_index, ])  
  
  # Initialize effective size result
  effective_size_result <- 0  
  
  # Loop through nodes excluding the specified ego node
  for (current_node in setdiff(1:num_nodes, ego_index)) {  
    max_tie_strength <- max(adj_matrix[current_node, ], na.rm = TRUE)  # Max tie strength for current node
    if (max_tie_strength == 0) next  # Skip if no ties exist
    
    redundancy_sum <- 0  # Initialize redundancy sum for current node
    for (other_neighbor in setdiff(1:num_nodes, c(ego_index, current_node))) {
      # Proportion of ego's total ties to other_neighbor
      ego_proportion <- adj_matrix[ego_index, other_neighbor] / total_tie_strength
      
      # Proportion of current_node's strongest tie to other_neighbor
      node_redundancy <- adj_matrix[current_node, other_neighbor] / max_tie_strength
      
      redundancy_sum <- redundancy_sum + (ego_proportion * node_redundancy)
    }
    
    # Update effective size result
    effective_size_result <- effective_size_result + (1 - redundancy_sum)
  }
  return(round(effective_size_result,2)) 
}