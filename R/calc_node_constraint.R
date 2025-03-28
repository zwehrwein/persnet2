#' Calculate Burt's Constraint for a given node in a network
#'
#' Computes Burt's Constraint measure for a specified node in a tidygraph object.
#' Burt's Constraint quantifies how much a node’s connections are concentrated within
#' a closed group, limiting access to diverse information.
#'
#' @param tidygra A tidygraph object representing a personal network.
#' @param node_index (Optional) The node for which constraint is calculated (default = "ego").
#'
#' @return The constraint value for the specified node.
#'         Returns NA if the network is an isolate.
#' @importFrom magrittr %>%
#' @export
#'
#' @references
#' Everett, M. G., & Borgatti, S. P. (2020). *Unpacking Burt’s constraint measure*.
#' Social Networks, 62, 50-57. ISSN 0378-8733.
#' \doi{10.1016/j.socnet.2020.02.001}
#'
#' @examples
#' calc_node_constraint(tidygra, "ego")
#' calc_node_constraint(tidygra, "burt")

calc_node_constraint <- function(tidygra, node_index = NULL) {
  ##########
  # Function: Computes Burt's Constraint measure for a specified node  
  #           in a tidygraph object, quantifying structural holes.
  # Inputs:  
  #   tidygra    = A tidygraph object representing a personal network
  #   node_index = (Optional) The node for which constraint is calculated (default = "ego")
  # Outputs:  
  #   Burt's Constraint value for the specified node
  ##########
  
  # Test if the input is a valid tidygraph object
  if (is.null(tidygra) || !"tbl_graph" %in% class(tidygra)) {
    stop("The input graph must be a tidygraph graph object.")
  }
  
  # Check if the graph is an isolate (no edges)
  if (dim(igraph::as_adjacency_matrix(tidygra))[1] < 2) return(NA)
  
  # Check if edge attribute weight is missing
  edge_attrs <- tidygra %>% tidygraph::activate(edges) %>% tibble::as_tibble()
  if (!"weight" %in% colnames(edge_attrs)) {
    warning("Warning: Network has no weights. Calculating Burt's constraint with all tie weights set to 1.")
    tidygra <- tidygra %>%
      tidygraph::activate(edges) %>%
       dplyr::mutate(weight = 1)  # Add default weight of 1
  }
  
  # Convert to adjacency matrix
  adj_matrix <- igraph::as_adjacency_matrix(tidygra, attr = "weight", sparse = FALSE)
  node_names <- igraph::V(tidygra)$name  # Extract node names from the graph
  
  # Identify the node of interest (ego)
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

  # Number of nodes in the graph
  num_nodes <- nrow(adj_matrix)
  
  # Initialize total tie strength for each node
  sum_tie_strengths <- rowSums(adj_matrix + t(adj_matrix))  # Sum of ties for each node
  
  # Compute proportional tie strengths 
  matrix_proportions <- matrix(0, nrow = num_nodes, ncol = num_nodes)  # Initialize blank network
  for (i in 1:num_nodes) {
    if (sum_tie_strengths[i] == 0) next  # Skip nodes with no ties
    for (j in setdiff(1:num_nodes, i)) {
      matrix_proportions[i, j] <- (adj_matrix[i, j] + adj_matrix[j, i]) / sum_tie_strengths[i]
    }
  }
  
  # Calculate Burt's constraint for the selected node
  constraint <- 0
  for (j in setdiff(1:num_nodes, ego_index)) { # Ego index could be other nodes
    redundancy <- 0
    for (q in setdiff(1:num_nodes, c(ego_index, j))) {
      redundancy <- redundancy + matrix_proportions[ego_index, q] * matrix_proportions[q, j]
    }
    constraint <- constraint + (matrix_proportions[ego_index, j] + redundancy)^2
  }
  
  return(round(constraint,2))
}