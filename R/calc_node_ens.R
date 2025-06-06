#' Calculate the Effective Network Size (ENS) for a given node in a network
#'
#' Effective Network Size measures the number of non-redundant ties an ego node has. n.b.
#' This function treats missing weight values as equal to 1 and defines redundant tie in
#' terms of stong ties, not as a proportion of all ties as these networks are weighted.
#
#' ENS is calculated as:
#' \deqn{
#'   ENS_i = \sum_{j \neq i} \left[ 1 - \sum_{q \neq i,j} (p_{iq} \cdot m_{jq}) \right]
#' }
#' where:
#' \itemize{
#'   \item \eqn{i}: Ego node
#'   \item \eqn{j}: Direct neighbors (alters) of ego
#'   \item \eqn{q}: Other nodes in the network (excluding ego \eqn{i} and alter \eqn{j})
#'   \item \eqn{p_{iq}}: Proportion of ego's total tie strength directed to node \eqn{q}:
#'   \deqn{p_{iq} = \frac{w_{iq}}{\sum_{k \neq i} w_{ik}}}
#'   \item \eqn{m_{jq}}: Proportion of alter \eqn{j}'s strongest tie directed to node \eqn{q}:
#'   \deqn{m_{jq} = \frac{w_{jq}}{\max_{k \neq j}(w_{jk})}}
#'   \item \eqn{w_{ij}}: Weight of tie between node \eqn{i} and \eqn{j}
#' }
#'
#' @param tidygra A tidygraph object representing a personal network.
#' @param node_index (Optional) The node for which ENS is calculated (default = "ego").
#'
#' @return The Effective Network Size (ENS), a numeric value, for the specified node.
#'         Returns NA if the network is an isolate.
#' @importFrom magrittr %>%
#' @export
#'
#' @references
#' Burt, R. S. (1995). *Structural Holes: The Social Structure of Competition*.
#' Harvard University Press.
#'
#' @examples
#' \dontrun{
#' ens_value <- calc_node_ens(tidygra, node_index = "burt")
#' }
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