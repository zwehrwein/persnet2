#' Calculate Burt's Constraint for a given node in a weighted undirected egonetwork
#'
#' Computes Burt's Constraint measure for a specified node in a tidygraph object, by default,
#' the node labeled 'ego'. Constraint quantifies how much a node’s connections are concentrated within
#' a closed group, limiting access to diverse information. 
#'
#' For undirected networks, constraint \eqn{C_i} for a given ego \eqn{i}: is calculated as:
#'
#' \deqn{
#' C_i = \sum_{j \neq i} \left(p_{ij} + \sum_{q \neq i,j} p_{iq} p_{qj}\right)^2
#' }
#'
#' Where:
#' \itemize{
#'   \item \eqn{p_{ij}} is the proportion of ego \eqn{i}'s total tie strength 
#'   that connects to alter \eqn{j}. Formally, for an undirected network:
#'   \deqn{
#'   p_{ij} = \frac{w_{ij}}{\sum_{k \neq i} w_{ik}}
#'   }
#'   
#'   \item \eqn{w_{ij}} represents the weight of the undirected edge between nodes \eqn{i} and \eqn{j}.
#'   
#'   \item The term \eqn{\sum_{q \neq i,j} p_{iq} p_{qj}} captures indirect redundancy
#'   between ego \eqn{i} and alter \eqn{j}, reflecting the proportion of tie strength mediated by other nodes \eqn{q}.
#'   
#'   \item The expression inside the parentheses is squared to strongly penalize redundant paths.
#' }
#'
#' @details
#' Clarifying the notation for undirected networks:
#' \itemize{
#'   \item \eqn{i}: focal node ("ego").
#'   \item \eqn{j}: any node directly connected to \eqn{i}.
#'   \item \eqn{q}: intermediary node connecting \eqn{i} and \eqn{j}.
#'   \item \eqn{C_i}: constraint score for node \eqn{i}.
#' }
#'
#' @param tidygra A tidygraph object representing a personal network.
#' @param node_index (Optional) The node for which constraint is calculated (default = "ego").
#'
#' @return The constraint value for the specified node. In undirected networks with ties weighted
#' either 1 (weak) or 2 (strong), Burt’s constraint C_i is bounded between 0 and 1.25. Returns NA if the network is an isolate.
#' @importFrom magrittr %>%
#' @export
#'
#' @references
#' Everett, M. G., & Borgatti, S. P. (2020). *Unpacking Burt’s constraint measure*.
#' Social Networks, 62, 50-57. ISSN 0378-8733.
#' \doi{10.1016/j.socnet.2020.02.001}
#'
#' @examples
#' calc_node_constraint(tidygra)
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
    warning("Network has no weights; assigning all tie weights as 1.")
    tidygra <- tidygra %>%
      tidygraph::activate(edges) %>%
      dplyr::mutate(weight = 1)
  }
  
  # Convert to adjacency matrix (undirected)
  adj_matrix <- igraph::as_adjacency_matrix(tidygra, attr = "weight", sparse = FALSE)
  node_names <- igraph::V(tidygra)$name
  
  # Identify ego node index
  if (is.null(node_index)) {
    if (!"ego" %in% node_names) {
      stop("No node labeled 'ego' exists in the graph.")
    }
    ego_index <- which(node_names == "ego")
  } else {
    if (!node_index %in% node_names) {
      stop("The specified node does not exist.")
    }
    ego_index <- which(node_names == node_index)
  }

  num_nodes <- nrow(adj_matrix)

  # Total tie strength for each node (undirected)
  sum_tie_strengths <- rowSums(adj_matrix)

  # Compute proportional tie strengths for undirected networks
  matrix_proportions <- matrix(0, nrow = num_nodes, ncol = num_nodes)
  for (i in 1:num_nodes) {
    if (sum_tie_strengths[i] == 0) next
    for (j in setdiff(1:num_nodes, i)) {
      matrix_proportions[i, j] <- adj_matrix[i, j] / sum_tie_strengths[i]
    }
  }

  # Calculate Burt's constraint for ego node
  constraint <- 0
  for (j in setdiff(1:num_nodes, ego_index)) {
    redundancy <- sum(matrix_proportions[ego_index, -c(ego_index, j)] * matrix_proportions[-c(ego_index, j), j])
    constraint <- constraint + (matrix_proportions[ego_index, j] + redundancy)^2
  }

  return(round(constraint, 2))
}