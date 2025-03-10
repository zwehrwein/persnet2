#' Convert one row of a personal network (persnet) data frame to a tidygraph object
#'
#' Converts a single row of a personal network data frame into a tidygraph object, 
#' representing the network structure of ego and their alters.
#'
#' - Constructs an adjacency matrix from tie variables.
#' - Uses alter names as node labels.
#' - Removes missing or NA alters from the network.
#' - Ensures undirected network representation.
#'
#' @param df_row_input A single row of a personal network data frame.
#'
#' @return A tidygraph object representing the personal network.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' organize_row_to_tidygraph(persnet_row)
organize_row_to_tidygraph <- function(df_row_input) {
  ##########
  # Function: Converts a single row of a personal network data frame into  
  #           a tidygraph object, representing network ties
  # Inputs: df_row_input = A single row of a personal network data frame
  # Outputs: A tidygraph object representing the network structure
  ##########

  # Extract personal network (psn) tie values from the input row
  psn_edges_variables <- dplyr::select(df_row_input, tie1:a_tie105)
  psn_edges_values <- as.integer(psn_edges_variables)  # convert to integer
  
  # Initialize an adjacency matrix for the ego network
  psn_mat <- matrix(NA, 16, 16)  # create a 16x16 matrix (max 15 alters + ego)
  diag(psn_mat) <- 0  # Set diagonal to 0 (no self-loops)
  
  # Populate the lower triangular part of the adjacency matrix with tie values
  psn_mat[lower.tri(psn_mat)] <- psn_edges_values
  
  # Make the matrix symmetric by copying the lower triangle to the upper triangle
  psn_mat <- t(psn_mat)
  psn_mat[lower.tri(psn_mat)] <- psn_edges_values
  
  # Extract column names for alters from the input row
  psn_name_cols <- dplyr::select(df_row_input, c(
    name1, name2, name3, name4, name5,
    name6, name7, name8, name9, name10,
    name11, name12, name13, name14, name15
  ))
  
  # Determine which alters to keep based on a "keep" column (binary indicator)
  psn_keep_name_cols <- dplyr::select(df_row_input, name_1:name_15)
  psn_names_to_keep <- c("ego", ifelse(psn_keep_name_cols == 1, psn_name_cols, NA))
  
  # Flatten the names to a vector, ignoring names marked as NA
  psn_names_for_mat <- as.vector(unlist(psn_names_to_keep, use.names = FALSE))
  colnames(psn_mat) <- rownames(psn_mat) <- psn_names_for_mat  # Set matrix row/column names
  
  # Remove rows and columns corresponding to NAs in the matrix
  psn_mat <- psn_mat[!is.na(rownames(psn_mat)), !is.na(colnames(psn_mat))]
  
  # Identify edges (non-zero entries in the matrix)
  psn_edges <- which(psn_mat != 0, arr.ind = TRUE)
  
  # Check if there are edges (non-isolate)
  if (!is.null(psn_edges) && is.matrix(psn_edges) && nrow(psn_edges) > 0) {
    psn_el <- data.frame(
      from = rownames(psn_mat)[psn_edges[, 1]],
      to = colnames(psn_mat)[psn_edges[, 2]],
      weight = psn_mat[psn_edges]
    )
    
    # Ensure the edge list is undirected (from < to condition)
    psn_el <- psn_el[psn_el$from < psn_el$to, ]
    
    # convert to tidygraph object
    tgra <- tidygraph::as_tbl_graph(psn_el, directed = FALSE) %>%
      dplyr::mutate(record_id = df_row_input$record_id,  # Add record ID
             node_id = dplyr::row_number())  # Create a unique node ID
    
    return(tgra)
  } else {
    # if no edges exist, create an isolate 
    tgra <- tidygraph::tbl_graph(
      nodes = tibble::tibble(name = c("ego"))) %>%
      tidygraph::activate(nodes) %>%
      dplyr::mutate(record_id = df_row_input$record_id,
             node_id = dplyr::row_number())
    
    return(tgra)
  }
}