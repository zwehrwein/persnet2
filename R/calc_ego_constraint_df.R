#' Calculate Ego Constraint from a persnet dataframe
#'
#' Computes the ego network constraint for a persnet dataframe.
#'
#' @param persnet_df A dataframe containing personal network data.
#'
#' @return A named vector containing the ego's network constraint for each network.
#' @export
#'

calc_ego_constraint_df <- function(persnet_df) {
  tryCatch({
    # Organize a list of tidygraphs from the persnet dataframe
    gra_list <- organize_list_tidygraphs(persnet_df)
    # Calculate the ego's network constraint for each graph in the list
    vector_ego_constraint <- sapply(gra_list, calc_node_constraint)
    return(vector_ego_constraint)  
  }, error = function(e) { 
    # If an error occurs, stop with a message indicating that the input is not a valid persnet dataframe
    stop("Error: not a valid persnet dataframe")  
  })
}