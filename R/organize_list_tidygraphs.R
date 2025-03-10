#' Convert a persnet data frame into a list of tidygraph objects
#'
#' Converts a persnet data frame into a list of tidygraph objects, with each row 
#' representing an individual network.
#'
#' @param persnet_df A persnet data frame.
#'
#' @return A list of tidygraph objects.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' organize_list_tidygraphs(persnet_df)
organize_list_tidygraphs <- function(persnet_df) {
  ##########
  # Function: Converts a personal network data frame into a list of tidygraph 
  #           objects, with each row representing an individual network.
  # Inputs: persnet_df = A personal network data frame
  # Outputs: A list of tidygraph objects
  ##########
  
  # Split the data frame into a list of individual rows
  df_as_list <- persnet_df %>%
    dplyr::mutate(index = 1:dplyr::n()) %>%  
    dplyr::group_split(index)  # Split by the index
  
  # Convert each row into a tidygraph object using row_to_tidygraph()
  tidygraph_list <- lapply(df_as_list, organize_row_to_tidygraph)
  
  return(tidygraph_list)
}