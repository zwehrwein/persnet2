#' Count the total number of unique alters in a personal network row
#'
#' Computes the total number of unique alters in a given personal network row, combining named alters 
#' from the tidygraph structure and additional names listed in "more_names" columns.
#'
#' @param persnet_row A single row of a personal network data frame.
#'
#' @return The total count of unique alters in the network.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' count_total_alters_row(persnet_row)
calc_total_alters_row <- function(persnet_row) {
  ##########
  # Function: Computes the total number of unique alters in a given  
  #           personal network row, combining named alters and additional  
  #           names listed in "more_names" columns.
  # Inputs:  
  #   persnet_row = A single row of a personal network data frame
  # Outputs:  
  #   Total count of unique alters in the network
  ##########
  
  # Validate input
  if (is.null(persnet_row) || !"data.frame" %in% class(persnet_row)) {
    stop("The input must be a single row of a personal network dataframe.")
  }
  
  # Convert the row into a tidygraph object
  tc_tidygraph <- organize_row_to_tidygraph(persnet_row)
  
  # Extract unique alter names from the tidygraph, excluding the ego node
  unique_names_tidygra <- tc_tidygraph %>%
    tidygraph::activate(nodes) %>%       
    dplyr::as_tibble() %>%           
    dplyr::filter(name != "ego") %>% 
    dplyr::pull(name) %>%           
    unique()
  
  # Extract and clean additional alter names from "more_names" columns
  all_more_names <- paste(persnet_row$more_names_1, 
                          persnet_row$more_names_2, 
                          persnet_row$more_names_3, 
                          sep = ", ") %>%
    stringr::str_split(",\\s*") %>%  # split by commas
    unlist() %>%            # conver to vector
    .[. != ""] %>%          # semove empty strings     
    unique() 
  
  # Return the count of unique alters from both sources
  return(length(union(unique_names_tidygra, all_more_names)))
}