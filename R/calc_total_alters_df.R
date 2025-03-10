#' Calculate Total Alters from a persnet dataframe
#'
#' Computes the total number of unique alters for each record in a persnet dataframe. This counts the number of unique strings in the `more names' columns and unique alters in the persnet tidgraph.
#'
#' @param persnet_df A dataframe containing personal network data.
#'
#' @return A vector containing the total count of unique alters for each record.
#' @importFrom magrittr %>%
#' @export
#'

calc_total_alters_df <- function(persnet_df) {
  tryCatch({
    #apply to each row the function of calculate total alters 
    vector_count_total_alters <- apply(persnet_df, 1, function(row) 
      calc_total_alters_row(persnet_df %>% dplyr::filter(record_id == row["record_id"])))
    return(vector_count_total_alters)  
  }, error = function(e) { 
    # If an error occurs, stop with a message indicating that the input is not a valid persnet dataframe
    warning(" error: there was an error calculating number of unqiue alters, returning NA")  
    return(NA)
  })
}