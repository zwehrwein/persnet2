#' Return the columns regarding health problems of the ego
#'
#' @param persnet_df A dataframe containing personal network data.
#'
#' @return A string indicating the primary racial identification of the ego.
#' @export
#'

extract_health_problems <- function(persnet_row, health_position) {
  health_labels <- c("general","pain","cognitive","cardiac","none")
  
  health_columns <- c("health___1", "health___2", "health___3",
                    "health___4", "health___0")
  
  selected_health <- health_labels[which(persnet_row[health_columns] == 1)]
  # Return the requested race (1st or 2nd), or NA if not available
  if (length(selected_health) >= health_position) {
    return(selected_health[health_position])
  } else {
    return(NA)
  }
}