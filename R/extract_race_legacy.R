#' Return the race of the ego from the demographics questions
#'
#' Returns the race of the ego from the demographics questions
#'
#' @param persnet_df A dataframe containing personal network data.
#'
#' @return A string indicating the primary racial identification of the ego.
#' @export
#'

extract_race_legacy <- function(persnet_row, race_position) {
  race_labels <- c("black", "white", "indigenous", "asian", "pacific_islander", "other", NA)
  #these specific race columns can be edited depending on names in persnet edition
  race_columns <- c("health___1", "race___2", "race___3", "race___4", "race___5", "race___77", "race___88")
  # identify which race columns are marked as 1
  selected_races <- race_labels[which(persnet_row[race_columns] == 1)]
  #select which item in list of race columns == 1
  if (length(selected_races) >= race_position) {
    return(selected_races[race_position])
  } else {
    return(NA)
  }
}