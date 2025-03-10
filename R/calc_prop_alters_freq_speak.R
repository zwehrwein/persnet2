#' Calculate the proportion of alters who speak to ego at a specified frequency
#'
#' Computes the proportion of alters who speak to the ego at a specified frequency.
#'
#' @param persnet_row A single row of a personal network data frame.
#' @param frequency One of 'daily', 'weekly', 'monthly', 'more_monthly', or 'dont_know'.
#'
#' @return The proportion of alters who speak to the ego at the specified frequency.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' calc_prop_alters_freq_speak(persnet_row, "daily")
#' calc_prop_alters_freq_speak(persnet_row, "weekly")
#' calc_prop_alters_freq_speak(persnet_row, "monthly")
#' calc_prop_alters_freq_speak(persnet_row, "more_monthly")
#' calc_prop_alters_freq_speak(persnet_row, "dont_know")
calc_prop_alters_freq_speak <- function(persnet_row, frequency) {
  ##########
  # Function: Computes the proportion of alters who speak to the ego  
  #           at a specified frequency.
  # Inputs:  
  #   persnet_row = A single row of a personal network data frame
  #   frequency   = One of 'daily', 'weekly', 'monthly', 'more_monthly', or 'dont_know'
  # Outputs:  
  #   Proportion of alters who speak to the ego at the specified frequency
  ##########
  
  # Map frequency labels to numeric codes
  frequency_map <- c(
    daily = 1,
    weekly = 2,
    monthly = 3,
    more_monthly = 4,
    dont_know = 99
  )
  
  # Validate the frequency input
  if (!(frequency %in% names(frequency_map))) {
    stop("Error: Choose one of 'daily', 'weekly', 'monthly', 'more_monthly', or 'dont_know'.")
  }
  
  # Identify speaking frequency-related columns
  speak_cols_string <- grep("^name\\d+speak$", colnames(persnet_row), value = TRUE)
  
  # Handle missing columns
  if (length(speak_cols_string) == 0) {
    stop("Error: Cannot find columns related to how frequently alters speak to the patient.")
  }
  
  # Select speaking frequency columns
  speak_cols <- persnet_row %>% dplyr::select(name1speak:name15speak)
  
  # Calculate and return the proportion of alters with the specified frequency
  return(
    length(which(speak_cols == frequency_map[[frequency]])) /
    sum(persnet_row %>% dplyr::select(tie1:tie15) != 0, na.rm = TRUE)
  )
}