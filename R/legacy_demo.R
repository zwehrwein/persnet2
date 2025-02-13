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

extract_health_problems_legacy <- function(persnet_row, health_position) {
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


legacy_kin_prop_row <- function(persnet_row) {
  return(prop_alters_relationship(persnet_row,"spouse")+
           prop_alters_relationship(persnet_row,"family"))
}

legacy_sd_age_alters_row <- function(persnet_row) {
  return(
    round(sd(persnet_row %>% select(name1age:name15age) %>% unlist(), na.rm = TRUE),2)
    )
}

legacy_weak_freq_prop_row <- function(persnet_row) {
  #proportion of alters who contact ego monthly or less frequently
  return(
         round(  prop_alters_speak_monthly(persnet_row)+
             prop_alters_speak_more_monthly(persnet_row)
         ,2)
  )
}

legacy_weak_dur_prop_row <- function(persnet_row) {
  #proportion of alters who contact ego monthly or less frequently
  return(
    round(  prop_alters_less_than_3years(persnet_row)+
              prop_alters_less_3_to_6years(persnet_row)
            ,2)
  )
}

legacy_far_dist_prop_row <- function(persnet_row) {
  #proportion of alters who live > 15 miles away
  return(
    round(  prop_alters_16_50miles(persnet_row)+
              prop_alters_more50miles(persnet_row)
            ,2)
  )
}

legacy_miscalc_heavy_drinkers_row <- function(persnet_row) {
  alcohol_cols_string <- grep("^name\\d+alcohol$", colnames(persnet_row), value = TRUE)
  if ( length(alcohol_cols_string) == 0 ) {
    stop("Error: cannot find variables realted to whether alters drink alcohol regularly.")
  } else {
    alcohol_cols <- persnet_row %>% select(name1alcohol:name15alcohol)
    return(
      round(  
      sum(length(which(alcohol_cols == 0)),length(which(alcohol_cols == 1))) /
          sum(persnet_row %>% select(tie1:tie15) != 0, na.rm = TRUE)
          #sum(length(which(alcohol_cols == 0)),length(which(alcohol_cols == 1)))
      ,2)
    )
  }
}

legacy_alter_health_problems_row <- function(persnet_row) {
  alter_health_cols_string <- grep("^name\\d+health___\\d+$", colnames(persnet_row), value = TRUE)
  if ( length(alter_health_cols_string) == 0 ) {
    stop("Error: cannot find variables realted to alter health problems.")
  } else {
    alter_health_problem_cols <- persnet_row %>% select(name1health___1:name15health___99) %>%
      select(!contains("___99")) %>% select(!contains("___0")) 
    return(
      round(  
        sum(alter_health_problem_cols) /
          sum(persnet_row %>% select(tie1:tie15) != 0, na.rm = TRUE)
        ,2)
    )
  }
}

