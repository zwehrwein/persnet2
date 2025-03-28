#' Reproduces the output of 'clean.csv'
#'
#' Computes all columns of interest, with a catch-except so everything does not fail at once.
#'
#' @param persnet_df A dataframe containing personal network data.
#'
#' @return a clean csv
#' @importFrom magrittr %>%
#' @export
#'
build_clean_df = function(df_input) {

#create factor variables here with a try except if the particular variable name is missing

# Check and convert 'edu'
if (!"edu" %in% names(df_input) || length(df_input$edu) == 0) {
  warning("Column 'edu' is missing or empty. Creating default NA vector.")
  df_input$edu <- rep(NA, nrow(df_input))
}
df_input$edu <- tryCatch({
  factor(df_input$edu, 
         levels = c(1, 2, 3, 4, 5, 6, 88),
         labels = c("some_high_school", "high_school_grad",
                    "some_college", "associate_degree", 
                    "bachelor_degree", "graduate_degree", 
                    "no_answer"))
}, error = function(e) {
  warning("Error converting edu: ", conditionMessage(e))
  factor(rep(NA, nrow(df_input)))
})

# Check and convert 'employment'
if (!"employment" %in% names(df_input) || length(df_input$employment) == 0) {
  warning("Column 'employment' is missing or empty. Creating default NA vector.")
  df_input$employment <- rep(NA, nrow(df_input))
}
df_input$employment <- tryCatch({
  factor(df_input$employment, 
         levels = c(1, 2, 3, 4, 5, 6, 7, 0),
         labels = c("employed_for_wages", "self_employed",
                    "out_of_work_looking_for_work", "student", 
                    "retired", "unable_to_work", 
                    "no_answer", "out_of_work_not_looking_for_work"))
}, error = function(e) {
  warning("Error converting employment: ", conditionMessage(e))
  factor(rep(NA, nrow(df_input)))
})

# Check and convert 'occupation'
if (!"occupation" %in% names(df_input) || length(df_input$occupation) == 0) {
  warning("Column 'occupation' is missing or empty. Creating default NA vector.")
  df_input$occupation <- rep(NA, nrow(df_input))
}
df_input$occupation <- tryCatch({
  factor(df_input$occupation, 
         levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "77"),
         labels = c("executive_manager", "sales_or_clerical_worker",
                    "mechanic_electrician_skilled_worker", 
                    "machine_operator_inspector_bus_cab_driver",
                    "service_worker", "professional", 
                    "business_owner", "laborer_unskilled_worker",
                    "farming", "military", "other"))
}, error = function(e) {
  warning("Error converting occupation: ", conditionMessage(e))
  factor(rep(NA, nrow(df_input)))
})

# Check and convert 'income'
if (!"income" %in% names(df_input) || length(df_input$income) == 0) {
  warning("Column 'income' is missing or empty. Creating default NA vector.")
  df_input$income <- rep(NA, nrow(df_input))
}
df_input$income <- tryCatch({
  factor(df_input$income, 
         levels = c("1", "2", "3", "4", "5"),
         labels = c("less_than_5000", "5000_to_49000", "50000_to_169000",
                    "170000_to_499000", "more_than_500000"))
}, error = function(e) {
  warning("Error converting income: ", conditionMessage(e))
  factor(rep(NA, nrow(df_input)))
})

# Check and convert 'live_alone'
if (!"live_alone" %in% names(df_input) || length(df_input$live_alone) == 0) {
  warning("Column 'live_alone' is missing or empty. Creating default NA vector.")
  df_input$live_alone <- rep(NA, nrow(df_input))
}
df_input$live_alone <- tryCatch({
  factor(df_input$live_alone, 
         levels = c(0, 1),
         labels = c("no", "yes"))
}, error = function(e) {
  warning("Error converting live_alone: ", conditionMessage(e))
  factor(rep(NA, nrow(df_input)))
})

# Check and convert 'married'
if (!"married" %in% names(df_input) || length(df_input$married) == 0) {
  warning("Column 'married' is missing or empty. Creating default NA vector.")
  df_input$married <- rep(NA, nrow(df_input))
}
df_input$married <- tryCatch({
  factor(df_input$married, 
         levels = c(0, 1),
         labels = c("not_married", "married"))
}, error = function(e) {
  warning("Error converting married: ", conditionMessage(e))
  factor(rep(NA, nrow(df_input)))
})

# Check and convert 'alcohol'
if (!"alcohol" %in% names(df_input) || length(df_input$alcohol) == 0) {
  warning("Column 'alcohol' is missing or empty. Creating default NA vector.")
  df_input$alcohol <- rep(NA, nrow(df_input))
}
df_input$alcohol <- tryCatch({
  factor(df_input$alcohol, 
         levels = c(1, 0, 9),
         labels = c("yes", "no", "does_not_drink_heavily"))
}, error = function(e) {
  warning("Error converting alcohol: ", conditionMessage(e))
  factor(rep(NA, nrow(df_input)))
})

# Check and convert 'smoke'
if (!"smoke" %in% names(df_input) || length(df_input$smoke) == 0) {
  warning("Column 'smoke' is missing or empty. Creating default NA vector.")
  df_input$smoke <- rep(NA, nrow(df_input))
}
df_input$smoke <- tryCatch({
  factor(df_input$smoke, 
         levels = c(1, 0, 9),
         labels = c("yes", "no", "does_not_smoke"))
}, error = function(e) {
  warning("Error converting smoke: ", conditionMessage(e))
  factor(rep(NA, nrow(df_input)))
})

# Check and convert 'exercise'
if (!"exercise" %in% names(df_input) || length(df_input$exercise) == 0) {
  warning("Column 'exercise' is missing or empty. Creating default NA vector.")
  df_input$exercise <- rep(NA, nrow(df_input))
}
df_input$exercise <- tryCatch({
  factor(df_input$exercise, 
         levels = c(1, 0),
         labels = c("yes", "no"))
}, error = function(e) {
  warning("Error converting exercise: ", conditionMessage(e))
  factor(rep(NA, nrow(df_input)))
})

# Check and convert 'diet'
if (!"diet" %in% names(df_input) || length(df_input$diet) == 0) {
  warning("Column 'diet' is missing or empty. Creating default NA vector.")
  df_input$diet <- rep(NA, nrow(df_input))
}
df_input$diet <- tryCatch({
  factor(df_input$diet, 
         levels = c(1, 0),
         labels = c("yes", "no"))
}, error = function(e) {
  warning("Error converting diet: ", conditionMessage(e))
  factor(rep(NA, nrow(df_input)))
})

#build output df here
df_clean = tibble::tibble(
  record_id = if ("record_id" %in% names(df_input)) df_input$record_id else rep(NA, nrow(df_input)),
  age       = if ("age" %in% names(df_input)) df_input$age else rep(NA, nrow(df_input)),
  
  sex = if ("sex" %in% names(df_input)) {
    factor(df_input$sex, levels = c(0, 1, 2), labels = c("woman", "men", "other"))
  } else {
    rep(NA, nrow(df_input))
  },
  
  race1 = purrr::map_chr(1:nrow(df_input), ~ tryCatch({
    extract_race_legacy(df_input[.x, , drop = FALSE], race_numeric = 1)
  }, error = function(e) {
    warning(sprintf("Row %s: Unable to compute race1", .x))
    NA_character_
  })),
  
  race2 = purrr::map_chr(1:nrow(df_input), ~ tryCatch({
    extract_race_legacy(df_input[.x, , drop = FALSE], race_numeric = 2)
  }, error = function(e) {
    warning(sprintf("Row %s: Unable to compute race2", .x))
    NA_character_
  })),
  
  education = if ("edu" %in% names(df_input)) df_input$edu else rep(NA, nrow(df_input)),
  
  zip = if ("zip" %in% names(df_input)) as.character(df_input$zip) else rep(NA, nrow(df_input)),
  
  employment = if ("employment" %in% names(df_input)) df_input$employment else rep(NA, nrow(df_input)),
  
  occupation = if ("occupation" %in% names(df_input)) df_input$occupation else rep(NA, nrow(df_input)),
  
  income = if ("income" %in% names(df_input)) df_input$income else rep(NA, nrow(df_input)),
  
  married = if ("married" %in% names(df_input)) df_input$married else rep(NA, nrow(df_input)),
  
  live_alone = if ("live_alone" %in% names(df_input)) df_input$live_alone else rep(NA, nrow(df_input)),
  
  household_number = if ("household_number" %in% names(df_input)) df_input$household_number else rep(NA, nrow(df_input)),
  
  ego_alcohol = if ("alcohol" %in% names(df_input)) df_input$alcohol else rep(NA, nrow(df_input)),
  
  ego_smoke = if ("smoke" %in% names(df_input)) df_input$smoke else rep(NA, nrow(df_input)),
  
  ego_exercise = if ("exercise" %in% names(df_input)) df_input$exercise else rep(NA, nrow(df_input)),
  
  ego_healty_diet = if ("diet" %in% names(df_input)) df_input$diet else rep(NA, nrow(df_input)),

# health problems
health_problems1 = purrr::map_chr(1:nrow(df_input), ~ tryCatch({
extract_health_problems(df_input[.x, , drop = FALSE], health_position = 1)
}, error = function(e) {
warning(sprintf("Row %s: Unable to find first listed health problem", .x))
NA_character_
})),
health_problems2 = purrr::map_chr(1:nrow(df_input), ~ tryCatch({
extract_health_problems(df_input[.x, , drop = FALSE], health_position = 2)
}, error = function(e) {
warning(sprintf("Row %s: Unable to find second listed health problem", .x))
NA_character_
})),
health_problems3 = purrr::map_chr(1:nrow(df_input), ~ tryCatch({
extract_health_problems(df_input[.x, , drop = FALSE], health_position = 3)
}, error = function(e) {
warning(sprintf("Row %s: Unable to find third listed health problem", .x))
NA_character_
})),
health_problems4 = purrr::map_chr(1:nrow(df_input), ~ tryCatch({
extract_health_problems(df_input[.x, , drop = FALSE], health_position = 4)
}, error = function(e) {
warning(sprintf("Row %s: Unable to find fouth listed health problem", .x))
NA_character_
})),

# network structure
network_size = tryCatch({
calc_total_alters_df(df_input)
}, error = function(e) {
warning("Unable to compute alter_count")
rep(NA_real_, nrow(df_input))
}),
density = tryCatch({
calc_egoless_density_df(df_input)
}, error = function(e) {
warning("Unable to compute egoless_density")
rep(NA_real_, nrow(df_input))
}),
constraint = tryCatch({
calc_ego_constraint_df(df_input)
}, error = function(e) {
warning("Unable to compute ego_constraint")
rep(NA_real_, nrow(df_input))
}),
effsize = tryCatch({
calc_ego_ens_df(df_input)
}, error = function(e) {
warning("Unable to compute ego_ens")
rep(NA_real_, nrow(df_input))
}),
max_degree = tryCatch({
calc_egoless_max_degree_df(df_input)
}, error = function(e) {
warning("Unable to compute egoless_max_degree")
rep(NA_real_, nrow(df_input))
}),
mean_degree = tryCatch({
calc_egoless_mean_degree_df(df_input)
}, error = function(e) {
warning("Unable to compute egoless_min_degree")
rep(NA_real_, nrow(df_input))
}),

# proportional calculations
kin_prop = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
prop_kin_persnet_row(df_input[.x, , drop = FALSE])
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute kin_prop", .x))
NA_real_
})),
age_sd = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
sd_age_alters_row(df_input[.x, , drop = FALSE])
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute sd_age_alters", .x))
NA_real_
})),
iqv_sex = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
calc_attribute_iqv(df_input[.x, , drop = FALSE], attribute = "gender")
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute iqv_gender", .x))
NA_real_
})),
iqv_race = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
calc_attribute_iqv(df_input[.x, , drop = FALSE], attribute = "race")
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute iqv_race", .x))
NA_real_
})),
iqv_educ = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
calc_attribute_iqv(df_input[.x, , drop = FALSE], attribute = "educ")
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute iqv_educ", .x))
NA_real_
})),
weak_freq_prop = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
weak_freq_prop_row(df_input[.x, , drop = FALSE])
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute weak_freq_prop", .x))
NA_real_
})),
weak_dur_prop = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
weak_dur_prop_row(df_input[.x, , drop = FALSE])
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute weak_dur_prop", .x))
NA_real_
})),
far_dist_prop = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
far_dist_prop_row(df_input[.x, , drop = FALSE])
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute far_dist_prop", .x))
NA_real_
})),
heavy_drinkers_prop = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
prop_heavy_drinkers_row(df_input[.x, , drop = FALSE])
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute heavy_drinkers_prop", .x))
NA_real_
})),
smoking_prop = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
calc_prop_alters_smoke(df_input[.x, , drop = FALSE])
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute alters_smoke_prop", .x))
NA_real_
})),
no_exercise_prop = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
calc_prop_alters_exercise(df_input[.x, , drop = FALSE])
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute alters_exercise_prop", .x))
NA_real_
})),
bad_diet_prop = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
calc_prop_alters_diet(df_input[.x, , drop = FALSE])
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute alters_diet_prop", .x))
NA_real_
})),
health_prob_prop = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
alter_health_problems_row(df_input[.x, , drop = FALSE])
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute alter_health_problems", .x))
NA_real_
})),
blau_gender = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
calc_blau_alter_heterophily(df_input[.x, , drop = FALSE], attribute = "gender")
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute blau_gender", .x))
NA_real_
})),
blau_educ = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
calc_blau_alter_heterophily(df_input[.x, , drop = FALSE], attribute = "educ")
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute blau_educ", .x))
NA_real_
})),
blau_distance = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
calc_blau_alter_heterophily(df_input[.x, , drop = FALSE], attribute = "distance")
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute blau_distance", .x))
NA_real_
})),
blau_length = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
calc_blau_alter_heterophily(df_input[.x, , drop = FALSE], attribute = "length")
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute blau_length", .x))
NA_real_
})),
blau_speak = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
calc_blau_alter_heterophily(df_input[.x, , drop = FALSE], attribute = "speak")
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute blau_speak", .x))
NA_real_
}))
)

return(df_clean)
}
