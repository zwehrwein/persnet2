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
df_clean = tibble::tibble(
record_id = df_input$record_id,
age = df_input$age,

sex = factor(df_input$sex, levels = c(0, 1, 2), labels = c("woman", "men", "other")),

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

# Education: convert and assign
df_input$edu <- factor(df_input$edu, 
  levels = c(1, 2, 3, 4, 5, 6, 88),
  labels = c("some_high_school", "high_school_grad",
          "some_college", "associate_degree", 
          "bachelor_degree", "graduate_degree", 
          "no_answer")),
education <- df_input$edu,

# Zip: convert to character
zip <- as.character(df_input$zip),

# Employment: convert and assign
df_input$employment <- factor(df_input$employment, 
  levels = c(1, 2, 3, 4, 5, 6, 7, 0),
  labels = c("employed_for_wages", "self_employed",
           "out_of_work_looking_for_work", "student", 
           "retired", "unable_to_work", 
           "no_answer", "out_of_work_not_looking_for_work")),
employment <- df_input$employment,

# Occupation: convert and assign
df_input$occupation <- factor(df_input$occupation, 
  levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "77"),
  labels = c("executive_manager", "sales_or_clerical_worker",
             "mechanic_electrician_skilled_worker", 
             "machine_operator_inspector_bus_cab_driver",
             "service_worker", "professional", 
             "business_owner", "laborer_unskilled_worker",
             "farming", "military", "other")),
occupation <- df_input$occupation,

# Income: convert and assign
df_input$income <- factor(df_input$income, 
  levels = c("1", "2", "3", "4", "5"),
  labels = c("less_than_5000", "5000_to_49000", "50000_to_169000",
             "170000_to_499000", "more_than_500000")),
income <- df_input$income,

# Married: convert and assign
df_input$married <- factor(df_input$married, 
  levels = c(0, 1),
  labels = c("not_married", "married")),
married <- df_input$married,

# Live Alone: convert and assign
df_input$live_alone <- factor(df_input$live_alone, 
  levels = c(0, 1),
  labels = c("no", "yes")),
live_alone <- df_input$live_alone,

# Household Number: simply assign
household_number <- df_input$household_number

# Health behaviors:
# Alcohol: convert and assign
df_input$alcohol <- factor(df_input$alcohol, 
  levels = c(1, 0, 9),
  labels = c("yes", "no", "does_not_drink_heavily")),
ego_alcohol <- df_input$alcohol,

# Smoke: convert and assign
df_input$smoke <- factor(df_input$smoke, 
  levels = c(1, 0, 9),
  labels = c("yes", "no", "does_not_smoke")),
ego_smoke <- df_input$smoke,

# Exercise: convert and assign
df_input$exercise <- factor(df_input$exercise, 
  levels = c(1, 0),
  labels = c("yes", "no")),
ego_exercise <- df_input$exercise,

# Diet: convert and assign
df_input$diet <- factor(df_input$diet, 
  levels = c(1, 0),
  labels = c("yes", "no")),
ego_diet <- df_input$diet,

# health problems
health1 = purrr::map_chr(1:nrow(df_input), ~ tryCatch({
extract_health_problems(df_input[.x, , drop = FALSE], health_position = 1)
}, error = function(e) {
warning(sprintf("Row %s: Unable to find first listed health problem", .x))
NA_character_
})),
health2 = purrr::map_chr(1:nrow(df_input), ~ tryCatch({
extract_health_problems(df_input[.x, , drop = FALSE], health_position = 2)
}, error = function(e) {
warning(sprintf("Row %s: Unable to find second listed health problem", .x))
NA_character_
})),
health3 = purrr::map_chr(1:nrow(df_input), ~ tryCatch({
extract_health_problems(df_input[.x, , drop = FALSE], health_position = 3)
}, error = function(e) {
warning(sprintf("Row %s: Unable to find third listed health problem", .x))
NA_character_
})),
health4 = purrr::map_chr(1:nrow(df_input), ~ tryCatch({
extract_health_problems(df_input[.x, , drop = FALSE], health_position = 4)
}, error = function(e) {
warning(sprintf("Row %s: Unable to find fouth listed health problem", .x))
NA_character_
})),

# network structure
alter_count = tryCatch({
calc_total_alters_df(df_input)
}, error = function(e) {
warning("Unable to compute alter_count")
rep(NA_real_, nrow(df_input))
}),
egoless_density = tryCatch({
calc_egoless_density_df(df_input)
}, error = function(e) {
warning("Unable to compute egoless_density")
rep(NA_real_, nrow(df_input))
}),
ego_constraint = tryCatch({
calc_ego_constraint_df(df_input)
}, error = function(e) {
warning("Unable to compute ego_constraint")
rep(NA_real_, nrow(df_input))
}),
ego_ens = tryCatch({
calc_ego_ens_df(df_input)
}, error = function(e) {
warning("Unable to compute ego_ens")
rep(NA_real_, nrow(df_input))
}),
egoless_max_degree = tryCatch({
calc_egoless_max_degree_df(df_input)
}, error = function(e) {
warning("Unable to compute egoless_max_degree")
rep(NA_real_, nrow(df_input))
}),
egoless_mean_degree = tryCatch({
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
sd_age_alters = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
sd_age_alters_row(df_input[.x, , drop = FALSE])
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute sd_age_alters", .x))
NA_real_
})),
iqv_gender = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
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
alters_smoke_prop = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
calc_prop_alters_smoke(df_input[.x, , drop = FALSE])
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute alters_smoke_prop", .x))
NA_real_
})),
alters_exercise_prop = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
calc_prop_alters_exercise(df_input[.x, , drop = FALSE])
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute alters_exercise_prop", .x))
NA_real_
})),
alters_diet_prop = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
calc_prop_alters_diet(df_input[.x, , drop = FALSE])
}, error = function(e) {
warning(sprintf("Row %s: Unable to compute alters_diet_prop", .x))
NA_real_
})),
alter_health_problems = purrr::map_dbl(1:nrow(df_input), ~ tryCatch({
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
