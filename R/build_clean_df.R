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
build_clean_df <- function(df_input) {
  df_clean <- tibble::tibble(
    record_id = df_input$record_id,
    age = df_input$age,
    sex = df_input$sex,  # n.b. male == 1

    # demographics
    race1 = purrr::map_chr(1:nrow(df_input), ~ tryCatch({
      extract_race_legacy(df_input[.x, , drop = FALSE], race_position = 1)
    }, error = function(e) {
      warning(sprintf("Row %s: Unable to compute race1", .x))
      NA_character_
    })),
    race2 = purrr::map_chr(1:nrow(df_input), ~ tryCatch({
      extract_race_legacy(df_input[.x, , drop = FALSE], race_position = 2)
    }, error = function(e) {
      warning(sprintf("Row %s: Unable to compute race2", .x))
      NA_character_
    })),

    education = df_input$edu,
    zip = df_input$zip,
    employment = df_input$employment,
    occupation = df_input$occupation,
    income = df_input$income,
    married = df_input$married,
    live_alone = df_input$live_alone,
    household_number = df_input$household_number,

    # health behaviors
    ego_alcohol = df_input$alcohol,
    ego_smoke = df_input$smoke,
    ego_exercise = df_input$exercise,
    ego_diet = df_input$diet,

    # health problems
    health1 = purrr::map_chr(1:nrow(df_input), ~ tryCatch({
      extract_health_problems(df_input[.x, , drop = FALSE], health_position = 1)
    }, error = function(e) {
      warning(sprintf("Row %s: Unable to compute health1", .x))
      NA_character_
    })),
    health2 = purrr::map_chr(1:nrow(df_input), ~ tryCatch({
      extract_health_problems(df_input[.x, , drop = FALSE], health_position = 2)
    }, error = function(e) {
      warning(sprintf("Row %s: Unable to compute health2", .x))
      NA_character_
    })),
    health3 = purrr::map_chr(1:nrow(df_input), ~ tryCatch({
      extract_health_problems(df_input[.x, , drop = FALSE], health_position = 3)
    }, error = function(e) {
      warning(sprintf("Row %s: Unable to compute health3", .x))
      NA_character_
    })),
    health4 = purrr::map_chr(1:nrow(df_input), ~ tryCatch({
      extract_health_problems(df_input[.x, , drop = FALSE], health_position = 4)
    }, error = function(e) {
      warning(sprintf("Row %s: Unable to compute health4", .x))
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
    egoless_min_degree = tryCatch({
      calc_egoless_min_degree_df(df_input)
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
