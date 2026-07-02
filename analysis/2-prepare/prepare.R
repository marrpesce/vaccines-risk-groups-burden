# _________________________________________________
# Purpose:
# Prepare one cohort extract for vaccination coverage and burden analyses
# _________________________________________________

# Preliminaries ----

# Import libraries
library("tidyverse")
library("dtplyr")
library("lubridate")
library("arrow")
library("here")
library("glue")

# Import custom functions
source(here("analysis", "0-lib", "design.R"))

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  cohort_id <- "flu_2023_24"
} else {
  cohort_id <- args[[1]]
}

cohort_info <- cohort_info |>
  filter(cohort_id == .env$cohort_id)

cohort_id_value <- cohort_info$cohort_id[[1]]
target_value <- cohort_info$target[[1]]
cohort_value <- cohort_info$cohort[[1]]
cohort_start_date_value <- cohort_info$cohort_start_date[[1]]
cohort_end_date_value <- cohort_info$cohort_end_date[[1]]
cohort_end_days_value <- cohort_info$cohort_end_days[[1]]
age_threshold_value <- cohort_info$age_threshold[[1]]
clinical_priority_value <- cohort_info$clinical_priority[[1]]

output_dir <- here("output", "2-prepare", glue("prepare_{cohort_id}"))
fs::dir_create(output_dir)

options(width = 200)

# Import extract ----

data_extract <- read_feather(
  here("output", "1-extract", glue("extract_{cohort_id}.arrow"))
)

capture.output(
  skimr::skim_without_charts(data_extract),
  file = fs::path(output_dir, "data_extract_skim.txt"),
  split = FALSE
)

# Prepare dataset ----

data_prepared <-
  data_extract |>
  lazy_dt() |>
  mutate(
    cohort_id = cohort_id_value,
    target = target_value,
    cohort = cohort_value,
    cohort_start_date = cohort_start_date_value,
    cohort_end_date = cohort_end_date_value,
    cohort_end_days = cohort_end_days_value,
    age_threshold = age_threshold_value,

    all = "All",

    # demographics
    !!!standardise_demographic_characteristics,
    !!!standardise_primis_and_extended_characteristics) |>
  mutate(

    # eligibility
    age_above_eligibility_threshold = age >= age_threshold,

    # used to choose if the at risk group is all clinical risk variables
    # or just immunosuppressed people
    clinical_priority = .data[[clinical_priority_value]],

    clinical_priority_only = clinical_priority & !age_above_eligibility_threshold,

    any_eligibility = age_above_eligibility_threshold | clinical_priority | carehome_status,

    # baseline vaccination history
    baseline_vax_status = case_when(
      target == "Influenza" &
        last_vax_date_before_start_date >= cohort_start_date - years(1) ~ "Vaccinated",

      target == "Influenza" ~ "Unvaccinated",

      target == "COVID-19" &
        last_vax_date_before_start_date >= cohort_start_date - months(6) ~ "<6 months",

      target == "COVID-19" &
        last_vax_date_before_start_date >= cohort_start_date - months(12) ~ "6-11 months",

      target == "COVID-19" &
        !is.na(last_vax_date_before_start_date) ~ ">=12 months",

      target == "COVID-19" ~ "Unvaccinated",

      target == "RSV" &
        last_vax_date_before_start_date >= as.Date("2024-09-01") ~ "Vaccinated",

      target == "RSV" ~ "Unvaccinated",

      TRUE ~ NA_character_
    ) |>
      factor(),

    # censoring
    censor_date = pmin(
      deregistered_date,
      death_date,
      cohort_end_date,
      na.rm = TRUE
    ),

    # follow-up vaccination
    vax_time = as.integer(
      pmin(first_vax_date_after_start_date, death_date, censor_date, na.rm = TRUE) -
        cohort_start_date
    ) + 1L,

    vax_indicator =
      first_vax_date_after_start_date <= pmin(death_date, censor_date, na.rm = TRUE) &
      !is.na(first_vax_date_after_start_date),

    # severe outcome: admitted
    admitted_time = as.integer(
      pmin(admitted_date, death_date, censor_date, na.rm = TRUE) -
        cohort_start_date
    ) + 1L,

    admitted_indicator =
      admitted_date <= pmin(death_date, censor_date, na.rm = TRUE) &
      !is.na(admitted_date),

    # severe outcome: primary diagnosis admitted
    admitted_primary_time = as.integer(
      pmin(admitted_primary_date, death_date, censor_date, na.rm = TRUE) -
        cohort_start_date
    ) + 1L,

    admitted_primary_indicator =
      admitted_primary_date <= pmin(death_date, censor_date, na.rm = TRUE) &
      !is.na(admitted_primary_date),

    # disease-specific death
    disease_death_time = as.integer(
      pmin(disease_death_date, death_date, censor_date, na.rm = TRUE) -
        cohort_start_date
    ) + 1L,

    disease_death_indicator =
      disease_death_date <= pmin(death_date, censor_date, na.rm = TRUE) &
      !is.na(disease_death_date),

    # all-cause death
    death_time = as.integer(
      pmin(death_date, censor_date, na.rm = TRUE) -
        cohort_start_date
    ) + 1L,

    death_indicator =
      death_date <= censor_date &
      !is.na(death_date),

    # deregistration
    deregistration_time = as.integer(
      pmin(deregistered_date, censor_date, na.rm = TRUE) -
        cohort_start_date
    ) + 1L,

    deregistration_indicator =
      deregistered_date <= censor_date &
      !is.na(deregistered_date),

    alive_and_registered = !death_indicator & !deregistration_indicator
  ) |>
  as_tibble() |>
  mutate(
    vax_status = case_when(
      vax_indicator ~ "vaccinated",
      death_date <= censor_date & !is.na(death_date) ~ "died",
      TRUE ~ "censored"
    ) |>
      factor(levels = c("censored", "vaccinated", "died")),

    across(
      where(is.factor) | where(is.character),
      ~ fct_drop(fct_na_value_to_level(.x, level = "(Missing)"))
    )
  )

# Checks ----

time_check <- data_prepared |>
  summarise(
    min_vax_time = min(vax_time, na.rm = TRUE),
    min_admitted_time = min(admitted_time, na.rm = TRUE),
    min_admitted_primary_time = min(admitted_primary_time, na.rm = TRUE),
    min_disease_death_time = min(disease_death_time, na.rm = TRUE),
    min_death_time = min(death_time, na.rm = TRUE),
    min_deregistration_time = min(deregistration_time, na.rm = TRUE)
  )

print(time_check)

capture.output(
  skimr::skim_without_charts(data_prepared),
  file = fs::path(output_dir, "data_prepared_skim.txt"),
  split = FALSE
)

write_feather(
  data_prepared,
  fs::path(output_dir, glue("prepare_{cohort_id}.arrow"))
)


vax_prod_table <- bind_rows(
  data_prepared |>
    transmute(
      indicador = "last_vax_before_start",
      date = last_vax_date_before_start_date,
      product = last_vax_product_before_start
    ),

  data_prepared |>
    transmute(
      indicador = "first_vax_after_start",
      date = first_vax_date_after_start_date,
      product = first_vax_product_after_start
    )
) |>
  filter(!is.na(date)) |>
  mutate(
    year = year(date),
    period = case_when(
      month(date) %in% 9:12 ~ paste0("Sep-Feb/", year(date)),
      month(date) %in% 1:2  ~ paste0("Sep-Feb/", year(date) - 1),
      month(date) %in% 3:8  ~ paste0("Feb-Aug/", year(date)),
      TRUE ~ NA_character_
    )
  ) |>
  count(indicador, period, product, name = "n_round10") |>
  mutate(
    n_round10 = round_any(n_round10, sdc_threshold
  )) |>
  arrange(indicador, period, product)

write_csv(
  vax_prod_table,
  fs::path(output_dir, glue("vax_prod_table_{cohort_id}.csv"))
)

# Table 1 ----
# This information is inside each adjusted_estimates()
# table1_summary <- function(...) {
#   group_names <- c(...)

#   summary_table <-
#     data_prepared |>
#     group_by(across(all_of(group_names))) |>
#     lazy_dt() |>
#     summarise(
#       target = first(target),
#       cohort_id = first(cohort_id),
#       cohort = first(cohort),

#       number_total_subgroup = n(),

#       number_baseline_vax_status_vaccinated =
#         sum(baseline_vax_status == "Vaccinated", na.rm = TRUE),

#       number_baseline_vax_status_unvaccinated =
#         sum(baseline_vax_status == "Unvaccinated", na.rm = TRUE),

#       number_baseline_vax_status_less_than_6_months =
#         sum(baseline_vax_status == "<6 months", na.rm = TRUE),

#       number_baseline_vax_status_6_11_months =
#         sum(baseline_vax_status == "6-11 months", na.rm = TRUE),

#       number_baseline_vax_status_12_months_or_more =
#         sum(baseline_vax_status == ">=12 months", na.rm = TRUE),

#       number_vax_post_index_date =
#         sum(vax_indicator, na.rm = TRUE),

#       number_admitted =
#         sum(admitted_indicator, na.rm = TRUE),

#       number_admitted_primary =
#         sum(admitted_primary_indicator, na.rm = TRUE),

#       number_disease_death =
#         sum(disease_death_indicator, na.rm = TRUE),

#       number_all_cause_death =
#         sum(death_indicator, na.rm = TRUE),

#       number_deregistered =
#         sum(deregistration_indicator, na.rm = TRUE),

#       .groups = "drop"
#     ) |>
#     as_tibble()

#   return(summary_table)
# }

# table1 <-
#   level_combos |>
#   mutate(
#     table1 = map2(
#       group1, group2,
#       .f = function(x, y) {
#         if (is.na(y)) y <- NULL

#         lookup <- c(
#           group1_value = x,
#           group2_value = y
#         )

#         table1_summary(x, y) |>
#           mutate(across(c(all_of(c(x, y))), as.character)) |>
#           rename(any_of(lookup))
#       }
#     )
#   ) |>
#   unnest(table1) |>
#   select(
#     target,
#     cohort_id,
#     cohort,
#     group1,
#     group1_value,
#     group2,
#     group2_value,
#     everything()
#   )

# Save ----
# write_csv(
#   table1,
#   fs::path(output_dir, glue("table1_{cohort_id}.csv"))
# )
