# _______________________________________________________________________________________
# Purpose:
# Report the distribution of vaccines schedules in different population subgroups as at a given date
# _______________________________________________________________________________________


# Preliminaries ----

# Import libraries
library("tidyverse")
library("dtplyr")
library("lubridate")
library("glue")
library("here")
library("arrow")
library("survival")
library("splines")
library("parglm")

# Import custom functions
source(here("analysis", "0-lib", "design.R"))

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  # use for interactive testing
  # removeobjects <- FALSE
  snapshot_date <- as.Date("20220321", format = "%Y%m%d")
} else {
  # removeobjects <- TRUE
  snapshot_date <- as.Date(args[[1]], format = "%Y%m%d")
}

# how wide are the temporal bins for frequencies over time? in days
# use fix width, rather than months or bi-months or quarters, so that temporal denominator is even and the ratio of weekends to weekdays is fixed
temporal_resolution_history <- 28L

# how wide are the temporal bins for frequencies over time for Kaplan-Meier plots? in days
temporal_resolution_km <- 7L

campaign_info <- campaign_info |> filter(campaign_start_date == snapshot_date)

# list2env(campaign_info, globalenv())

# overwrite primary milestone to match rounded kaplan meier curve
campaign_info$primary_milestone_days <- ceiling_any(campaign_info$primary_milestone_days, temporal_resolution_km)


# dates to round down to
# use this with `findInterval` until lubridate package is updated in the opensafely R image
# (then use `floor_date(date, unit=floor_dates`)
floor_dates <- seq(
  as.Date("2020-06-01"), # monday
  as.Date("2029-12-31"),  # to monday!
  by = temporal_resolution_history
)

# create string representation of date in compact format YYYMMDD
snapshot_date_compact <- format(snapshot_date, "%Y%m%d")

## Create output directory
output_dir <- here("output", "4-snapshot", glue("report_snapshot_{snapshot_date_compact}"))
fs::dir_create(output_dir)
options(width = 200) # set output width for capture.output


# Import processed data ----
data_snapshot <- read_feather(here("output", "1-extract", glue("extract_snapshot_{snapshot_date_compact}.arrow")))
data_fixed <- read_feather(here("output", "2-prepare", "prepare", "data_fixed.arrow"))

capture.output(
  skimr::skim_without_charts(data_snapshot),
  file = fs::path(output_dir, "data_snapshot_skim.txt"),
  split = FALSE
)

# merge fixed data and vaccine data onto snapshot data
# note that in dummy data this doesn't work very well because patient IDs might not be matched across all datasets
data_combined <-
  data_snapshot |>
  lazy_dt() |>
  left_join(
    lazy_dt(data_fixed) |> select(patient_id, sex, ethnicity5, ethnicity16, death_date, covid_death_date),
    by = "patient_id"
  ) |>
  # remove currently unused variables
  select(
    -covid_vax_prior_2_date,
    -covid_vax_prior_2_product,
    -covid_vax_prior_3_date,
    -covid_vax_prior_3_product,
    -stp,
    -age # remove snapshot age  and replace (below) with age_eligibility
  ) |>
  rename(

    # overwrite age to be that of a date later on in the campaign (rather than as at the start date) to allow "operational flexibility"
    age = eligibility_age,

    # previous vaccine summary
    # add more variables here based on covid_vax_prior_1_date, covid_vax_prior_2_date,... etc if needed
    vax_count = covid_vax_prior_count,
    last_vax_date = covid_vax_prior_1_date,
    last_vax_product = covid_vax_prior_1_product,

    # info on first vaccine(s) received after snapshot date
    next_vax_date = covid_vax_1_date,
    next_vax_product = covid_vax_1_product,
    # next2_vax_date = covid_vax_2_date,
  ) |>
  mutate(
    all = "All",

    !!!standardise_demographic_characteristics,
    !!!standardise_primis_and_extended_characteristics,

    age_above_eligiblity_threshold = (age >= campaign_info$age_threshold),

    # used to chose if the at risk group is all clinical risk variables or just immunosuppressed people
    clinical_priority = .data[[campaign_info$clinical_priority]],

    clinical_priority_only = clinical_priority & !age_above_eligiblity_threshold,

    any_eligibility = age_above_eligiblity_threshold | clinical_priority | carehome_status,

    last_vax_product = fct_na_value_to_level(last_vax_product, "Unvaccinated"),
    last_vax_date = if_else(vax_count == 0, study_dates$firstpossiblevax_date + as.integer(runif(n(), 0, 10)), last_vax_date),
    # last_vax_week = floor_date(last_vax_date, unit = "week", week_start = 1), # starting on a monday
    last_vax_period = floor_date(last_vax_date, unit = floor_dates), # use floor_dates[findInterval(last_vax_date, floor_dates)] if lubridate isn't working

    censor_date = pmin(
      deregistered_date,
      campaign_info$final_milestone_date,
      study_dates$end_date,
      na.rm = TRUE
    ),

    # time from snapshot date until next vaccination
    vax_time = as.integer(pmin(next_vax_date, death_date, censor_date, na.rm = TRUE) - snapshot_date) + 1L, # +1 because vaccination on snapshot date is allowed, but events at time zero are not
    vax_indicator = (next_vax_date <= pmin(censor_date, death_date, na.rm = TRUE)) & !is.na(next_vax_date),

    # time from snapshot date until covid hospital admission
    covid_admitted_time = as.integer(pmin(covid_admitted_date, death_date, censor_date, na.rm = TRUE) - snapshot_date) + 1L,
    covid_admitted_indicator = (covid_admitted_date <= pmin(censor_date, death_date, na.rm = TRUE)) & !is.na(covid_admitted_date),

    # time from snapshot date until covid (primary position only) hospital admission
    covid_admitted_primary_time = as.integer(pmin(covid_admitted_primary_date, death_date, censor_date, na.rm = TRUE) - snapshot_date) + 1L,
    covid_admitted_primary_indicator = (covid_admitted_primary_date <= pmin(censor_date, death_date, na.rm = TRUE)) & !is.na(covid_admitted_primary_date),

    # time from snapshot date until covid critical care admission
    covid_critcare_time = as.integer(pmin(covid_critcare_date, death_date, censor_date, na.rm = TRUE) - snapshot_date) + 1L,
    covid_critcare_indicator = (covid_critcare_date <= pmin(censor_date, death_date, na.rm = TRUE)) & !is.na(covid_critcare_date),

    # time from snapshot date until covid death
    covid_death_time = as.integer(pmin(covid_death_date, death_date, censor_date, na.rm = TRUE) - snapshot_date) + 1L,
    covid_death_indicator = (covid_death_date <= pmin(censor_date, death_date, na.rm = TRUE)) & !is.na(covid_death_date),

    # time from snapshot date until death
    death_time = as.integer(pmin(death_date, censor_date, na.rm = TRUE) - snapshot_date) + 1L,
    death_indicator = (death_date <= pmin(censor_date, death_date, na.rm = TRUE)) & !is.na(death_date),

    # time from snapshot date until deregistration
    deregistration_time = as.integer(pmin(deregistered_date, censor_date, na.rm = TRUE) - snapshot_date) + 1L,
    deregistration_indicator = (deregistered_date <= pmin(censor_date, na.rm = TRUE)) & !is.na(deregistered_date),

    # indicator for if patient is alive and registered at the end of the campaign (for comparison with UKHSA reporting)
    alive_and_registered = (!death_indicator) & (!deregistration_indicator)
  ) |>
  as_tibble() |>
  mutate(
    vax_status = case_when(
      vax_indicator ~ "vaccinated",
      (death_date <= censor_date) & !is.na(death_date) ~ "died",
      (censor_date < death_date) | is.na(death_date) ~ "censored",
      .default = NA_character_
    ) |> factor(levels = c("censored", "vaccinated", "death")),
    vax_status_product = if_else(vax_indicator, next_vax_product, vax_status),
    next_vax_product_uncensored = if_else(vax_indicator, next_vax_product, NA_character_),

    # setting missing values to explicit missing
    across(where(is.factor) | where(is.character), ~ fct_drop(fct_na_value_to_level(.x, level = "(Missing)")))
  )

# remove snapshot and fixed datasets to save memory, as these is all captured in the data_combiend dataset.
rm(data_snapshot, data_fixed)
# recover memory
gc()


capture.output(
  skimr::skim_without_charts(data_combined),
  file = fs::path(output_dir, "data_combined_skim.txt"),
  split = FALSE
)

# ________________________________________________________________________________________
# Pre-snapshot date vaccine history, stratified by characteristic recorded on the snapshot_date ----
# ________________________________________________________________________________________


## _______________________________________________________________________________________
cat("## Report info on date of last vaccination", "\n")
## _______________________________________________________________________________________

# This produces data and a plot showing the distribution of most recent prior vaccination date (to the nearest 4 weeks)
# if no documented prior vaccination, then values are stacked on the LHS of the chart

plot_date_of_last_dose <- function(subgroup) {

  over2years_dummy_date <- (snapshot_date - ceiling_any(365 * 2, 7))

  summary_by <-
    data_combined |>
    mutate(
      subgroup = .data[[subgroup]]
    ) |>
    lazy_dt() |>
    group_by(subgroup, last_vax_product, last_vax_period) |>
    summarise(
      n = roundmid_any(n(), sdc_threshold)
    ) |>
    ungroup() |>
    mutate(
      # if last vaccination date was over 2 years ago, replace with dummy date
      last_vax_period = if_else(
        (last_vax_period < over2years_dummy_date)  | is.na(last_vax_period),
        over2years_dummy_date - 42,
        last_vax_period
      )
    ) |>
    as_tibble() |>
    complete(
      subgroup, last_vax_product, last_vax_period,
      fill = list(n = 0)
    )

  breaks <- seq(as.Date("2021-01-01"),  as.Date("2028-06-01"), by = "6 month")

  temp_plot <-
    summary_by |>
    ggplot() +
    geom_col(
      aes(x = last_vax_period, y = n, fill = last_vax_product, group = last_vax_product),
      alpha = 0.5,
      position = position_stack(reverse = TRUE),
      # position=position_identity(),
      width = temporal_resolution_history
    ) +
    facet_grid(
      rows = vars(subgroup),
      switch = "y",
      space = "free_x",
      scales = "free_x"
    ) +
    labs(
      x = "Date of most recent COVID-19 vaccine",
      y = NULL,
      fill = NULL
    ) +
    scale_fill_brewer(
      palette = "Set2",
      na.value = "grey50",
      labels = function(breaks) {
        breaks[is.na(breaks)] <- "Other"
        breaks
      }
    ) +
    scale_x_date(
      breaks = c(over2years_dummy_date - 42, breaks),
      date_minor_breaks = "month",
      # labels = ~{c("Unvaccinated", scales::label_date("%Y")(.x[-1]))},
      labels = c("+2 years prior)", scales::label_date("%Y-%b")(breaks)),
    ) +
    theme_minimal() +
    theme(
      axis.text.x.top = element_text(hjust = 0),
      axis.text.x.bottom = element_text(hjust = 0),
      strip.text.y.left = element_text(angle = 0, hjust = 1),
      axis.ticks.x = element_line(),
      strip.placement = "outside",
      axis.text.y = element_blank(),
      legend.position = "bottom"
    )

  # print(temp_plot)

  ggsave(fs::path(output_dir, glue("last_vax_date_{subgroup}.png")), plot = temp_plot)

  # write tables that capture underlying plotting data
  # write_csv(summary_by, fs::path(output_dir, glue("last_vax_date_{subgroup}.csv")))
}

for (group in level1_group) {
  plot_date_of_last_dose(group)
}

## _______________________________________________________________________________________
cat("## Report info on prior dose count and product type", "\n")
## _______________________________________________________________________________________

plot_vax_count <- function(subgroup) {

  summary_by <-
    data_combined |>
    mutate(
      subgroup = .data[[subgroup]],
      vax_count_group = cut(vax_count, c(-Inf, 0, 2, 4, Inf), labels = c("0", "1-2", "3-4", "5+")),
    ) |>
    lazy_dt() |>
    group_by(subgroup, vax_count_group) |>
    summarise(
      n = roundmid_any(n(), sdc_threshold),
    ) |>
    ungroup() |>
    as_tibble() |>
    complete(
      subgroup, vax_count_group,
      fill = list(n = 0)
    ) |>
    group_by(subgroup) |>
    mutate(
      row_total = sum(n),
      prop = ifelse(row_total > 0, n / row_total * 100, 0),
    ) |>
    ungroup()

  temp_plot <-
    ggplot(summary_by) +
    geom_bar(
      aes(x = prop, y =  subgroup, width = row_total, fill = vax_count_group),
      stat = "identity", # position = "fill",
      position = position_stack(reverse = TRUE),
    ) +
    facet_grid(
      rows = vars(subgroup),
      scales = "free_y",
      space = "free_y") +
    labs(
      x = "%",
      y = NULL,
      fill = "Vaccine count"
    ) +
    scale_fill_brewer(
      palette = "Set2",
      na.value = "grey50",
      labels = function(breaks) {
        breaks[is.na(breaks)] <- "(Missing)"
        breaks
      }
    ) +
    theme_minimal() +
    theme(
      axis.text.x.top = element_text(hjust = 0),
      axis.text.x.bottom = element_text(hjust = 0),
      strip.text.y.left = element_text(angle = 0, hjust = 1),
      axis.ticks.x = element_line(),
      strip.text = element_blank(),
      legend.position = "bottom"
    ) +
    NULL

  # print(temp_plot)

  ggsave(fs::path(output_dir, glue("vax_count_{subgroup}.png")), plot = temp_plot)

  # write tables that capture underlying plotting data
  write_csv(summary_by, fs::path(output_dir, glue("vax_count_{subgroup}.csv")))
}


for (group in level1_group) {
  plot_vax_count(group)
}


## _______________________________________________________________________________________
cat("## Report info in a standardised table", "\n")
## _______________________________________________________________________________________

# function to print table for an abritrary number of grouping variables

# groups are passed as strings via dots (...)
table_prior_vax_summary <- function(...) {

  group_names <- c(...)

  summary_table <-
    data_combined |>
    mutate(days_since_vax = snapshot_date - last_vax_date) |>
    group_by(across(all_of(group_names))) |>
    lazy_dt() |>
    summarise(
      # Dose counts
      total = roundmid_any(n(), sdc_threshold),
      count_n0 = roundmid_any(sum(vax_count == 0, na.rm = TRUE), sdc_threshold),
      count_n1 = roundmid_any(sum(vax_count == 1, na.rm = TRUE), sdc_threshold),
      count_n2 = roundmid_any(sum(vax_count == 2, na.rm = TRUE), sdc_threshold),
      count_n3 = roundmid_any(sum(vax_count == 3, na.rm = TRUE), sdc_threshold),
      count_n4 = roundmid_any(sum(vax_count == 4, na.rm = TRUE), sdc_threshold),
      count_n5plus = roundmid_any(sum(vax_count >= 5, na.rm = TRUE), sdc_threshold),
      # Dose summary
      count_median = quantile(vax_count, probs = 0.5, na.rm = TRUE),
      count_p10 = quantile(vax_count, probs = 0.10, na.rm = TRUE),
      count_p25 = quantile(vax_count, probs = 0.25, na.rm = TRUE),
      count_p75 = quantile(vax_count, probs = 0.75, na.rm = TRUE),
      count_p90 = quantile(vax_count, probs = 0.90, na.rm = TRUE),
      # Vaccination in past 12 and 24 months
      days_since_n12m = roundmid_any(sum(days_since_vax <= 365, na.rm = TRUE), sdc_threshold),
      days_since_n24m = roundmid_any(sum(days_since_vax <= 365 * 2, na.rm = TRUE), sdc_threshold),
      # Time since last dose
      days_since_median = quantile(days_since_vax, probs = 0.5, na.rm = TRUE),
      days_since_p10 = quantile(days_since_vax, probs = 0.10, na.rm = TRUE),
      days_since_p25 = quantile(days_since_vax, probs = 0.25, na.rm = TRUE),
      days_since_p75 = quantile(days_since_vax, probs = 0.75, na.rm = TRUE),
      days_since_p90 = quantile(days_since_vax, probs = 0.90, na.rm = TRUE),

      .groups = "drop"
    ) |>
    mutate(
      # Dose percentages - put this here and not in earlier summarise step so that it works with dtplyr
      count_pct0 = round(count_n0 * 100 / total, 1),
      count_pct1 = round(count_n1 * 100 / total, 1),
      count_pct2 = round(count_n2 * 100 / total, 1),
      count_pct3 = round(count_n3 * 100 / total, 1),
      count_pct4 = round(count_n4 * 100 / total, 1),
      count_pct5plus = round(count_n5plus * 100 / total, 1),
      # Vaccination % in past 12 and 24 months
      days_since_pct12m = round(days_since_n12m * 100 / total, 1),
      days_since_pct24m = round(days_since_n24m * 100 / total, 1),
    ) |>
    as_tibble()

  # subgroup_name <- map_chr(rlang::quos(...), rlang::as_name) |> paste0(collapse = "_")

  # Write table to a CSV file
  # write_csv(summary_table, fs::path(output_dir, glue("prior_vax_table_{subgroup_name}.csv")))

  return(summary_table)
}


# for testing function interactively
# table_prior_vax_summary("all", "all")
# table_prior_vax_summary("all", "ageband4")
# table_prior_vax_summary("ageband4", "all")
# table_prior_vax_summary("ageband4", "sex")

# loop over all group1 and group2 variable combinations and combine into one big dataset
prior_vax_summary_table_all <-
  level_combos |>
  mutate(
    prior_vax_summary = map2(
      group1, group2,
      .f = function(x, y) {

        if (is.na(y)) y <- NULL
        lookup <- c(group1_value = x, group2_value = y)

        table_prior_vax_summary(x, y) |>
          mutate(across(c(all_of(c(x, y))), as.character)) |>
          rename(any_of(lookup))
      }
    )
  ) |>
  unnest(prior_vax_summary) |>
  select(group1, group1_value, group2, group2_value, everything())

# Write table to a CSV file
write_csv(prior_vax_summary_table_all, fs::path(output_dir, glue("prior_vax_table.csv")))


# ________________________________________________________________________________________
# Post-snapshot vaccine coverage, stratified by characteristic recorded on the snapshot_date ----
# ________________________________________________________________________________________

# This code borrows heavily from the KM reusable action https://github.com/opensafely-actions/kaplan-meier-function/blob/main/analysis/km.R
# The reason not to use the KM reusable action directly is that we need to reuse it multiple times across many different stratification variables
# This would create a large number of project.yaml actions, so it's easier to do the repeats within a single script
# The reusable action could be modified to enable multiple stratified analyses to be run, but it's a bit faffy and a bit scope-creepy


## tests ----

times_count <- table(cut(data_combined$vax_time, c(-Inf, 0, 1, Inf), right = FALSE, labels = c("<0", "0", ">0")), useNA = "ifany")

if (!identical(as.integer(times_count), c(0L, 0L, nrow(data_combined)))) {
  print(times_count)
  stop("all event times must be strictly positive")
}

## Function to calculate KM estimates for a given stratification variable ----

# group variables are provided as characters via dots (...)
# resolution argument is the precision used for the time dimension. If zero, then original resolution is used.
km_estimates <- function(data, group_name1, group_name2, event_name, event_time, event_indicator, resolution = 0) {

  group_names <- c(group_name1, group_name2)

  # if (is.na(group_name2)) {
  #   group_name2 <- "all"
  # }

  data_outcome <-
    data |>
    select(
      patient_id,
      all_of(group_names),
      event_time = any_of(event_time),
      event_indicator = any_of(event_indicator)
    ) |>
    mutate(event_time = ceiling_any(event_time, resolution))

  data_km <-
    data_outcome |>
    group_by(across(all_of(group_names))) |>
    nest() |>
    mutate(
      surv_obj_tidy = map(
        data, ~ {
          survfit(
            Surv(event_time, event_indicator) ~ 1,
            data = .x,
            conf.type = "log-log"
          ) |>
            broom::tidy() |>
            add_row(
              time = 0, # assumes time origin is zero
              n.risk = 0,
              n.event = 0,
              n.censor = 0,
              estimate = 1,
              conf.low = 1,
              conf.high = 1,
              .before = 1L
            ) |>
            complete(
              time = seq(0L, campaign_info$final_milestone_days, resolution), # fill in 1 row for each period (defined by resolution) of follow up
              fill = list(n.event = 0L, n.censor = 0L) # fill in zero events on those days
            ) |>
            fill(
              n.risk,
              .direction = "up"
            ) |>
            fill(
              estimate, conf.low, conf.high,
              .direction = "down"
            )
        }
      ),
    ) |>
    select(-data) |>
    unnest(surv_obj_tidy) |>
    mutate(

      # disclosure control
      n.risk = ceiling_any(n.risk, sdc_threshold),
      estimate = plyr::round_any(estimate, sdc_threshold / nth(n.risk, 2)), # use 2nd value as this skips the t=0 row where n.risk=0
      conf.low = plyr::round_any(conf.low, sdc_threshold / nth(n.risk, 2)),
      conf.high = plyr::round_any(conf.high, sdc_threshold / nth(n.risk, 2)),

      # cumulative incidence
      cmlinc = 1 - estimate,
      cmlinc.low = 1 - conf.high,
      cmlinc.high = 1 - conf.low,
    )

  rm(data_outcome)

  # print(data_km)

  # write tables that capture underlying plotting data
  data_km_nozero <-
    data_km |>
    arrange(across(all_of(group_names))) |>
    ungroup() |>
    filter(time != 0) |>
    select(
      all_of(group_names),
      time,
      cmlinc,
      cmlinc.low,
      cmlinc.high,
    )

  # commented out because outputting the entire data creates a dataset that is too large to output-check on it's own
  # write_csv(data_km_nozero, fs::path(output_dir, glue("km_{event_name}_{paste0(group_names, collapse='_')}.csv")))

  return(data_km_nozero)

}

# for testing function interactively
# km_estimates_vax <- partial(
#   km_estimates, data = data_combined, event_name = "vax", event_time = "vax_time", event_indicator = "vax_indicator", resolution = temporal_resolution_km
# )
# km_estimates_vax("all", "all")
# km_estimates_vax("all", "ageband4")
# km_estimates_vax("ageband4", "all")
# km_estimates_vax("ageband4", "sex")

## Function to get km estimates over all subgroup combinations ----

get_all_km_estimates <- function(data, event_name, event_time, event_indicator, resolution) {
  # loop over all group1 and group2 variable combinations and combine into one big dataset
  km_estimates_table <-
    level_combos |>
    mutate(
      km_summary = map2(
        group1, group2,
        .f = \(x, y) {

          if (is.na(y)) y <- NULL
          lookup <- c(group1_value = x, group2_value = y)

          km_estimates(data = data, group_name1 = x, group_name2 = y, event_name = event_name, event_time = event_time, event_indicator = event_indicator, resolution = temporal_resolution_km) |>
            mutate(across(c(all_of(c(x, y))), as.character)) |>
            rename(all_of(c(group1_value = x, group2_value = y)))
        }
      )
    ) |>
    unnest(km_summary) |>
    select(group1, group1_value, group2, group2_value, everything()) |>
    mutate(
      early_milestone = (time == campaign_info$early_milestone_days) * 1L,
      primary_milestone = (time == campaign_info$primary_milestone_days) * 1L,
      final_milestone = (time == campaign_info$final_milestone_days) * 1L,
    )


  # Write table to CSV files
  # split up by level1 grouping variables, so as not to exceed 5,000 row limit
  iwalk(
    split(km_estimates_table, km_estimates_table$group1),
    ~ write_csv(.x, fs::path(output_dir, glue("km_estimates_{event_name}_table_{.y}.csv")))
  )

  # Write table to a CSV file containing _only_ reporting milestones
  km_estimates_milestones <-
    km_estimates_table |>
    filter(
      time %in% (c(campaign_info$early_milestone_days, campaign_info$primary_milestone_days, campaign_info$final_milestone_days) * 1L)
    ) |>
    mutate(
      milestone_date = case_when(
        early_milestone == 1 ~ campaign_info$early_milestone_date,
        primary_milestone == 1 ~ campaign_info$primary_milestone_date,
        final_milestone == 1 ~ campaign_info$final_milestone_date,
      ),
      milestone = case_when(
        early_milestone == 1 ~ "Early",
        primary_milestone == 1 ~ "Primary",
        final_milestone == 1 ~ "Final",
      )
    ) |>
    rename(days_since_campaign_start = time) |>
    select(-early_milestone, -primary_milestone, -final_milestone)

  # extremely irritatingly we have to split this output up because it's too big to be shown (5000 row limit)
  iwalk(
    split(km_estimates_milestones, km_estimates_milestones$milestone),
    ~ write_csv(.x, fs::path(output_dir, glue("km_estimates_{event_name}_milestones_{.y}.csv")))
  )

  return(km_estimates_table)
}

## _______________________________________________________________________________________
cat("## Report KM cumulative incidence of vaccination in a standardised table", "\n")
## _______________________________________________________________________________________


km_estimates_vax_table <- get_all_km_estimates(data = data_combined, event_name =  "vax", event_time = "vax_time", event_indicator = "vax_indicator")
km_estimates_vax_alive_table <- get_all_km_estimates(filter(data_combined, alive_and_registered), "vax_alive", event_time = "vax_time", event_indicator = "vax_indicator")

# consider raw KM plots for disease burden too
# km_estimates_covid_admitted_table <- get_all_km_estimates(data = data_combined, event_name = "covid_admitted", event_time = "covid_admitted_time", event_indicator = "covid_admitted_indicator")


# km_estimates_vax_table |>
#   group_by(group1) |>
#   summarise(n = n())


## function to make KM plots for the data ----

km_plot <- function(km_data, event_name, group1, group2) {

  group_names <- c(group1, group2)

  # plot km curves locally for checking (but probs not for release as these can be reconstructed from released data)
  coverage_plot <-
    bind_rows(
      # this bit adds an extra row of data so that the firsrt line of the KM plot shows
      km_data |> summarise(
        time = 0,
        cmlinc = 0, cmlinc.low = 0, cmlinc.high = 0, lagtime = 0,
        .by = c("group1", "group1_value", "group2", "group2_value")
      ),
      km_data |> mutate(
        lagtime = lag(time, 1L, 0L), # assumes the time-origin is zero

        .by = c("group1", "group1_value", "group2", "group2_value")
      )
    ) |>
    ggplot() +
    geom_step(aes(x = time, y = cmlinc, group = group2_value, colour = group2_value), direction = "vh") +
    geom_rect(aes(xmin = lagtime, xmax = time, ymin = cmlinc.low, ymax = cmlinc.high, group = group2_value, fill = group2_value), alpha = 0.1, colour = "transparent") +
    facet_grid(rows = "group1_value") +
    scale_color_brewer(type = "qual", palette = "Set1", na.value = "grey") +
    scale_fill_brewer(type = "qual", palette = "Set1", guide = "none", na.value = "grey") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(xlim = c(0, NA)) +
    labs(
      x = "Days since start of campaign",
      y = "Cumulative Incidence",
      colour = NULL,
      title = NULL
    ) +
    theme_minimal() +
    theme(
      axis.line.x = element_line(colour = "black"),
      panel.grid.minor.x = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(.05, .95),
      legend.justification = c(0, 1),
    )

  ggsave(fs::path(output_dir, glue("km_{event_name}_{paste0(group_names, collapse='_')}.png")), plot = coverage_plot)
  # print(coverage_plot)
}

# km_plot(km_estimates_all |> filter(group1 == "all", group2 == "crd"), "vax", "all", "crd")

## _______________________________________________________________________________________
cat("## Plot KM cumulative incidences of for inspection the server", "\n")
## _______________________________________________________________________________________


# plot km estimates for inspection the server
walk2(
  level_combos$group1, level_combos$group2,
  .f = \(x, y) {
    km_plot(
      km_data = km_estimates_vax_table |> filter(group1 == x, group2 == y),
      event_name = "vax",
      group1 = x,
      group2 = y
    )
  }
)


# ________________________________________________________________________________________
# Post-snapshot Covid-19 vaccination and disease burden up to final milestone, stratified by characteristics recorded on the snapshot_date ----
# ________________________________________________________________________________________


# list of reference categories, which overwrites default reference category if needed
contrasts_reference_levels <- list(
  ageband4 = "65-74",
  ageband13 = "65-69"
)

## Function to output HRs and IRRs for disease burden comparing different subgroups ----
# note that parglm is faster, but produces an annoying warning that "'mustart' will not be used"
# don't know how to get rid of it!

adjusted_estimates <- function(data, subgroup, event_time, event_indicator) {
  # use age-splines unless age is the subgroup of interest

  poisson_formula <- as.formula(glue("event_indicator ~ {subgroup} + sex + ns(age, 3)"))
  if (subgroup == "ageband4") poisson_formula <- as.formula(glue("event_indicator ~ ageband4 + sex"))
  if (subgroup == "ageband13") poisson_formula <- as.formula(glue("event_indicator ~ ageband13 + sex"))

  # prepare dataset
  data_outcome <-
    data |>
    mutate(
      event_time = .data[[event_time]],
      event_indicator = .data[[event_indicator]]
    ) |>
    select(
      all_of(subgroup),
      sex, age,
      event_time,
      event_indicator
    ) |>
    # to save memory, reduce dataset size by counting all distinct rows
    # then use these counts as weights when modelling / summarising later
    summarise(
      count = n(),
      .by = all_of(c(subgroup, "sex", "age",  "event_time", "event_indicator"))
    )

  # how many possible values of the group are there
  n_values <- n_distinct(data_outcome[[subgroup]])

  # all levels of variable that exist in the data, sorted by factor (or alphanumeric if not)
  # the first of these is the default reference level
  all_levels <- unique(as.character(sort(data_outcome[[subgroup]])))

  # explicitly include reference_level, and define contrasts if unusual
  # for example, if we wanted to use not the default reference level for a factor
  # this object is used inside a glm() call, to be passed to the `contrasts` argument
  if (
    (subgroup %in% names(contrasts_reference_levels)) &
      (n_values > 0) &
      ifelse(is.null(contrasts_reference_levels[[subgroup]]), FALSE, contrasts_reference_levels[[subgroup]] %in% all_levels)
  ) {
    # create the object passed to `contrasts` argument in glm call to use a different reference category
    subgroup_contrasts <- list(contr.treatment(all_levels,  which(all_levels == contrasts_reference_levels[[subgroup]])))
    names(subgroup_contrasts) <- subgroup
    reference_level <- contrasts_reference_levels[[subgroup]]
  } else {
    subgroup_contrasts <- NULL
    reference_level <- all_levels[1]
  }

  # cat(subgroup, "-", reference_level, " n=", n_values, " \n")

  # summarise total people, events, and person-time
  # and add contrast label for merging with model output later
  data_summary <-
    data_outcome |>
    mutate(label = .data[[subgroup]]) |>
    arrange(label) |>
    summarise(
      variable = subgroup,
      n_obs = roundmid_any(sum(count), sdc_threshold),
      n_event = roundmid_any(sum(event_indicator * count), sdc_threshold),
      exposure = roundmid_any(sum(event_time * count), sdc_threshold),

      .by = label
    ) |>
    mutate(
      label = as.character(label),
      reference_row = label == reference_level,
      contrast = glue("{subgroup}{label}")
    )

  # IRR model

  if (n_values > 1) {

    parglm_control <- parglm.control(maxit = 40, nthreads = 4)

    # fit the model
    # if there is an error, just return an empty dataset rather than fail
    data_poisson0 <-
      tryCatch(
        expr = {
          data_outcome |>
            parglm(
              data = _,
              formula = poisson_formula,
              family = poisson,
              offset = log(event_time),
              control = parglm_control,
              weights = count,
              contrasts = subgroup_contrasts
            ) |>
            broom.helpers::tidy_and_attach(tidy_fun = broom.helpers::tidy_parameters, ci_method = "wald") |>
            # broom.helpers::tidy_add_reference_rows() |>
            broom.helpers::tidy_add_term_labels() |>
            filter(variable == subgroup) |>
            select(variable, label, estimate, std.error, conf.low, conf.high)
          # note: the filter above is the same as doing marginaleffects::avg_comparisons(model, type = "link", variables = subgroup, comparison = "difference"),
          # as long as there are no interaction terms between subgroup and anything else
          # we use broom.helpers functions because it gives us the really nice variable and label info formatting for the outputted tidy dataset
          # if we want to use avg_comparisons in future, then attach the nicely formatted meta info onto a broom::tidy(avg_comparisons) object
          # or see "get_estimates_using_marginaleffects.R" script for a clue
        },
        error = function(e) {
          cat("error for subgroup", subgroup, ":", conditionMessage(e), "\n")
          data_summary |>
            select(variable, label) |>
            mutate(estimate = NA_real_, std.error = NA_real_, conf.low = NA_real_, conf.high = NA_real_)
        }
      )

    # combine summary and model outputs
    data_poisson <-
      full_join(
        data_summary,
        data_poisson0,
        by = c("variable", "label"),
      ) |>
      transmute(
        variable, label, reference_row,
        n_obs, n_event, exposure,
        ir = n_event / exposure,
        irr_unadjusted = ir / ir[reference_row],
        irr = exp(estimate),
        irr.low = exp(conf.low),
        irr.high = exp(conf.high),
        irr.ln.std.error = std.error,
      )

  } else {
    data_poisson <- data_summary |> select(-contrast)
  }

  return(data_poisson)

  rm(data_outcome)
  gc()
}

# adjusted_estimates(data_combined, "ageband4", "covid_admitted_time", "covid_admitted_indicator")

## function to loop over all group combinations and report IRR of level0 versus levelX ----
# for a given outcome, loop over all groups combinations, obtaining contrasts for each using adjusted_estimates function, and combining into one file
# specifically, compare level2 groups amongst each other, for all people meeting level1 group criteria
get_all_estimates <- function(data, event_name, event_time, event_indicator) {

  l1ticker <<- ""

  estimates_list <-
    level_combos |>
    mutate(
      estimates = map2(
        group1, group2,
        function(group1, group2) {
          # just to keep track of how far along we are
          if (l1ticker != group1) {
            print(group1)
            l1ticker <<- group1
          }

          summary_data <-
            data |>
            mutate(
              label1 = data[[group1]],
            ) |>
            nest(.by = c(label1), .key = "group1_subset") |>
            mutate(
              estimates = map(group1_subset, \(group1_subset) {
                adjusted_estimates(group1_subset, group2, event_time, event_indicator)
              })
            ) |>
            select(-group1_subset) |>
            unnest(estimates) |>
            select(-variable) |>
            rename(label2 = label) |>
            mutate(
              across(c(label1, label2), as.character) # to ensure the unnest() works later
            )

          return(summary_data)
          rm(summary_data)
          gc()
        }
      )
    ) |>
    unnest(estimates) |>
    select(group1, label1, group2, label2, everything()) # reorder columns

  write_csv(estimates_list, fs::path(output_dir, glue("contrasts_{event_name}.csv")))

  # return(estimates_list)

}


## _______________________________________________________________________________________
cat("## Get all IRR contrasts for covid vaccination and burden", "\n")
## _______________________________________________________________________________________

# IRR for vaccination, in the usual way
get_all_estimates(data_combined, "vax", "vax_time", "vax_indicator")

# IRR for vaccination, only looking at those who survived or stayed registered to the end of the season (collider bias! but matches UKHSA reports)
get_all_estimates(
  filter(data_combined, alive_and_registered),
  "vax_alive", "vax_time", "vax_indicator"
)

# IRR for COVID burden, in the usual way
get_all_estimates(data_combined, "covid_admitted", "covid_admitted_time", "covid_admitted_indicator")
get_all_estimates(data_combined, "covid_admitted_primary", "covid_admitted_primary_time", "covid_admitted_primary_indicator")
get_all_estimates(data_combined, "covid_critcare", "covid_critcare_time", "covid_critcare_indicator")
get_all_estimates(data_combined, "covid_death", "covid_death_time", "covid_death_indicator")


## Function to output length of stay quantiles for different subgroups ----
los_estimates <- function(data, subgroup, event_los) {

  subgroup_name <- deparse(substitute(subgroup))

  # prepare dataset
  data_outcome <-
    data |>
    mutate(
      event_los = .data[[event_los]]
    ) |>
    select(
      all_of(subgroup),
      event_los
    )

  # LoS summary stats

  data_los <-
    data_outcome |>
    mutate(
      variable = subgroup,
      label = .data[[subgroup]],
    ) |>
    summarise(
      n = roundmid_any(n(), sdc_threshold),
      n_at_least_1_event = roundmid_any(sum(!is.na(event_los)), sdc_threshold),
      median_los = quantile(event_los, 0.5, na.rm = TRUE),
      p10 = quantile(event_los, 0.1, na.rm = TRUE),
      p25 = quantile(event_los, 0.25, na.rm = TRUE),
      p75 = quantile(event_los, 0.75, na.rm = TRUE),
      p90 = quantile(event_los, 0.9, na.rm = TRUE),

      .by = c(variable, label)
    )

  return(data_los)
}


los_estimates(data_combined, "sex", "covid_admitted_los")

## function to get LoS across all group combinations ----
# for a given los outcome, loop over all groups combinations, obtaining los summaries for each using los_estimates function, and combining into one file
get_all_los_estimates <- function(data, event_name, event_los) {

  estimates_list <-
    level_combos |>
    mutate(
      estimates = map2(
        group1, group2,
        \(group1, group2) {

          data |>
            mutate(
              label1 = data[[group1]],
            ) |>
            nest(.by = c(label1), .key = "group1_subset") |>
            mutate(
              estimates = map(group1_subset, \(group1_subset) {
                los_estimates(group1_subset, group2, event_los)
              })
            ) |>
            select(-group1_subset) |>
            unnest(estimates) |>
            select(-variable) |>
            rename(label2 = label) |>
            mutate(
              across(c(label1, label2), as.character)
            )
        }
      )
    ) |>
    unnest(estimates) |>
    select(group1, label1, group2, label2, everything()) # reorder columns

  write_csv(estimates_list, fs::path(output_dir, glue("los_{event_name}.csv")))

}

## _______________________________________________________________________________________
cat("## Get all LOS values for burden", "\n")
## _______________________________________________________________________________________

get_all_los_estimates(data_combined, "covid_admitted", "covid_admitted_los")
