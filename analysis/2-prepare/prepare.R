# _________________________________________________
# Purpose:
# import vaccination date data extracted by ehrql
# organise vaccination date data to "vax X product", "vax X date" (rather than "pfizer X date", "az X date", ...)
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

# create output directory
output_dir <- here("output", "2-prepare", "prepare")
fs::dir_create(output_dir)
options(width = 200) # set output width for capture.output

# Import and process fixed dataset ----

# Import fixed dataset
data_extract_fixed <- read_feather(here("output", "1-extract", "extract_fixed.arrow"))

stopifnot(
  "inconsistency between ethnicity5 and ethnicity 16" = identical(data_extract_fixed$ethnicity5, ethnicity_16_to_5(data_extract_fixed$ethnicity16))
)

# print details about dataset
capture.output(
  skimr::skim_without_charts(data_extract_fixed),
  file = fs::path(output_dir, "data_extract_fixed_skim.txt"),
  split = FALSE
)

# Process snapshot dataset
data_processed_fixed <-
  data_extract_fixed |>
  lazy_dt() |>
  mutate(
    sex = fct_case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      # sex == "intersex" ~ "Inter-sex",
      # sex == "unknown" ~ "Unknown",
      TRUE ~ NA_character_
    ),
    ethnicity5 = factor(ethnicity5, levels = factor_levels$ethnicity5, ordered = FALSE),
    ethnicity16 = factor(ethnicity16, levels = factor_levels$ethnicity16, ordered = FALSE) |>
      fct_relabel(~ str_extract(.x, "(?<= - )(.*)")), # pick up everything after " - "
  ) |>
  as_tibble()

# print details about dataset
capture.output(
  skimr::skim_without_charts(data_processed_fixed),
  file = fs::path(output_dir, "data_processed_fixed_skim.txt"),
  split = FALSE
)

# save processed fixed dataset
data_processed_fixed |>
  select(
    patient_id,
    sex,
    ethnicity5,
    ethnicity16,
    death_date,
    covid_death_date
  ) |>
  write_feather(fs::path(output_dir, "data_fixed.arrow"))



## delete in-memory objects to save space
rm(data_processed_fixed)
rm(data_extract_fixed)


# Import and process fixed dataset ----

# import
# data_extract_varying <-
#  import_extract(
#    here("lib", "dummydata", "dummyinput_varying.arrow"),
#    here("output", "extracts", "extract_varying.arrow")
#  )

data_extract_varying <- read_feather(here("output", "1-extract", "extract_varying", "dataset.arrow"))

# Reshape vaccination data
data_vax <-
  data_extract_varying |>
  # lazy_dt() |>
  select(
    patient_id,
    matches("covid_vax\\_\\d+\\_date"),
    matches("covid_vax_product_\\d+"),
    matches("registered_\\d+"),
    matches("deregistered_\\d+"),
    matches("age_\\d+"),
    matches("region_\\d+"),
    matches("stp_\\d+"),
    matches("imd_\\d+"),
    matches("imd_quintile_\\d+"),
  ) |>
  pivot_longer(
    cols = -patient_id,
    names_to = c(".value", "vax_index"),
    names_pattern = "^(.*)_(\\d+)",
    # values_drop_na = TRUE, # this causes an error in dtplyr - replace with filter(!is_na(covid_vax))
    # names_transform = list(vax_index = as.integer) # not supported by dtplyr - use vax_index = as.integer(vax_index) in a mutate step
  ) |>
  filter(!is.na(covid_vax)) |>
  mutate(
    vax_index = as.integer(vax_index)
  ) |>
  rename(
    vax_date = covid_vax,
    vax_product = covid_vax_product,
  ) |>
  # as_tibble() |> # insert this here to revert to standard dplyr as `cut` function doesn't work with dtplyr
  mutate(
    !!!standardise_demographic_characteristics,
    vax_campaign = cut(
      vax_date,
      breaks = c(campaign_info$campaign_start_date, study_dates$end_date),
      labels = campaign_info$campaign_label,
      include.lowest = TRUE, right = FALSE
    )
  ) |>
  arrange(patient_id, vax_date) |>
  mutate(
    vax_product_raw = vax_product,
    vax_product = fct_recode(factor(vax_product, vax_product_lookup), !!!vax_product_lookup) |> fct_na_value_to_level("UNMAPPED")
  ) |>
  group_by(patient_id) |>
  mutate(
    vax_interval = as.integer(vax_date - lag(vax_date, 1))
  ) |>
  ungroup()

capture.output(
  skimr::skim_without_charts(data_vax),
  file = fs::path(output_dir, "data_vax_skim.txt"),
  split = FALSE
)

# save dataset with all vaccines
write_feather(data_vax, fs::path(output_dir, "data_vax.arrow"))

# remove vaccinations occurring within 14 days of a previous vaccination
data_vax_clean <-
  # remove vaccine events occurring within 14 days of a previous vaccine event
  data_vax |>
  filter(
    !is.na(vax_date),
    is.na(vax_interval) | vax_interval >= 14,
    vax_date >= study_dates$start_date,
    vax_date <= study_dates$end_date
  ) |>
  group_by(patient_id) |>
  mutate(
    vax_index = row_number()
  ) |>
  ungroup()

capture.output(
  skimr::skim_without_charts(data_vax_clean),
  file = fs::path(output_dir, "data_vax_clean_skim.txt"),
  split = FALSE
)

# save dataset with <14-day vaccines removed
write_feather(data_vax_clean, fs::path(output_dir, "data_vax_clean.arrow"))


# extract event level data for vaccines ----

data_vax_ELD0 <- read_feather(here("output", "1-extract", "extract_varying", "vaccinations.arrow"))

# - remove rows where vaccination date is missing
# - attach info about the campaign during which the vaccination was given
# - collapse exact duplicates (where patient id, date, and product all match)
data_vax_ELD <-
  data_vax_ELD0 |>
  lazy_dt() |>
  arrange(patient_id, vax_date) |>
  filter(!is.na(vax_date)) |>
  # distinct(.keep_all = TRUE) |> # remove exact duplicates # or use
  count(patient_id, vax_date, vax_product, age) |> # or alternatively, capture how many duplicate vaccines there are. This creates a new variable `n` counting the duplicates
  as_tibble() |>
  mutate(
    vax_product_raw = vax_product,
    vax_product = fct_recode(factor(vax_product, vax_product_lookup), !!!vax_product_lookup) |> fct_na_value_to_level("UNMAPPED"),
    campaign = cut(
      vax_date,
      breaks = c(campaign_info$campaign_start_date, as.Date(Inf)),
      labels = campaign_info$campaign_label
    ),
    campaign_start = cut(
      vax_date,
      breaks = c(campaign_info$campaign_start_date, as.Date(Inf)),
      labels = campaign_info$campaign_start_date
    ),
  ) |>
  lazy_dt()


# Test equivalence of ELD extract ----

data_vax_ELD_filtered <-
  data_vax_ELD |>
  filter(vax_date > as.Date("1899-01-01")) |>
  group_by(patient_id) |>
  filter((vax_date != lag(vax_date)) | row_number() == 1) |>
  mutate(vax_index = row_number()) |>
  filter(vax_index <= 16) |>
  ungroup() |>
  as_tibble()

capture.output(
  skimr::skim_without_charts(data_vax_ELD_filtered),
  file = fs::path(output_dir, "data_vax_ELD_skim.txt"),
  split = FALSE
)
write_feather(data_vax_ELD_filtered, fs::path(output_dir, "data_vax_ELD.arrow"))

data_vax_PLD <-
  data_vax |>
  select(patient_id, vax_date, vax_product = vax_product_raw, age, vax_index)

capture.output(
  skimr::skim_without_charts(data_vax_PLD),
  file = fs::path(output_dir, "data_vax_PLD_skim.txt"),
  split = FALSE
)
write_feather(data_vax_PLD, fs::path(output_dir, "data_vax_PLD.arrow"))


# check equality of datasets
cat(
  "\n",
  "are datasets from ELD versus PLD identical after some standardisation? \n"
)

all.equal(data_vax_ELD_filtered, data_vax_PLD)

# report multiple vaccinations on the same day
cat(
  "\n",
  "number of occassions where a person is vaccinated more than once in a day:\n",
  data_vax_ELD |>
    group_by(patient_id, vax_date) |>
    summarise(n = n()) |>
    filter(n > 1) |>
    nrow()
)

# report no vax date
cat(
  "\n",
  "number of occassions where a person is vaccinated with a null date:\n",
  data_vax_ELD |>
    as_tibble() |>
    nrow()
)


# report no vax date
cat(
  "\n",
  "number of occassions where a person is vaccinated on or before 1899:\n",
  data_vax_ELD |>
    filter(vax_date <= as.Date("1899-01-01")) |>
    as_tibble() |>
    nrow()
)
