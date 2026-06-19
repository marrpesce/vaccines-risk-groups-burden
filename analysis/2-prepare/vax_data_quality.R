# _________________________________________________
# Purpose:
# import vaccination date data extracted by ehrql
# report data quality for event-level vaccination data
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
output_dir <- here("output", "2-prepare", "vax_data_quality")
fs::dir_create(output_dir)
options(width = 200) # set output width for capture.output


# extract event level data for vaccines ----

data_vax_ELD0 <- read_feather(here("output", "1-extract", "extract_varying", "vaccinations.arrow"))


# when running locally, create some dummy data to make outputs more sensible
# this should NOT EVER be run on the real data, as it will overwrite real data
if (localrun) {
  data_vax_ELD0 <-
    data_vax_ELD0 |>
    mutate(
      vax_date = as.Date(runif(n(), as.Date("2019-01-12"), max(campaign_info$early_milestone_date))),
      vax_product = sample(vax_product_lookup, n(), replace = TRUE)
    )
}

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

# report any unmapped product names
# and stop if there are any
unmapped_products <- data_vax_ELD |> filter(vax_product %in% "UNMAPPED") |> pull(vax_product_raw) |> unique()
cat("Unmapped product names: \n")
cat(paste0(unmapped_products, collapse = "\n"))
stopifnot("There are unmapped product names" = length(unmapped_products) == 0)

## count products ----

# count the number of vaccines of each type over all time, by age (over / under 12 years)

# overall
count_product <-
  data_vax_ELD |>
  mutate(
    under12 = age < 12,
    vax_date_onorafter20201201 = if_else(vax_date >= as.Date("2020-12-01"), vax_date, as.Date(NA))
  ) |>
  group_by(under12, vax_product) |>
  summarise(
    count_total = roundmid_any(n(), sdc_threshold),
    count_before20200101 = roundmid_any(sum(vax_date < as.Date("2020-01-01")), sdc_threshold),
    count_onorafter20200101 = roundmid_any(sum(vax_date >= as.Date("2020-01-01")), sdc_threshold),
    count_onorafter20201201 = roundmid_any(sum(vax_date >= as.Date("2020-12-01")), sdc_threshold),
    earliest_date_onorafter20201201 = min(vax_date_onorafter20201201, na.rm = TRUE),
    count_on_earlier_date = sum(vax_date_onorafter20201201 %in% min(vax_date_onorafter20201201, na.rm = TRUE))
  ) |>
  as_tibble()

write_csv(count_product, fs::path(output_dir, "count_product.csv"))

# count the number of vaccines of each type, by age (over or under 12 years) and by campaign

count_product_campaign <-
  data_vax_ELD |>
  mutate(
    under12 = age < 12
  ) |>
  group_by(under12, campaign, vax_product) |>
  summarise(
    count_total = roundmid_any(n(), sdc_threshold),
    earliest_date = min(vax_date),
    count_on_earliest_date = sum(vax_date %in% min(vax_date))
  ) |>
  as_tibble()

write_csv(count_product_campaign, fs::path(output_dir, "count_product_campaign.csv"))



## count product same-day co-occurrence ----

# summary function to turn eg c("A", "A", "B", "C", "C", "C") into "2x A -- 1x B -- 3x C"
flat_table_chr <- function(x, collapse = NULL) {
  rle_obj <- rle(x)
  paste0(rle_obj$length, "x ", rle_obj$values, collapse = collapse)
}

products_cooccurrence_flat <-
  data_vax_ELD |>
  filter(age >= 12) |>
  arrange(vax_product) |>
  group_by(patient_id, vax_date, campaign) |>
  summarise(
    # vax_product = flat_table_chr(as.character(vax_product), collapse = "  --AND-- "), # this is only useful if we don't remove duplicate rows (same patient, same date, same product)
    vax_product = paste0(paste0(n, "x ", as.character(vax_product)), collapse = "  --AND-- "),
  )


# CONSIDER USING THIS FORMAT INSTEAD, IF EASIER!

# products_cooccurrence_wide <-
#   data_vax_ELD |>
#   filter(age >= 12) |>
#   pivot_wider(
#     id_cols = c(patient_id, vax_date, campaign),
#     names_from = vax_product,
#     values_from = n,
#     values_fill = 0L
#   ) |> as_tibble()


# count overall

count_products_cooccurrence <-
  products_cooccurrence_flat |>
  mutate(
    vax_date_onorafter20201201 = if_else(vax_date >= as.Date("2020-12-01"), vax_date, as.Date(NA))
  ) |>
  group_by(vax_product) |>
  summarise(
    count_total = roundmid_any(n(), sdc_threshold),
    count_before20200101 = roundmid_any(sum(vax_date < as.Date("2020-01-01")), sdc_threshold),
    count_onorafter20200101 = roundmid_any(sum(vax_date >= as.Date("2020-01-01")), sdc_threshold),
    count_onorafter20201201 = roundmid_any(sum(vax_date >= as.Date("2020-12-01")), sdc_threshold),
    earliest_date_onorafter20201201 = min(vax_date_onorafter20201201, na.rm = TRUE),
    count_on_earliest_date = sum(vax_date_onorafter20201201 %in% min(vax_date_onorafter20201201, na.rm = TRUE))
  ) |>
  as_tibble()

write_csv(count_products_cooccurrence, fs::path(output_dir, "count_product_cooccurrence.csv"))

# count by campaign

count_products_cooccurrence_campaign <-
  products_cooccurrence_flat |>
  group_by(vax_product, campaign) |>
  summarise(
    count_total = roundmid_any(n(), sdc_threshold),
    count_before20200101 = roundmid_any(sum(vax_date < as.Date("2020-01-01")), sdc_threshold),
    count_onorafter20200101 = roundmid_any(sum(vax_date >= as.Date("2020-01-01")), sdc_threshold),
    count_onorafter20201201 = roundmid_any(sum(vax_date >= as.Date("2020-12-01")), sdc_threshold),
    earliest_date = min(vax_date, na.rm = TRUE),
    count_on_earliest_date = sum(vax_date %in% min(vax_date, na.rm = TRUE))
  ) |>
  as_tibble()

write_csv(count_products_cooccurrence_campaign, fs::path(output_dir, "count_product_cooccurrence_campaign.csv"))
