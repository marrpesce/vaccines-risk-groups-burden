# select most recent vaccine _before_ snapshot date, and summarise
# ignore this for now as we're extracting most recent 3 vaccines and next 2 vaccines in the snapshot extract itself
data_last_vax_date_clean <-
  data_vax_clean |>
  lazy_dt() |>
  filter(vax_date < snapshot_date ) |>
  group_by(patient_id) |>
  filter(vax_index == max(vax_index, na.rm=TRUE)) |>
  ungroup() |>
  transmute(
    patient_id,
    vax_count = vax_index,
    last_vax_date = vax_date,
    last_vax_product = vax_product,
    days_since_vax = snapshot_date - last_vax_date,
  )

rm(data_vax_clean)

# check there's only one patient per row:
check_last_1rpp <-
  data_last_vax_date_clean %>%
  group_by(patient_id) %>%
  filter(row_number() != 1) %>%
  as_tibble()
stopifnot("data_last_vax_date_clean should not have multiple rows per patient" = nrow(check_last_1rpp) == 0)


# select earliest vaccine _after_ snapshot date and before next campaign, and summarise
data_next_vax_date_clean <-
  data_vax_clean |>
  lazy_dt() |>
  group_by(patient_id) |>
  filter(vax_date >= snapshot_date &  vax_date < next_campaign_date) %>%
  group_by(patient_id) |>
  filter(vax_index == min(vax_index, na.rm=TRUE)) %>%
  ungroup() |>
  transmute(
    patient_id,
    vax_date,s
    vax_product,
  )

# check there's only one patient per row:
check_next_1rpp <-
  data_next_vax_date_clean %>%
  group_by(patient_id) %>%
  filter(row_number() != 1) %>%
  as_tibble()
stopifnot("data_next_vax_date_clean should not have multiple rows per patient" = nrow(check_next_1rpp) == 0)

rm(data_vax_clean)
