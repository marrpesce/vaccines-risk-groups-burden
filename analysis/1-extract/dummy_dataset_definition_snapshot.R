# _________________________________________________
# Purpose:
# Create a dummy dataset for`dataset_definition_fixed.py` script
# _________________________________________________


# Import libraries and functions ----

library("tidyverse")
library("arrow")
library("here")
library("glue")

# remotes::install_github("https://github.com/wjchulme/dd4d") #package for more convenient data simulation
library("dd4d")

# Import custom functions
source(here("analysis", "0-lib", "design.R"))


# truncated normal distribution
rnormt <- function(n, range, mean, sd = 1) {

  F.a <- pnorm(min(range), mean = mean, sd = sd)
  F.b <- pnorm(max(range), mean = mean, sd = sd)

  u <- runif(n, min = F.a, max = F.b)

  qnorm(u, mean = mean, sd = sd)
}


# Define and simulate the dataset ----

# Set the size of the dataset
population_size <- 10000



# set the index date for date variables
# all variables will be defined as the number of days before or after this day
# and then at the end of the script they are transformed into dates
# we do this because some dplyr operations to not preserve date attributes, so dates will be converted to numerics


all_snapshot_dates <- campaign_info |>
  filter(
    campaign_start_date >= as.Date("2020-12-07"),
    campaign_start_date <= as.Date("2026-03-31")
  ) |>
  pull(campaign_start_date)

for (snapshot_date in all_snapshot_dates) {
  # use these if selecting a specific date to run
  # snapshot_date <- as.Date("2020-12-07")
  # snapshot_date <- campaign_info |>  filter(campaign_label == "Spring 2025") |> pull(campaign_start_date)

  snapshot_date <- as.Date(snapshot_date)

  print(snapshot_date)

  snapshot_day <- 0L

  # set the variables and functions that are known a-priori to the simulation engine
  # ie, defined and accessible outside of the scope of the dataset
  known_variables <- c(
    "index_date",
    "snapshot_day"
  )


  # define the simulation configuration
  # ie, a list of variables to simulate
  # use the form _variable_name_ = bn_node(~ _formula_for_simulating_variable_, ) see help("bn_node")
  # ..n as a place holder for the length of the variable

  sim_list <- lst(
    deregistered_day = bn_node(
      ~ as.integer(runif(n = ..n, snapshot_day, snapshot_day + 1200)),
      missing_rate = ~0.99
    ),
    age = bn_node(
      ~ as.integer(rnormt(n = ..n, mean = 60, sd = 14, range = c(12, 104)))
    ),
    eligibility_age = bn_node(
      ~ age + rbernoulli(n = ..n, p = 0.25)
    ),
    imd = bn_node(
      ~ as.integer(plyr::round_any(runif(n = ..n, 100, 32000), 100)),
      missing = ~0.05
    ),
    stp = bn_node(
      ~ factor(as.integer(runif(n = ..n, 1, 36)), levels = 1:36)
    ),
    region = bn_node(
      variable_formula = ~ rfactor(n = ..n, levels = c(
        "North East",
        "North West",
        "Yorkshire and The Humber",
        "East Midlands",
        "West Midlands",
        "East",
        "London",
        "South East",
        "South West"
      ), p = c(0.2, 0.2, 0.3, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05))
    ),
    carehome_status = bn_node(
      ~ rbernoulli(n = ..n, p = if_else(age < 65, 0.01, 0.2)),
    ),
    # PRIMIS
    crd = bn_node(
      ~ rbernoulli(n = ..n, p = 0.02),
    ),
    chd = bn_node(
      ~ rbernoulli(n = ..n, p = 0.02),
    ),
    ckd = bn_node(
      ~ rbernoulli(n = ..n, p = 0.02),
    ),
    cld = bn_node(
      ~ rbernoulli(n = ..n, p = 0.02),
    ),
    cns = bn_node(
      ~ rbernoulli(n = ..n, p = 0.02),
    ),
    learndis = bn_node(
      ~ rbernoulli(n = ..n, p = 0.02),
    ),
    diabetes = bn_node(
      ~ rbernoulli(n = ..n, p = 0.02),
    ),
    immunosuppressed = bn_node(
      ~ rbernoulli(n = ..n, p = 0.02),
    ),
    asplenia = bn_node(
      ~ rbernoulli(n = ..n, p = 0.02),
    ),
    severe_obesity = bn_node(
      ~ rbernoulli(n = ..n, p = 0.02),
    ),
    smi = bn_node(
      ~ rbernoulli(n = ..n, p = 0.02),
    ),
    primis_atrisk = bn_node(
      ~ crd | chd | ckd | cld | cns | learndis | diabetes | immunosuppressed | asplenia | severe_obesity | smi,
    ),
    # extended subgroups
    rrt_cat = bn_node(
      variable_formula = ~ rfactor(n = ..n, levels = c(
        "0 no RRT",
        "1 dialysis",
        "2 transplant"),
      p = c(0.98, 0.01, 0.01)
      )),
    ckd_stage_3to5 = bn_node(
      variable_formula = ~ rfactor(n = ..n, levels = c(
        "no CKD",
        "3",
        "4",
        "5",
        "CKD, without stage 3-5 code"),
      p = c(0.90, 0.06, 0.02, 0.01, 0.01)
      )),
    # creatinine_umol = bn_node(
    #   ~ as.numeric(runif(n = ..n, 20.0, 3000.0)),
    #   missing_rate = ~0.60
    # ),
    # creatinine_age = bn_node(
    #   ~ as.integer(rnorm(n = ..n, mean = 60, sd = 14))
    copd = bn_node(
      ~ rbernoulli(n = ..n, p = 0.02),
    ),
    learndis_cat = bn_node(
      variable_formula = ~ rfactor(n = ..n, levels = c(
        "No learning disability",
        "Down's syndrome",
        "Other learning disability",
        "Learning disability register"),
      p = c(0.80, 0.05, 0.1, 0.05)
      )),
    sickle_cell = bn_node(
      ~ rbernoulli(n = ..n, p = 0.02),
    ),
    cirrhosis = bn_node(
      ~ rbernoulli(n = ..n, p = 0.02),
    ),
    cochlear_implant = bn_node(
      ~ rbernoulli(n = ..n, p = 0.02),
    ),
    cystic_fibrosis = bn_node(
      ~ rbernoulli(n = ..n, p = 0.02),
    ),
    csfl = bn_node(
      ~ rbernoulli(n = ..n, p = 0.02),
    ),
    homeless = bn_node(
      ~ rbernoulli(n = ..n, p = 0.02),
    ),
    # covid vaccines
    covid_vax_1_day = bn_node(
      ~ runif(n = ..n, snapshot_day, snapshot_day + 200),
      missing_rate = ~0.2,
    ),
    covid_vax_2_day = bn_node(
      ~ runif(n = ..n, covid_vax_1_day + 14, covid_vax_1_day + 400),
      missing_rate = ~0.95,
      needs = "covid_vax_1_day"
    ),
    covid_vax_1_product = bn_node(~ rcat(n = ..n, c("pfizer", "az", "moderna"), c(0.5, 0.3, 0.2)), needs = "covid_vax_1_day"),
    covid_vax_2_product = bn_node(~ if_else(runif(..n) < 0.98, covid_vax_1_product, "pfizer"), needs = "covid_vax_2_day"),

    covid_vax_prior_1_day = bn_node(
      ~ runif(n = ..n, snapshot_day - 400, snapshot_day - 200),
      missing_rate = ~0.1,
      needs = "covid_vax_1_day"
    ),
    covid_vax_prior_2_day = bn_node(
      ~ runif(n = ..n, covid_vax_prior_1_day - 400, covid_vax_prior_1_day - 200),
      missing_rate = ~0.1,
      needs = "covid_vax_prior_1_day"
    ),
    covid_vax_prior_3_day = bn_node(
      ~ runif(n = ..n, covid_vax_prior_2_day - 400, covid_vax_prior_2_day - 200),
      missing_rate = ~0.1,
      needs = "covid_vax_prior_2_day"
    ),

    covid_vax_prior_1_product = bn_node(~ rcat(n = ..n, c("pfizer", "az", "moderna"), c(0.5, 0.3, 0.2)), needs = "covid_vax_prior_1_day"),
    covid_vax_prior_2_product = bn_node(~ rcat(n = ..n, c("pfizer", "az", "moderna"), c(0.5, 0.3, 0.2)), needs = "covid_vax_prior_2_day"),
    covid_vax_prior_3_product = bn_node(~ rcat(n = ..n, c("pfizer", "az", "moderna"), c(0.5, 0.3, 0.2)), needs = "covid_vax_prior_3_day"),

    covid_vax_prior_count = bn_node(~ as.integer(runif(n = ..n, 0, 15))), # in  dummy data, this will not match total vax count in "time-varying" dataset, but that's ok

    covid_admitted_day = bn_node(
      ~ as.integer(runif(n = ..n, snapshot_day, snapshot_day + 300)),
      missing_rate = ~0.7
    ),

    covid_admitted_los = bn_node(
      ~ as.integer(runif(n = ..n, 1, 10)),
      needs = "covid_admitted_day"
    ),

    covid_admitted_primary_day = bn_node(
      ~ if_else(rbernoulli(n = ..n, p = 0.5) == 1, covid_admitted_day, NA_integer_)
    ),

    covid_critcare_day = bn_node(
      ~covid_admitted_day,
      needs = "covid_admitted_day",
      missing_rate = ~0.7
    ),
  )

  # check and create the simulation object, including all dependencies, topological orders, etc
  bn <- bn_create(sim_list, known_variables = known_variables)

  # plot the network
  bn_plot(bn)

  # plot the network (connected nodes only)
  bn_plot(bn, connected_only = TRUE)

  # set the seed for the simulation
  set.seed(10)

  # simulate the dataset
  dummydata <- bn_simulate(bn, pop_size = population_size, keep_all = FALSE, .id = "patient_id")

  # do some post simulation processing for features that are not easily handled by the simulation configuration
  dummydata_processed <- dummydata %>%
    # convert integer days to dates since index date and rename vars
    mutate(across(ends_with("_day"), ~ as.Date(as.character(snapshot_date + .)))) %>%
    rename_with(~ str_replace(., "_day", "_date"), ends_with("_day"))


  # save the dataset in arrow format

  snapshot_date_compact <- format(snapshot_date, "%Y%m%d")
  write_feather(dummydata_processed, sink = here("analysis", "1-extract", "dummy-data", glue("dummy_snapshot_{snapshot_date_compact}.arrow")))


}
