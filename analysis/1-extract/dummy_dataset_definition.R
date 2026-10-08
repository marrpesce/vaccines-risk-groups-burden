# _________________________________________________
# Purpose:
# Create a dummy dataset for the disease-specific dataset definition
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


cohort_ids <- cohort_info$cohort_id

for (cohort_id in cohort_ids) {

  cohort_row <- cohort_info |>
    filter(cohort_id == .env$cohort_id)

  target <- cohort_row$target
  cohort <- cohort_row$cohort
  snapshot_date <- as.Date(cohort_row$cohort_start_date)
  cohort_end_date <- as.Date(cohort_row$cohort_end_date)

  print(glue("{cohort_id}: {target} {cohort}"))

  snapshot_day <- 0L
  cohort_end_day <- as.integer(cohort_end_date - snapshot_date)

  # set the variables and functions that are known a-priori to the simulation engine
  # ie, defined and accessible outside of the scope of the dataset
  known_variables <- c(
    "snapshot_day",
    "cohort_end_day"
  )


  # define the simulation configuration
  # ie, a list of variables to simulate
  # use the form _variable_name_ = bn_node(~ _formula_for_simulating_variable_, ) see help("bn_node")
  # ..n as a place holder for the length of the variable

  sim_list <- lst(
  # demographics
  sex = bn_node(
    ~ rfactor(n = ..n, levels = c("female", "male", "intersex", "unknown"), p = c(0.51, 0.49, 0, 0))
  ),

  age = bn_node(
    ~ as.integer(rnormt(n = ..n, mean = 60, sd = 14, range = c(12, 104)))
  ),

  ethnicity5 = bn_node(variable_formula = ~ ethnicity_16_to_5(ethnicity16), needs = "ethnicity16"),
  ethnicity16 = bn_node(
    variable_formula = ~ rfactor(
      n = ..n,
      levels = c(
        "White - British",
        "White - Irish",
        "White - Any other White background",
        "Mixed - White and Black Caribbean",
        "Mixed - White and Black African",
        "Mixed - White and Asian",
        "Mixed - Any other mixed background",
        "Asian or Asian British - Indian",
        "Asian or Asian British - Pakistani",
        "Asian or Asian British - Bangladeshi",
        "Asian or Asian British - Any other Asian background",
        "Black or Black British - Caribbean",
        "Black or Black British - African",
        "Black or Black British - Any other Black background",
        "Other Ethnic Groups - Chinese",
        "Other Ethnic Groups - Any other ethnic group"
      ),
      p = c(
        0.5, 0.05, 0.05, # White
        0.025, 0.025, 0.025, 0.025, # Mixed
        0.025, 0.025, 0.025, 0.025, # Asian
        0.033, 0.033, 0.034, # Black
        0.05, 0.05 # Other
      )
    ),
    missing_rate = ~0.1,
  ),

  region = bn_node(
    variable_formula = ~ rfactor(n = ..n, levels = c(
      "North East", "North West", "Yorkshire and The Humber",
      "East Midlands", "West Midlands", "East",
      "London", "South East", "South West"
    ), p = c(0.2, 0.2, 0.3, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05))
  ),

  stp = bn_node(
    ~ factor(as.integer(runif(n = ..n, 1, 36)), levels = 1:36)
  ),

  imd = bn_node(
    ~ as.integer(plyr::round_any(runif(n = ..n, 100, 32000), 100)),
    missing_rate = ~0.05
  ),

  carehome_status = bn_node(
    ~ rbernoulli(n = ..n, p = if_else(age < 65, 0.01, 0.2)),
    needs = "age"
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
  # registration
  deregistered_day = bn_node(
    ~ as.integer(runif(n = ..n, snapshot_day, snapshot_day + 1200)),
    missing_rate = ~0.99
  ),

  # all-cause death
  death_day = bn_node(
    ~ as.integer(runif(n = ..n, snapshot_day, snapshot_day + 3000)),
    missing_rate = ~0.98
  ),

  # disease-specific vaccination variables
  last_vax_date_before_start_day = bn_node(
    ~ runif(n = ..n, snapshot_day - 400, snapshot_day - 1),
    missing_rate = ~0.5
  ),

  last_vax_product_before_start = bn_node(
    ~ rcat(n = ..n, c("prodA", "prodB"), c(0.5, 0.5)),
    needs = "last_vax_date_before_start_day"
  ),

  first_vax_date_after_start_day = bn_node(
    ~ runif(n = ..n, snapshot_day, snapshot_day + 200),
    missing_rate = ~0.5
  ),

  first_vax_product_after_start = bn_node(
    ~ rcat(n = ..n, c("prodA", "prodB"), c(0.5, 0.5)),
    needs = "first_vax_date_after_start_day"
  ),
  # disease-specific severe outcomes
  admitted_day = bn_node(
    ~ as.integer(runif(n = ..n, snapshot_day, snapshot_day + 300)),
    missing_rate = ~0.7
  ),

  admitted_primary_day = bn_node(
    ~ if_else(rbernoulli(n = ..n, p = 0.5) == 1, admitted_day, NA_integer_),
    needs = "admitted_day"
  ),

  disease_death_day = bn_node(
    ~ as.integer(runif(n = ..n, snapshot_day, snapshot_day + 300)),
    missing_rate = ~0.95
  )
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
    mutate(
      cohort_start_date = snapshot_date,
      cohort_end_date = cohort_end_date
    ) %>%
    mutate(across(ends_with("_day"), ~ as.Date(as.character(snapshot_date + .)))) %>%
    rename_with(~ str_replace(., "_day", "_date"), ends_with("_day"))


  # save the dataset in arrow format

  write_feather(dummydata_processed, sink = here("analysis", "1-extract", "dummy-data", glue("dummy_{cohort_id}.arrow")))


}
