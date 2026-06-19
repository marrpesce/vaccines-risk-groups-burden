# _________________________________________________
# Purpose:
# Create a dummy dataset for `dataset_definition_varying.py` script
# column set 1 = details of first vaccination event
# column set 2 = details of second vaccination event
# and so on
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

# Define and simulate the dataset ----

# Set the size of the dataset
population_size <- 1000

# set the index date for date variables
# all variables will be defined as the number of days before or after this day
# and then at the end of the script they are transformed into dates
# we do this because some dplyr operations to not preserve date attributes, so dates will be converted to numerics

# choose the june 2020 to pick up possible trial participants, but definitely exclude errors or unknown dates (1900-01-01, 2018-03-24, etc)
index_date <- study_dates$firstpossiblevax_date

index_day <- 0L


start_day <- as.integer(study_dates$start_date - study_dates$index_date)

# set the variables that are known a-priori to the simulation engine
# ie, defined ad accessible outside of the scope of the dataset
known_variables <- c(
  "index_date", "start_date",
  "index_day",  "start_day"
)

# define the simulation configuration
# ie, a list of variables to simulate
# use the form _variable_name_ = bn_node(~ _formula_for_simulating_variable_, ) see help("bn_node")
# ..n as a place holder for the length of the variable



# define the list of vaccination-related variables to extract for each vaccination event
# this is essentially a date-product pair for up to the Nth vaccination
# it uses some very roughly realistic date ranges and product types
sim_list_vax_info <- lst(

  ## vaccination variables

  # covid vax any
  covid_vax_1_day = bn_node(~ runif(n = ..n, start_day, start_day + 365),
    missing_rate = ~0.05, ),
  covid_vax_2_day = bn_node(
    ~ runif(n = ..n, covid_vax_1_day + 14, covid_vax_1_day + 100),
    missing_rate = ~0.001,
    needs = "covid_vax_1_day"
  ),
  covid_vax_3_day = bn_node(
    ~ runif(n = ..n, covid_vax_2_day + 250, covid_vax_2_day + 350),
    missing_rate = ~0.3,
    needs = "covid_vax_2_day"
  ),
  covid_vax_4_day = bn_node(
    ~ runif(n = ..n, covid_vax_3_day + 300, covid_vax_3_day + 400),
    missing_rate = ~0.7,
    needs = "covid_vax_3_day"
  ),
  covid_vax_5_day = bn_node(
    ~ runif(n = ..n, covid_vax_4_day + 300, covid_vax_4_day + 400),
    missing_rate = ~0.9,
    needs = "covid_vax_4_day"
  ),
  covid_vax_6_day = bn_node(
    ~ runif(n = ..n, covid_vax_5_day + 300, covid_vax_5_day + 400),
    missing_rate = ~0.99,
    needs = "covid_vax_5_day"
  ),
  covid_vax_7_day = bn_node(
    ~ runif(n = ..n, covid_vax_6_day + 300, covid_vax_6_day + 400),
    missing_rate = ~0.99,
    needs = "covid_vax_6_day"
  ),
  covid_vax_8_day = bn_node(
    ~ runif(n = ..n, covid_vax_7_day + 300, covid_vax_7_day + 400),
    missing_rate = ~0.99,
    needs = "covid_vax_7_day"
  ),
  covid_vax_9_day = bn_node(
    ~ runif(n = ..n, covid_vax_8_day + 300, covid_vax_8_day + 400),
    missing_rate = ~0.99,
    needs = "covid_vax_8_day"
  ),
  covid_vax_10_day = bn_node(
    ~ runif(n = ..n, covid_vax_9_day + 300, covid_vax_9_day + 400),
    missing_rate = ~0.99,
    needs = "covid_vax_9_day"
  ),
  covid_vax_11_day = bn_node(
    ~ runif(n = ..n, covid_vax_10_day + 300, covid_vax_10_day + 400),
    missing_rate = ~0.99,
    needs = "covid_vax_10_day"
  ),
  covid_vax_12_day = bn_node(
    ~ runif(n = ..n, covid_vax_11_day + 300, covid_vax_11_day + 400),
    missing_rate = ~0.99,
    needs = "covid_vax_11_day"
  ),
  covid_vax_13_day = bn_node(
    ~ runif(n = ..n, covid_vax_12_day + 300, covid_vax_12_day + 400),
    missing_rate = ~0.99,
    needs = "covid_vax_12_day"
  ),
  covid_vax_14_day = bn_node(
    ~ runif(n = ..n, covid_vax_13_day + 300, covid_vax_13_day + 400),
    missing_rate = ~0.99,
    needs = "covid_vax_13_day"
  ),
  covid_vax_15_day = bn_node(
    ~ runif(n = ..n, covid_vax_14_day + 300, covid_vax_14_day + 400),
    missing_rate = ~0.99,
    needs = "covid_vax_14_day"
  ),
  covid_vax_16_day = bn_node(
    ~ runif(n = ..n, covid_vax_15_day + 300, covid_vax_15_day + 400),
    missing_rate = ~0.99,
    needs = "covid_vax_15_day"
  ),
  covid_vax_product_1 = bn_node(~ rcat(n = ..n, c("pfizer", "az"), c(0.5, 0.5)), needs = "covid_vax_1_day"),
  covid_vax_product_2 = bn_node(~ if_else(runif(..n) < 0.98, covid_vax_product_1, "az"), needs = "covid_vax_2_day"),
  covid_vax_product_3 = bn_node(~ rcat(n = ..n, c("pfizer", "moderna"), c(0.5, 0.5)), needs = "covid_vax_3_day"),
  covid_vax_product_4 = bn_node(~ rcat(n = ..n, c("pfizer", "moderna"), c(0.5, 0.5)), needs = "covid_vax_4_day"),
  covid_vax_product_5 = bn_node(~ rcat(n = ..n, c("pfizer", "moderna"), c(0.5, 0.5)), needs = "covid_vax_5_day"),
  covid_vax_product_6 = bn_node(~ rcat(n = ..n, c("pfizer", "moderna"), c(0.5, 0.5)), needs = "covid_vax_6_day"),
  covid_vax_product_7 = bn_node(~ rcat(n = ..n, c("pfizer", "moderna"), c(0.5, 0.5)), needs = "covid_vax_7_day"),
  covid_vax_product_8 = bn_node(~ rcat(n = ..n, c("pfizer", "moderna"), c(0.5, 0.5)), needs = "covid_vax_8_day"),
  covid_vax_product_9 = bn_node(~ rcat(n = ..n, c("pfizer", "moderna"), c(0.5, 0.5)), needs = "covid_vax_9_day"),
  covid_vax_product_10 = bn_node(~ rcat(n = ..n, c("pfizer", "moderna"), c(0.5, 0.5)), needs = "covid_vax_10_day"),
  covid_vax_product_11 = bn_node(~ rcat(n = ..n, c("pfizer", "moderna"), c(0.5, 0.5)), needs = "covid_vax_11_day"),
  covid_vax_product_12 = bn_node(~ rcat(n = ..n, c("pfizer", "moderna"), c(0.5, 0.5)), needs = "covid_vax_12_day"),
  covid_vax_product_13 = bn_node(~ rcat(n = ..n, c("pfizer", "moderna"), c(0.5, 0.5)), needs = "covid_vax_13_day"),
  covid_vax_product_14 = bn_node(~ rcat(n = ..n, c("pfizer", "moderna"), c(0.5, 0.5)), needs = "covid_vax_14_day"),
  covid_vax_product_15 = bn_node(~ rcat(n = ..n, c("pfizer", "moderna"), c(0.5, 0.5)), needs = "covid_vax_15_day"),
  covid_vax_product_16 = bn_node(~ rcat(n = ..n, c("pfizer", "moderna"), c(0.5, 0.5)), needs = "covid_vax_16_day"),
)


# define a function to create a sublist containing all demographic / clinical variables to extract for the nth vaccination event
# function will be iterated over first second third ... Nth vaccines
sim_list_varying_i <- function(i) {
  vax_variable <- glue("covid_vax_{i}_day")

  lst(
    "deregistered_{i}_day" := bn_node(
      ~ as.integer(runif(n = ..n, index_day, index_day + 1200)),
      missing_rate = ~0.99,
      needs = vax_variable
    ),
    "age_{i}" := bn_node(~ as.integer(rnorm(
      n = ..n, mean = 60, sd = 14
    )),
    needs = vax_variable),
    "imd_{i}" := bn_node(
      ~ as.integer(plyr::round_any(runif(n = ..n, 100, 32000), 100)),
      missing = ~0.05,
      needs = vax_variable
    ),
    "registered_{i}" := bn_node(~ rbernoulli(n = ..n, p = 0.99),
      needs = vax_variable),
    "stp_{i}" := bn_node(~ factor(as.integer(
      runif(n = ..n, 1, 36)
    ), levels = 1:36),
    needs = vax_variable),
    "region_{i}" := bn_node(
      variable_formula = ~ rfactor(
        n = ..n,
        levels = c(
          "North East",
          "North West",
          "Yorkshire and The Humber",
          "East Midlands",
          "West Midlands",
          "East",
          "London",
          "South East",
          "South West"
        ),
        p = c(0.2, 0.2, 0.3, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05)
      ),
      needs = vax_variable
    ),
    # PRIMIS
    "crd_{i}" := bn_node(~ rbernoulli(n = ..n, p = 0.02),
      needs = vax_variable),
    # chronic respiratory disease
    "chd_{i}" := bn_node(~ rbernoulli(n = ..n, p = 0.02),
      needs = vax_variable),
    # chronic heart disease
    "ckd_{i}" := bn_node(~ rbernoulli(n = ..n, p = 0.02),
      needs = vax_variable),
    # chronic kidney disease
    "cld_{i}" := bn_node(~ rbernoulli(n = ..n, p = 0.02),
      needs = vax_variable),
    # chronic liver disease
    "cns_{i}" := bn_node(~ rbernoulli(n = ..n, p = 0.02),
      needs = vax_variable),
    # chronic neurological disease
    "learndis_{i}" := bn_node(~ rbernoulli(n = ..n, p = 0.02),
      needs = vax_variable),
    # learning Disability
    "diabetes_{i}" := bn_node(~ rbernoulli(n = ..n, p = 0.02),
      needs = vax_variable),
    # diabetes
    "immunosuppressed_{i}" := bn_node(~ rbernoulli(n = ..n, p = 0.02),
      needs = vax_variable),
    # immunosuppress grouped
    "asplenia_{i}" := bn_node(~ rbernoulli(n = ..n, p = 0.02),
      needs = vax_variable),
    # asplenia or Dysfunction of the Spleen
    "severe_obesity_{i}" := bn_node(~ rbernoulli(n = ..n, p = 0.02),
      needs = vax_variable),
    # immunosuppress grouped
    "smi_{i}" := bn_node(~ rbernoulli(n = ..n, p = 0.02),
      needs = vax_variable),
    # severe mental illness
    "primis_atrisk_{i}" := bn_node(~ rbernoulli(n = ..n, p = 0.02),
      needs = vax_variable),
    # clinically vulnerable

    # extended subgroups
    "rrt_{i}" := bn_node(
      variable_formula = ~ rfactor(
        n = ..n,
        levels = c("0",
          "1",
          "2"),
        p = c(0.98, 0.01, 0.01)
      ),
      needs = vax_variable
    ),
    "ckd_stage_3to5_{i}" := bn_node(
      variable_formula = ~ rfactor(
        n = ..n,
        levels = c("no ckd", "3", "4", "5", "ckd, without ckd3-5 code"),
        p = c(0.90, 0.06, 0.02, 0.01, 0.01)
      ),
      needs = vax_variable),

    # "creatinine_umol_{i}" := bn_node(
    #   ~ as.numeric(runif(n = ..n, 20.0, 3000.0)),
    #   missing_rate = ~ 0.60,
    #   needs = vax_variable
    # ),
    # "creatinine_age_{i}" := bn_node(
    #   ~ as.integer(rnorm(n = ..n, mean = 60, sd = 14)),
    #   needs = vax_variable
    # ),
    "copd_{i}" := bn_node(~ rbernoulli(n = ..n, p = 0.02),
      needs = vax_variable),
    "learndis_cat_{i}" := bn_node(
      variable_formula = ~ rfactor(
        n = ..n,
        levels = c("No learning disability", "Down’s syndrome", "Other learning disability", "Learning disability register"),
        p = c(0.80, 0.05, 0.10, 0.05)),
      needs = vax_variable),
    "sickle_cell_{i}" := bn_node(~ rbernoulli(n = ..n, p = 0.02),
      needs = vax_variable),
    "cochlear_implant_{i}" := bn_node(~ rbernoulli(n = ..n, p = 0.02),
      needs = vax_variable),

    "cystic_fibrosis_{i}" := bn_node(~ rbernoulli(n = ..n, p = 0.02),
      needs = vax_variable),

    "csfl_{i}" := bn_node(~ rbernoulli(n = ..n, p = 0.02),
      needs = vax_variable),
  )
}


# compose simulation list for all variables of interest for each vaccination event
sim_list_varying <- splice(
  sim_list_varying_i(1),
  sim_list_varying_i(2),
  sim_list_varying_i(3),
  sim_list_varying_i(4),
  sim_list_varying_i(5),
  sim_list_varying_i(6),
  sim_list_varying_i(7),
  sim_list_varying_i(8),
  sim_list_varying_i(9),
  sim_list_varying_i(10),
  sim_list_varying_i(11),
  sim_list_varying_i(12),
  sim_list_varying_i(13),
  sim_list_varying_i(14),
  sim_list_varying_i(15),
  sim_list_varying_i(16)
)


# put vax info and other info together in the same list
sim_list <- splice(sim_list_vax_info,
  sim_list_varying)

# check and create the simulation object, including all dependencies, topological orders, etc
bn <- bn_create(sim_list, known_variables = known_variables)

# plot the network
bn_plot(bn)

# plot the network (connected nodes only)
bn_plot(bn, connected_only = TRUE)

# set the seed for the simulation
set.seed(10)

# simulate the dataset
dummydata <-
  bn_simulate(bn,
    pop_size = population_size,
    keep_all = FALSE,
    .id = "patient_id")

# do some post simulation processing for features that are not easily handled by the simulation configuration
dummydata_processed <- dummydata %>%
  # convert integer days to dates since index date and rename vars
  mutate(across(ends_with("_day"), ~ as.Date(as.character(index_date + .)))) %>%
  rename_with(~ str_replace(., "_day", "_date"), ends_with("_day")) %>%
  # convert vaccine product short names to full product names
  mutate(across(
    starts_with("covid_vax_product_"),
    ~ factor(
      .,
      levels = names(vax_product_lookup),
      labels = unname(vax_product_lookup)
    )
  ))

# save the datasetin arrow format
write_feather(
  dummydata_processed,
  sink = here("analysis", "1-extract", "dummy-data", "dummy_varying.arrow")
)
