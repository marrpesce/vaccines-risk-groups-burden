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

# Define and simulate the dataset ----

# Set the size of the dataset
population_size <- 10000

# set the index date for date variables
# all variables will be defined as the number of days before or after this day
# and then at the end of the script they are transformed into dates
# we do this because some dplyr operations to not preserve date attributes, so dates will be converted to numerics
index_date <- study_dates$end_date

index_day <- 0L

# set the variables and functions that are known a-priori to the simulation engine
# ie, defined and accessible outside of the scope of the dataset
known_variables <- c(
  "index_date",
  "index_day"
)


# define the simulation configuration
# ie, a list of variables to simulate
# use the form _variable_name_ = bn_node(~ _formula_for_simulating_variable_, ) see help("bn_node")
# ..n as a place holder for the length of the variable



sim_list <- lst(
  sex = bn_node(
    ~ rfactor(n = ..n, levels = c("female", "male", "intersex", "unknown"), p = c(0.51, 0.49, 0, 0))
    # missing_rate = ~0.001 # this is shorthand for ~(rbernoulli(n=..n, p = 0.2))
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
  death_day = bn_node(
    ~ as.integer(runif(n = ..n, index_day, index_day + 3000)),
    missing_rate = ~0.98
  ),

  covid_death_day = bn_node(
    ~death_day,
    missing_rate = ~0.7,
    needs = "death_day"
  ),
  covid_vax_count = bn_node(~ as.integer(runif(n = ..n, 0, 15))) # in  dummy data, this will not match total vax count in "time-varying" dataset, but that's ok
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
  mutate(across(ends_with("_day"), ~ as.Date(as.character(index_date + .)))) %>%
  rename_with(~ str_replace(., "_day", "_date"), ends_with("_day"))


# save the datasetin arrow format
write_feather(dummydata_processed, sink = here("analysis", "1-extract", "dummy-data", "dummy_fixed.arrow"))
