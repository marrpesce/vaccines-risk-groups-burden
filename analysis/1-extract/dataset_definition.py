##########################
# purpose:
# extract for a given cohort
# - patient-level baseline demographic + clinical variables 
# - disease-specific vaccination variables, and 
# - disease-specific outcomes 
# - death date and deregistration
# for a given cohort
##########################

from json import loads
from pathlib import Path
from datetime import date

from ehrql import (
    get_parameter,
    create_dataset,
    days,
    weeks,
)

from ehrql.tables.tpp import (
    patients,
    practice_registrations,
    ons_deaths,
  #  apcs,
  #  vaccinations,
)

import codelists
from variables_function import *


# ------------------------------------------------------------------------------
# Cohort parameters
# ------------------------------------------------------------------------------

cohort_id = get_parameter(name="cohort_id")

all_cohort_info = loads(
    Path("analysis/0-lib/cohort_info.json").read_text()
)

cohort_info = all_cohort_info[cohort_id]

target = cohort_info["target"]
cohort_start_date = date.fromisoformat(cohort_info["cohort_start_date"])
cohort_end_date = date.fromisoformat(cohort_info["cohort_end_date"])

# Age calculated the day before cohort start
age_calculation_date = cohort_start_date - days(1)


# ------------------------------------------------------------------------------
# Initialise dataset
# ------------------------------------------------------------------------------

dataset = create_dataset()
dataset.configure_dummy_data(population_size=1000)


# ------------------------------------------------------------------------------
# Population definition
# ------------------------------------------------------------------------------

registered_patients = practice_registrations.for_patient_on(cohort_start_date)

registered = registered_patients.exists_for_patient()
registered_start_date = registered_patients.start_date

alive_ONS = (ons_deaths.date > cohort_start_date) | ons_deaths.date.is_null()
alive_GP = (patients.date_of_death > cohort_start_date) | patients.date_of_death.is_null()
alive = alive_ONS & alive_GP

eligibility_age = patients.age_on(age_calculation_date)

dataset.define_population(
    registered
    & (registered_start_date <= (cohort_start_date - weeks(12)))
    & alive
    & (eligibility_age >= 12)
    & (eligibility_age <= 104)
    & patients.sex.is_in(["male", "female"])
)


# ------------------------------------------------------------------------------
# Baseline variables
# ------------------------------------------------------------------------------

demographic_variables(dataset=dataset, index_date=cohort_start_date)

primis_variables(dataset=dataset, index_date=cohort_start_date)

extended_subgroups(dataset=dataset, index_date=cohort_start_date)

# Registration end date for the registration active at cohort start
dataset.deregistered_date = registered_patients.end_date

# All-cause death date
# TODO: include ehr date?
dataset.death_date = ons_deaths.date


# ------------------------------------------------------------------------------
# Disease-specific variables: vaccination + outcomes
# ------------------------------------------------------------------------------
# TODO: add mild outcomes
if target == "COVID-19":
    add_target_vaccines(dataset, ["SARS-2 CORONAVIRUS"], cohort_start_date, cohort_end_date)
    add_target_sev_outcomes(dataset, codelists.covid_icd10, cohort_start_date, cohort_end_date)

elif target == "Influenza":
    add_target_vaccines(dataset, ["INFLUENZA"], cohort_start_date, cohort_end_date)
    add_target_sev_outcomes(dataset, codelists.flu_icd10, cohort_start_date, cohort_end_date)

elif target == "RSV":
    add_target_vaccines(dataset, ["HUMAN RESPIRATORY SYNCYTIAL VIRUS"], cohort_start_date, cohort_end_date)
    add_target_sev_outcomes(dataset, codelists.rsv_icd10, cohort_start_date, cohort_end_date)

#TODO: Zoster

else:
    raise ValueError(f"Unknown target: {target}")