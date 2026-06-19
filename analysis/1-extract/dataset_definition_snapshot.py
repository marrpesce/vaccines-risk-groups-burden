
##########################
# extract patient information as at a fixed date
# vaccination dates and info will be joined onto this later using the "varying" dataset
##########################

# import libraries and functions

from json import loads
from pathlib import Path
from datetime import datetime

from ehrql import (
#    show,
    get_parameter,
#    case,
    create_dataset,
    days,
    weeks,
#    when,
#    minimum_of,
#    maximum_of
)
from ehrql.tables.tpp import (
  patients,
  practice_registrations, 
  apcs,
  ons_deaths,
)

# import codelists
from codelists import *

from variables_function import *

# get arguments supplied in the yaml file, stored in argv[1], argv[2], etc
snapshot_date_string = get_parameter(name="snapshot_date")
snapshot_date = datetime.strptime(get_parameter(name="snapshot_date"), '%Y%m%d').strftime('%Y-%m-%d') 

# extract campaign-specific dates
all_campaign_info = loads(
    Path("analysis/0-lib/campaign_info.json").read_text(),
)

campaign_info = all_campaign_info[snapshot_date]

# get date on which age should be calculated
# this is different to snapshot date because we allow people to be eligible if they reach the age during the campaign
age_calculation_date = campaign_info["age_date"]

# end of follow-up for campaign (= day before next campaign))
end_of_campaign_date = campaign_info["final_milestone_date"]

# initialise dataset

dataset = create_dataset()

dataset.configure_dummy_data(population_size=1000)

registered_patients = practice_registrations.for_patient_on(snapshot_date)

registered = registered_patients.exists_for_patient()
registered_start_date = registered_patients.start_date
alive_ONS = (ons_deaths.date>snapshot_date) | ons_deaths.date.is_null()
alive_GP = (patients.date_of_death>snapshot_date) | patients.date_of_death.is_null()
alive = (alive_ONS & alive_GP)
eligibility_age = patients.age_on(age_calculation_date)

# define dataset poppulation
dataset.define_population(
  registered 
  & (registered_start_date <= (snapshot_date - weeks(12)))
  & alive
  & (eligibility_age >= 12) & (eligibility_age <= 104)
  & (patients.sex.is_in(["male", "female"]))
)

## demographic variables ----

dataset.eligibility_age = eligibility_age

demographic_variables(dataset = dataset, index_date = snapshot_date)

## primis variables ----

primis_variables(dataset = dataset, index_date = snapshot_date)

## extended subgroups ----

extended_subgroups(dataset = dataset, index_date = snapshot_date)

# other_cx_variables(dataset = dataset, index_date = snapshot_date)

# COVID-19 vaccination history ----

covid_vaccinations = (
  vaccinations
  .where(vaccinations.target_disease.is_in(["SARS-2 CORONAVIRUS"]))
  .sort_by(vaccinations.date)
)

# retrieve first 3 vaccines before and first 2 vaccines on or after snapshot date


# add next 2 recorded covid vaccination
add_n_vaccines(
    dataset = dataset, 
    index_date = snapshot_date, 
    target_disease = "SARS-2 CORONAVIRUS", 
    name = "covid_vax", 
    direction = "on_or_after",
    number_of_vaccines = 2,
    minimum_gap = 13
)

# add most recent 3 vaccines prior to snapshot date
add_n_vaccines(
    dataset = dataset, 
    index_date = snapshot_date, 
    target_disease = "SARS-2 CORONAVIRUS", 
    name = "covid_vax_prior", 
    direction = "before",
    number_of_vaccines = 3,
    minimum_gap = 13
)

# count total number of vaccines prior to vaccine date
dataset.covid_vax_prior_count = (
    covid_vaccinations
    .where(vaccinations.date.is_before(snapshot_date))
    .count_for_patient()
)

# Deregistration dates after the snapshot date
dataset.deregistered_date = registered_patients.end_date

# Covid-19 outcomes

all_covid_admissions = (
    apcs
    .where(apcs.all_diagnoses.contains_any_of(codelists.covid_icd10))
    .where(apcs.admission_method.is_in(["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"]))
    .where(apcs.patient_classification == "1")  # Ordinary admissions only
    .where(apcs.admission_date.is_on_or_between(snapshot_date,end_of_campaign_date))
    .sort_by(apcs.admission_date)
)

# covid-related admission 
dataset.covid_admitted_date = (
    all_covid_admissions
    .first_for_patient()
    .admission_date
)

# covid-related admission, primary diagnosis only
dataset.covid_admitted_primary_date = (
    all_covid_admissions
    .where(apcs.primary_diagnosis.is_in(codelists.covid_icd10))
    .first_for_patient()
    .admission_date
)

# first covid-related admission, length of stay
#dataset.covid_admitted_first_los = (all_covid_admissions.first_for_patient().discharge_date) - dataset.covid_admitted_date


# length of stay across all covid admissions with admission within the campaign date
dataset.covid_admitted_los = (
  all_covid_admissions.discharge_date - all_covid_admissions.admission_date
).days.sum_for_patient()

# covid-related critical care admission 
dataset.covid_critcare_date = (
  all_covid_admissions
  .where(apcs.days_in_critical_care>0)
  .first_for_patient()
  .admission_date
)


# covid-related death
#dataset.covid_death_date = (
#    ons_deaths
#        .cause_of_death_is_in(codelists.covid_icd10)
#        .date
#)


