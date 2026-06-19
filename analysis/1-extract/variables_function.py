# ##############################################
# # Common functions for contructing variables #
# ##############################################

# from ehrql.codes import CTV3Code, ICD10Code

from ehrql import case, days, when, years, months
from ehrql.tables.tpp import clinical_events_ranges

import codelists
 
from ehrql.tables.core import (
  medications,
  patients
)

from ehrql.tables.tpp import (
  addresses,
#  opa_cost,
  clinical_events,
  practice_registrations,
# appointments,
# vaccinations
)


#####################################################
# Common functions for contructing clinical queries 
#####################################################

# events occurring before a specified date

# query prior_events for existence of event-in-codelist, returns a patientSeries
def has_prior_event(codelist, index_date, where=True):
    prior_events = clinical_events.where(clinical_events.date.is_on_or_before(index_date))
    return (
        prior_events
        .where(where)
        .where(prior_events.snomedct_code.is_in(codelist))
        .exists_for_patient()
    )

# query prior_events for date of most recent event-in-codelist, returns a patientFrame
def last_prior_event(codelist, index_date, where=True):
    prior_events = clinical_events.where(clinical_events.date.is_on_or_before(index_date))
    return (
        prior_events.where(where)
        .where(prior_events.snomedct_code.is_in(codelist))
        .sort_by(clinical_events.date)
        .last_for_patient()
    )

# meds occurring before a specified date

# query prior_meds for existence of event-in-codelist, returns a patientSeries
def has_prior_meds(codelist, index_date, where=True):
    prior_meds = medications.where(medications.date.is_on_or_before(index_date))
    return (
        prior_meds.where(where)
        .where(prior_meds.dmd_code.is_in(codelist))
        .exists_for_patient()
    )
    
# query prior meds for date of most recent med-in-codelist, returns a patientFrame
def last_prior_meds(codelist, index_date, where=True):
    prior_meds = medications.where(medications.date.is_on_or_before(index_date))
    return (
        prior_meds.where(where)
        .where(prior_meds.dmd_code.is_in(codelist))
        .sort_by(medications.date)
        .last_for_patient()
    )




# --VARIABLES--

#######################################################
# PRIMIS
#######################################################




# Asthma
def has_asthma(index_date):
    # Asthma diagnosis
    has_astdx = has_prior_event(codelists.ast, index_date)
    # Asthma admision
    has_asthadm = has_prior_event(
        codelists.astadm,
        index_date,
        where = clinical_events.date.is_on_or_between(index_date - years(2), index_date)
    )
    # Inhaled asthma prescription in previous year
    has_astrx_inhaled = has_prior_meds(
        codelists.astrxm1,
        index_date,
        where = medications.date.is_on_or_after(index_date - years(1))
    )
    # count of systemic steroid prescription inpast 2 years
    prior_meds = medications.where(medications.date.is_on_or_before(index_date))
    count_astrx_oral = (
        prior_meds
        .where(prior_meds.dmd_code.is_in(codelists.astrxm2))
        .where(prior_meds.date.is_on_or_between(index_date - years(2), index_date))
        .count_for_patient()
    )
    # Asthma 
    asthma = case(
        when(has_asthadm).then(True),
        when(has_astdx & has_astrx_inhaled & (count_astrx_oral >= 2)).then(True),
        otherwise=False
    )
    return asthma

# Chronic Kidney Disease (CKD)
def has_ckd(index_date):
    # Chronic kidney disease diagnostic codes
    has_ckd_cov = has_prior_event(codelists.ckd_cov, index_date)
    # Chronic kidney disease codes - all stages
    ckd15_date = last_prior_event(codelists.ckd15, index_date).date
    # Chronic kidney disease codes-stages 3 - 5
    ckd35_date = last_prior_event(codelists.ckd35, index_date).date
    # Chronic kidney disease
    ckd = case(
        when(has_ckd_cov).then(True),
        when(ckd15_date.is_null()).then(False),
        when((ckd35_date >= ckd15_date)).then(True),
        otherwise=False
    )
    return ckd

# Chronic Respiratory Disease (CRD)
def has_crd(index_date, where=True):
    has_resp_cov = has_prior_event(codelists.resp_cov, index_date)
    has_crd = has_resp_cov | has_asthma(index_date)
    return has_crd

# Severe Obesity
def has_severe_obesity(index_date): 
    # Severe obesity only defined for people aged 18 and over
    aged18plus = patients.age_on(index_date) >= 18
    # Last BMI stage event
    date_bmi_stage = last_prior_event(
        codelists.bmi_stage, 
        index_date
    ).date
    # Last severe obesity event
    date_sev_obesity = last_prior_event(
        codelists.sev_obesity, 
        index_date
    ).date
    # Last BMI event not null
    event_bmi = last_prior_event(
        codelists.bmi, 
        index_date,
        where=(
            clinical_events.numeric_value.is_not_null() & 
            (clinical_events.numeric_value > 4) & 
            (clinical_events.numeric_value < 200)
        )
    )
    # Severe obesity
    severe_obesity = case(
        when(~aged18plus).then(False),
        when(
            (date_sev_obesity > event_bmi.date) | 
            (date_sev_obesity.is_not_null() & event_bmi.date.is_null())
        ).then(True),
        when(
            (event_bmi.date >= date_bmi_stage) & 
            (event_bmi.numeric_value >= 40.0)
        ).then(True),
        when(
            (date_bmi_stage.is_null()) & 
            (event_bmi.numeric_value >= 40.0)
        ).then(True),
        otherwise=False
    )
    return severe_obesity

# Pregnant variable to identify gestational diabetes
def has_pregnancy(index_date):
    # Pregnancy delivery code date (a delivery code between 8 and 15 months prior to index date)
    pregAdel_date = last_prior_event(
        codelists.pregdel,
        index_date,
        where=clinical_events.date.is_on_or_between(index_date - days(7 * 65), index_date - days((7 * 30) + 1))
    ).date
    # Pregnancy: 8 months and 15 months (a pregnancy code between 8 and 15 months prior to index date)
    pregA_date = last_prior_event(
        codelists.preg,
        index_date,
        where=clinical_events.date.is_on_or_between(index_date - days(7 * 65), index_date - days((7 * 30) + 1))
    ).date
    # Pregnancy: <8 months (a pregnancy code within 8 months prior to index date)
    pregB = has_prior_event(
        codelists.preg,
        index_date,
        where=clinical_events.date.is_on_or_between(index_date - days(7 * 30), index_date)
    )
    # Pregnancy group
    has_pregnancy = case(
        when(pregB).then(True),
        when(
            pregAdel_date.is_not_null() & 
             pregA_date.is_not_null() & 
             (pregA_date > pregAdel_date)
        ).then(True),
        otherwise=False
    )
    return has_pregnancy

# Diabetes
def has_diabetes(index_date, where=True):
    date_diab = last_prior_event(codelists.diab, index_date).date
    date_dmres = last_prior_event(codelists.dmres, index_date).date
    has_gdiab = has_prior_event(codelists.gdiab, index_date)
    has_diab_group = has_gdiab & has_pregnancy(index_date)
    has_addis = has_prior_event(codelists.addis, index_date)
    # Diabetes condition
    diabetes = case(
        when(date_dmres < date_diab).then(True),
        when(date_diab.is_not_null() & date_dmres.is_null()).then(True),
        when(has_addis).then(True),
        when(has_diab_group).then(True),
        otherwise=False
    )
    return diabetes

# Immunosuppression
def is_immunosuppressed(index_date):
    # Immunosuppression diagnosis
    has_immdx_cov = has_prior_event(
        codelists.immdx_cov, 
        index_date
    )
    # Immunosuppression medication (within the last 6 months)
    has_immrx = has_prior_meds(
        codelists.immrx,
        index_date,
        where=medications.date.is_on_or_after(index_date - months(6))
    )
    # Immunosuppression admin date (within the last 3 years)
    has_immadm = has_prior_event(
        codelists.immadm,
        index_date,
        where=clinical_events.date.is_on_or_after(index_date - years(3))
    )
    # Chemotherapy medication date (within the last 6 months)
    has_dxt_chemo = has_prior_event(
        codelists.dxt_chemo,
        index_date,
        where=clinical_events.date.is_on_or_after(index_date - months(6))
    )
    # Immunosuppression
    immunosupp = case(
        when(has_immdx_cov).then(True),
        when(has_immrx).then(True),
        when(has_immadm).then(True),
        when(has_dxt_chemo).then(True),
        otherwise=False
    )
    return immunosupp

# Severe mental illness 
def has_smi(index_date, where=True):
    date_sev_mental = last_prior_event(codelists.sev_mental, index_date).date
    # Remission codes relating to Severe Mental Illness
    date_smhres = last_prior_event(codelists.smhres, index_date).date
    # Severe mental illness
    smi = case(
        when(date_smhres < date_sev_mental).then(True),
        when(date_sev_mental.is_not_null() & date_smhres.is_null()).then(True),
        otherwise=False
    )
    return smi

# At risk group
def primis_atrisk(index_date):

    # This definition excludes the following groups:
    #   younger adults in long-stay nursing and residential care settings
    #   pregnancy

    return (
        is_immunosuppressed(index_date) |       # immunosuppression grouped
        has_ckd(index_date) |                   # chronic kidney disease
        has_crd(index_date) |                   # chronic respiratory disease
        has_diabetes(index_date) |              # diabetes        
        has_prior_event(codelists.cld, index_date) |      # chronic liver disease
        has_prior_event(codelists.cns_cov, index_date) |  # chronic neurological disease
        has_prior_event(codelists.chd_cov, index_date) |  # chronic heart disease
        has_prior_event(codelists.spln_cov, index_date) | # asplenia or spleen dysfunction
        has_prior_event(codelists.learndis, index_date) | # learning disability
        has_smi(index_date) |                   # severe mental illness
        has_severe_obesity(index_date)          # severe obesity
    )

## function to define primis variables across multiple dataset definitions
def primis_variables(dataset, index_date, var_name_suffix=""):
    dataset.add_column(f"immunosuppressed{var_name_suffix}", is_immunosuppressed(index_date)) # immunosuppress grouped
    dataset.add_column(f"ckd{var_name_suffix}", has_ckd(index_date)) # chronic kidney disease
    dataset.add_column(f"crd{var_name_suffix}", has_prior_event(codelists.resp_cov, index_date)) # chronic respiratory disease (inclduing asthma)
    dataset.add_column(f"diabetes{var_name_suffix}", has_diabetes(index_date)) # diabetes
    dataset.add_column(f"cld{var_name_suffix}", has_prior_event(codelists.cld, index_date)) # chronic liver disease
    dataset.add_column(f"chd{var_name_suffix}", has_prior_event(codelists.chd_cov, index_date)) # chronic heart disease
    dataset.add_column(f"cns{var_name_suffix}", has_prior_event(codelists.cns_cov, index_date)) # chronic neurological disease
    dataset.add_column(f"asplenia{var_name_suffix}", has_prior_event(codelists.spln_cov, index_date)) # asplenia or dysfunction of the Spleen
    dataset.add_column(f"learndis{var_name_suffix}", has_prior_event(codelists.learndis, index_date)) # learning Disability
    dataset.add_column(f"smi{var_name_suffix}", has_smi(index_date)) # severe mental illness
    dataset.add_column(f"severe_obesity{var_name_suffix}", has_severe_obesity(index_date)) # severe obesity
    dataset.add_column(f"primis_atrisk{var_name_suffix}", primis_atrisk(index_date)) # at risk 


def carehome_status(index_date):
  
  # TPP care home flag, using ddress linkage with CQC data
  carehome_tpp = addresses.for_patient_on(index_date).care_home_is_potential_match.when_null_then(False)

  # Patients in long-stay nursing and residential care
  # as per diagnostic and/or consultation code events directly related to care home residency
  carehome_primis = has_prior_event(codelists.carehome_primis, index_date)
  carehome_nhs_refset = has_prior_event(codelists.carehome_nhs_refset, index_date)
  
  return(
      carehome_tpp | carehome_primis | carehome_nhs_refset
  )


# cochlear implant 
def has_cochlear_implant(index_date, where=True):
    date_cochlear_implant = last_prior_event(codelists.cochlear_implant, index_date).date
    # Removal codes relating to cochlear implant
    date_remove_cochlear_implant = last_prior_event(codelists.remove_cochlear_implant, index_date).date
    # Severe mental illness
    cochlear_implant = case(
        when(date_remove_cochlear_implant < date_cochlear_implant).then(True),
        when(date_cochlear_implant.is_not_null() & date_remove_cochlear_implant.is_null()).then(True),
        otherwise=False
    )
    return cochlear_implant
###########################################################
# Extended subgroups
###########################################################
# CKD/RRT
# adapted from https://github.com/opensafely/covid_mortality_over_time/blob/main/analysis/utils/kidney_functions.R
def rrt_cat(index_date):
    # RRT: dialysis / transplant) ----------------------------------------------
    has_kidney_tx = has_prior_event(codelists.kidney_transplant, index_date)
    has_dial      = has_prior_event(codelists.dialysis, index_date)

    tx_date   = last_prior_event(codelists.kidney_transplant, index_date).date
    dial_date = last_prior_event(codelists.dialysis, index_date).date

    # Transplant: (AND NOT dialysis) OR (after dialysis)
    rrt_transplant = case(
        when(has_kidney_tx & ~has_dial).then(True),
        when(
            has_kidney_tx & has_dial &
            tx_date.is_not_null() & dial_date.is_not_null() &
            (tx_date > dial_date)
        ).then(True),
        otherwise=False
    )

    # Dialysis: (AND NOT transplant) OR (after transplant)
    rrt_dialysis = case(
        when(has_dial & ~has_kidney_tx).then(True),
        when(
            has_dial & has_kidney_tx &
            dial_date.is_not_null() & tx_date.is_not_null() &
            (dial_date > tx_date)
        ).then(True),
        otherwise=False
    )
    # RRT category: "1" dialysis, "2" transplant,  "0" none
    rrt_cat = case(
      when(rrt_dialysis).then("1 dialysis"),
      when(rrt_transplant).then("2 transplant"),
      otherwise="0 no RRT"
    )
    return rrt_cat

# ckd stage 3-5 in patients has_ckd = TRUE
def ckd_stage_3to5(index_date):
    ckd = has_ckd(index_date)

    ckd3_date = last_prior_event(codelists.ckd3_snomed, index_date).date
    ckd4_date = last_prior_event(codelists.ckd4_snomed, index_date).date
    ckd5_date = last_prior_event(codelists.ckd5_snomed, index_date).date

    stage = case(
        # no CKD
        when(~ckd).then("no CKD"),

        # latest CKD stage
        when(
            ckd5_date.is_not_null()
            & (ckd4_date.is_null() | (ckd5_date >= ckd4_date))
            & (ckd3_date.is_null() | (ckd5_date >= ckd3_date))
        ).then("5"),

        when(
            ckd4_date.is_not_null()
            & (ckd5_date.is_null() | (ckd4_date > ckd5_date))
            & (ckd3_date.is_null() | (ckd4_date >= ckd3_date))
        ).then("4"),

        when(
            ckd3_date.is_not_null()
            & (ckd4_date.is_null() | (ckd3_date > ckd4_date))
            & (ckd5_date.is_null() | (ckd3_date > ckd5_date))
        ).then("3"),

        # ckd, without ckd3-5 code
        otherwise="CKD, without stage 3-5 code"
    )
    return stage

# Creatinine event
def last_creatinine_event(index_date):
    creatinine_event = (
        clinical_events_ranges
        .where(clinical_events_ranges.snomedct_code.is_in(codelists.creatinine))
        .where(
            clinical_events_ranges.date.is_on_or_between(
                index_date - years(2), index_date
            )
        )
        .where(
            (clinical_events_ranges.comparator == "=")
            | (clinical_events_ranges.comparator.is_null())
        )
        .where(
            clinical_events_ranges.numeric_value.is_not_null()
            & (clinical_events_ranges.numeric_value > 20.0)
            & (clinical_events_ranges.numeric_value < 3000.0)
        )
        .sort_by(clinical_events_ranges.date)
        .last_for_patient()
    )
    return creatinine_event

# learning disability cateogry
def learndis_cat(index_date): 
    
    down_syndrome = has_prior_event(codelists.down_syndrome, index_date)
    learndis = has_prior_event(codelists.learndis, index_date)
    learndis_excl_register = has_prior_event(codelists.learndis_excl_register, index_date)
    learndis_register = has_prior_event(codelists.learndis_register, index_date)
    
    # categories
    learndis_cat = case(
        when(down_syndrome).then("Down's syndrome"),
        when(learndis_excl_register).then("Other learning disability"),
        when(learndis_register).then("Learning disability register"),
        otherwise = "No learning disability"
    )
    
    return learndis_cat

# Homelessness
def homeless(index_date, where=True):
    # homeless date
    date_homeless = last_prior_event(codelists.homeless, index_date).date
    # Residence code  date indicating no longer homeless
    date_reside = last_prior_event(codelists.reside, index_date).date
    # homelessness
    homeless = case(
        when(date_homeless.is_null()).then(False),
        when(date_homeless.is_not_null() & ((date_reside.is_not_null() & (date_reside <= date_homeless)) | date_reside.is_null())).then(True),
        otherwise = False
    )
    return homeless


def extended_subgroups(dataset, index_date, var_name_suffix=""):
    ## extended subgroups
    dataset.add_column(f"rrt_cat{var_name_suffix}", rrt_cat(index_date)) # rrt
    dataset.add_column(f"ckd_stage_3to5{var_name_suffix}", ckd_stage_3to5(index_date)) # ckd 3-5
    # dataset.add_column(f"creatinine_umol{var_name_suffix}", last_creatinine_event(index_date).numeric_value)
    # dataset.add_column(f"creatinine_age{var_name_suffix}", patients.age_on(last_creatinine_event(index_date).date))
    dataset.add_column(f"copd{var_name_suffix}", has_prior_event(codelists.copd, index_date)) # Chronic obstructive pulmonary disease
    dataset.add_column(f"learndis_cat{var_name_suffix}", learndis_cat(index_date)) # Learning disabilities categories
    dataset.add_column(f"sickle_cell{var_name_suffix}", has_prior_event(codelists.sickle_cell, index_date)) # Sickle cell anaemia
    dataset.add_column(f"cirrhosis{var_name_suffix}", has_prior_event(codelists.cirrhosis, index_date)) # cirrhosis 
    dataset.add_column(f"cochlear_implant{var_name_suffix}", has_cochlear_implant(index_date)) # cochlear implant
    dataset.add_column(f"cystic_fibrosis{var_name_suffix}", has_prior_event(codelists.cystic_fibrosis, index_date)) # cystic fibrosis
    dataset.add_column(f"csfl{var_name_suffix}", has_prior_event(codelists.csfl, index_date)) # Cerebrospinal fluid leak
    dataset.add_column(f"homeless{var_name_suffix}", homeless(index_date)) # Homeless
    

# def other_cx_variables(dataset, index_date, var_name_suffix=""):
    ## others of interest
#    dataset.add_column(f"sol_org_trans{var_name_suffix}", has_prior_event(solid_organ_transplant, index_date)) # Organs transplant
#    dataset.add_column(f"hiv{var_name_suffix}", has_prior_event(hiv_aids, index_date)) #HIV/AIDS
#    dataset.add_column(f"cancer{var_name_suffix}", 
#                       has_prior_event(cancer_nonhaem_snomed, index_date, where=clinical_events.date.is_after(index_date - days(int(3 * 365.25))))|
#                       has_prior_event(cancer_haem_snomed, index_date, where=clinical_events.date.is_after(index_date - days(int(3 * 365.25))))
#                       ) #cancer   

############################################################
# demographic variables
############################################################
def demographic_variables(dataset, index_date, var_name_suffix=""):
    registration = practice_registrations.for_patient_on(index_date)
    dataset.add_column(f"age{var_name_suffix}", patients.age_on(index_date))
    dataset.add_column(f"region{var_name_suffix}", registration.practice_nuts1_region_name)
    dataset.add_column(f"stp{var_name_suffix}", registration.practice_stp)
    dataset.add_column(f"imd{var_name_suffix}", addresses.for_patient_on(index_date).imd_rounded)
    dataset.add_column(f"carehome_status{var_name_suffix}", carehome_status(index_date))
    

# See https://github.com/opensafely/reusable-variables/blob/main/analysis/vaccine-history/vaccine_variables.py
# this is an adpated version that only selects vaccines _near_ the index date 
# without extracting the entire vacciantion history

from ehrql.tables.tpp import (
  vaccinations
)

def add_n_vaccines(dataset, index_date, target_disease, name, direction = None, number_of_vaccines = 3, minimum_gap = 1):

    assert direction in ["after", "on_or_after", "before", "on_or_before"], "direction value must be after, on_or_after, before, on_or_before"
    
    assert minimum_gap > 0, "minimum_gap must be at least 1 to ensure that same-day vaccinations are not stuck in a loop"

    if direction == "after":
        current_date = index_date - days(minimum_gap-1)
    elif direction == "on_or_after":
        current_date = index_date - days(1) - days(minimum_gap-1)
    elif direction == "before":
        current_date = index_date + days(minimum_gap-1)
    elif direction == "on_or_before":
        current_date = index_date + days(1) + days(minimum_gap-1)
    else:
        raise ValueError("direction must be 'before' or 'after'") 
    
    # select all vaccination events that target {target_disease}
    covid_vaccinations = (
        vaccinations
        .where(vaccinations.target_disease == target_disease)
        .sort_by(vaccinations.date)
    )

    # loop over first, second, ..., nth vaccination event ON OR AFTER or ON OR BEFORE index date for each person
    # extract info on vaccination date and product
    for i in range(1, number_of_vaccines + 1):

        # vaccine variables
        if direction in ["after", "on_or_after"]:
            current_vax = covid_vaccinations.where(covid_vaccinations.date > (current_date + days(minimum_gap-1))).first_for_patient()
        if direction in ["before", "on_or_before"]:
            current_vax = covid_vaccinations.where(covid_vaccinations.date < (current_date - days(minimum_gap-1))).last_for_patient()
        
        dataset.add_column(f"{name}_{i}_date", current_vax.date)
        dataset.add_column(f"{name}_{i}_product", current_vax.product_name)
        
        current_date = current_vax.date
        

