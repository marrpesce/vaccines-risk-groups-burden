# _________________________________________________
# Purpose:
# define useful functions used in the codebase
# define key design features for the study
# define some look up tables to use in the codebase
# this script should be sourced (using `source(here("analysis", "design.R"))`) at the start of each R script
# _________________________________________________

library("tidyverse")

# utility functions ----

roundmid_any <- function(x, to = 1) {
  # like ceiling_any, but centers on (integer) midpoint of the rounding points
  if (to == 0) {
    x
  } else {
    ceiling(x / to) * to - (floor(to / 2) * (x != 0))
  }
}

ceiling_any <- function(x, to = 1) {
  # round to nearest 100 millionth to avoid floating point errors

  if (to == 0) {
    return(x)
  } else {
    return(ceiling(plyr::round_any(x / to, 1 / 100000000)) * to)
    # return(plyr::round_any(x, to, f=ceiling))
  }
}


floor_any <- function(x, to = 1) {
  # round to nearest 100 millionth to avoid floating point errors

  if (to == 0) {
    return(x)
  } else {
    return(floor(plyr::round_any(x / to, 1 / 100000000)) * to)
    # return(plyr::round_any(x, to, f=floor))
  }
}


round_any <- function(x, to = 1) {
  if (to == 0) {
    x
  } else {
    if_else(x != 5, plyr::round_any(x, accuracy = to), 0)
  }
}

# get nth largest value from list
nthmax <- function(x, n = 1) {
  dplyr::nth(sort(x, decreasing = TRUE), n)
}

# get nth smallest value from list
nthmin <- function(x, n = 1) {
  dplyr::nth(sort(x, decreasing = FALSE), n)
}

# overwrite splice function to avoid deprecation warnings
splice <- function(...) {
  list_flatten(lst(...), name_spec = "{inner}", name_repair = "check_unique")
}

# uses dplyr::case_when but converts the output to a factor,
# with factors ordered as they appear in the case_when's  ... argument
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels = levels)
}


# Design elements ----

# key study dates
# The dates are saved in json format so they can be read in by R and python scripts
# - firstpossiblevax_date is the date from which we want to identify covid vaccines. the mass vax programme was 8 Dec 2020 but other people were vaccinated earlier in trials, so want to pick these people up too (and possibly exclude them)
# - start_date is when we start the observational period proper, at the start of the mass vax programme
# - end_date is when we stop the observation period. This must be extended as the study progresses

# study_dates <-
#   list(
#   #  firstpossiblevax_date = "2020-07-01",
#     start_date1 = "2010-09-01",
#     end_date1 = "2013-08-31",
#     start_date2 = "2020-03-23",
#     end_date2 = "2026-08-31"
#   ) |>
#   lapply(as.Date)

# make these available in the global environment
# so we don't have to use `study_dates$start_date` or `start_date <- study_dates$start_date` in each script
# list2env(study_dates, globalenv())

# statistical disclosure control rounding precision
sdc_threshold <- 10

# cohort dates
cohort_info <-
  tribble(
    ~cohort_id,        ~target,      ~cohort,          ~cohort_start_date, ~cohort_end_date, ~age_threshold, ~clinical_priority,

    "flu_2023_24",     "Influenza",  "2023/24",        "2023-09-01",       "2024-03-31",     65,             "primis_atrisk",
    "flu_2024_25",     "Influenza",  "2024/25",        "2024-09-01",       "2025-03-31",     65,             "primis_atrisk",
    "flu_2025_26",     "Influenza",  "2025/26",        "2025-09-01",       "2026-03-31",     65,             "primis_atrisk",

    "covid_wave_1",    "COVID-19",   "Wave 1",         "2020-03-23",       "2020-05-30",     NA,             "primis_atrisk",
    "covid_ba1_ba2",   "COVID-19",   "BA.1/BA.2 wave", "2021-12-15",       "2022-04-29",     NA,             "primis_atrisk",
    "covid_jn1",       "COVID-19",   "JN.1 wave",      "2023-12-04",       "2024-03-31",     NA,             "primis_atrisk",
    "covid_2025_26",   "COVID-19",   "2025/26",        "2025-09-01",       "2026-03-31",     75,             "primis_atrisk",

    "rsv_2023_24",     "RSV",        "2023/24",        "2023-09-01",       "2024-03-31",     NA,             "primis_atrisk",
    "rsv_2024_25",     "RSV",        "2024/25",        "2024-09-01",       "2025-03-31",     NA,             "primis_atrisk",
    "rsv_2025_26",     "RSV",        "2025/26",        "2025-09-01",       "2026-03-31",     75,             "primis_atrisk" #,

    # "zoster_2010_11",  "Zoster",     "2010/11",        "2010-09-01",       "2011-08-31",     70,             "primis_atrisk",
    # "zoster_2011_12",  "Zoster",     "2011/12",        "2011-09-01",       "2012-08-31",     70,             "primis_atrisk",
    # "zoster_2012_13",  "Zoster",     "2012/13",        "2012-09-01",       "2013-08-31",     70,             "primis_atrisk",
    # "zoster_2023_24",  "Zoster",     "2023/24",        "2023-09-01",       "2024-08-31",     65,             "primis_atrisk",
    # "zoster_2024_25",  "Zoster",     "2024/25",        "2024-09-01",       "2025-08-31",     65,             "primis_atrisk",
    # "zoster_2025_26",  "Zoster",     "2025/26",        "2025-09-01",       "2026-08-31",     65,             "primis_atrisk"
  )|>
  mutate(
    across(
      c(cohort_start_date, cohort_end_date),
      as.Date
    )
  ) |>
  mutate(
    cohort_end_days = as.integer(cohort_end_date - cohort_start_date) + 1L
  )

# output from https://jobs.opensafely.org/opensafely-internal/tpp-vaccination-names/ workspace
# shows all possible covid vaccination names in TPP

# lookup to rename TPP product names to coding-friendly product names
# taking format: manufacturer/brand _ era/variant _ (dose/target) _ (modality)
vax_product_lookup <- c(

  # Pfizer adult
  "pfizer_original" = "COVID-19 mRNA Vaccine Comirnaty 30micrograms/0.3ml dose conc for susp for inj MDV (Pfizer)",
  "pfizer_BA1" = "Comirnaty Original/Omicron BA.1 COVID-19 Vacc md vials",
  "pfizer_BA45" = "Comirnaty Original/Omicron BA.4-5 COVID-19 Vacc md vials",
  "pfizer_XBB15" = "Comirnaty Omicron XBB.1.5 COVID-19 Vacc md vials",
  "pfizer_JN1" = "Comirnaty JN.1 COVID-19 mRNA Vaccine 0.3ml inj md vials (Pfizer Ltd)",
  "pfizer_LP81" = "Comirnaty LP.8.1 COVID-19 Vacc 30microg/0.3ml dose inj pfs (Pfizer Ltd)",
  "pfizer_KP2" = "Comirnaty KP.2 COVID-19 Vacc 30microg/0.3ml dose inj md vial (Pfizer Ltd)",
  "pfizer_KP2_pfs" = "Comirnaty KP.2 COVID-19 Vacc 30microg/0.3ml dose inj pfs (Pfizer Ltd)",

  "pfizer_unspecified" = "Comirnaty COVID-19 mRNA Vacc ready to use 0.3ml inj md vials",

  # Pfizer children

  "pfizer_original_children" = "COVID-19 mRNA Vaccine Comirnaty Children 5-11yrs 10mcg/0.2ml dose conc for disp for inj MDV (Pfizer)",
  "pfizer_JN1_children" = "Comirnaty JN.1 Children 5-11yrs COVID-19 Vacc 0.3ml sd vials (Pfizer Ltd)",
  "pfizer_XBB15_children" = "Comirnaty Omicron XBB.1.5 Child 5-11y COVID-19 Vacc md vials",
  "pfizer_LP81_children" = "Comirnaty LP.8.1 Children 5-11y COVID-19 Vacc 0.3ml sd vials  (Pfizer Ltd)", # note double space before "(Pfizer Ltd)"

  "pfizer_original_under5" = "Comirnaty Children 6m-4yrs COVID-19 mRNA Vacc 0.2ml md vials",
  "pfizer_JN1_under5" = "Comirnaty JN.1 Children 6m-4yrs COVID-19 Vacc 0.3ml md vials (Pfizer Ltd)",
  "pfizer_XBB15_under5" = "Comirnaty Omicron XBB.1.5 Child 6m-4y COVID-19 Vacc md vials",
  "pfizer_LP81_under5" = "Comirnaty LP.8.1 Children 6m-4y COVID-19 Vacc 0.3m md vials  (Pfizer Ltd)", # note, double space before "(Pfizer Ltd)"

  # Astrazeneca

  "az_original" = "COVID-19 Vaccine Vaxzevria 0.5ml inj multidose vials (AstraZeneca)",
  "az_original_half" = "COVID-19 Vac AZD2816 (ChAdOx1 nCOV-19) 3.5x10*9 viral part/0.5ml dose sol for inj MDV (AstraZeneca)",

  # Moderna

  "moderna_original" = "COVID-19 mRNA Vaccine Spikevax (nucleoside modified) 0.1mg/0.5mL dose disp for inj MDV (Moderna)",
  "moderna_omicron" = "COVID-19 Vac Spikevax (Zero)/(Omicron) inj md vials",
  "moderna_BA45" = "COVID-19 Vacc Spikevax Orig/Omicron BA.4/BA.5 inj md vials",
  "moderna_XBB15" = "COVID-19 Vacc Spikevax (XBB.1.5) 0.1mg/1ml inj md vials",
  "moderna_JN1" = "Spikevax JN.1 COVID-19 Vacc 0.1mg/ml inj md vials (Moderna, Inc)",
  "moderna_omicron2" = "COVID-19 Vaccine Moderna (mRNA-1273.529) 50micrograms/0.25ml dose sol for inj MDV",
  "moderna_unspecified" = "COVID-19 Vaccine Moderna 0.5ml dispersion for inj vials",

  # Sanofi-GSK
  "sanofigsk_B1" = "COVID-19 Vacc VidPrevtyn (B.1.351) 0.5ml inj multidose vials",
  "sanofigsk_D614" = "COVID-19 Vac Sanofi (CoV2 preS dTM monovalent D614 (recombinant)) 5mcg/0.5ml dose susp for inj MDV",
  "sanofigsk_D614B1" = "COVID-19 Vacc Sanofi (D614+B.1.351) 0.5ml inj md vials",


  # Novavax
  "novavax" = "COVID-19 Vac Nuvaxovid (recombinant, adj) 5micrograms/0.5ml dose susp for inj MDV (Novavax CZ a.s.)",

  # Sputnik
  "sputnik_i_multi" = "COVID-19 Vacc Sputnik V Component I 0.5ml multidose vials",
  "sputnik_ii_multi" = "COVID-19 Vacc Sputnik V Component II 0.5ml multidose vials",
  "sputnik_i_inj" = "COVID-19 Vaccine Sputnik V Component I 0.5ml inj vials",
  "sputnik_ii_inj" = "COVID-19 Vaccine Sputnik V Component II 0.5ml inj vials",

  # Janssen
  "jansenn" = "COVID-19 Vaccine Janssen (Ad26.COV2-S (recomb)) 0.5ml dose solution for injection multidose vials",

  # Sinopharm
  "sinopharm" = "COVID-19 Vac Sinopharm BIBP (inactivated adjuvanted) 6.5U/0.5ml dose susp for inj vials",

  # Valneva
  "valneva" = "COVID-19 Vaccine Valneva (inactivated adj whole virus) 40antigen units/0.5ml dose susp for inj MDV",

  # Medicago
  "medicago" = "COVID-19 Vaccine Medicago (CoVLP) 3.75micrograms/0.5ml dose emulsion for injection multidose vials",

  # Convidecia
  "convidecia" = "COVID-19 Vaccine Convidecia 0.5ml inj vials",

  # Covaxin
  "covaxin" = "COVID-19 Vac Covaxin (NIV-2020-770 inactivated) 6micrograms/0.5ml dose susp for inj MDV",

  # Coronavac
  "coronavac" = "COVID-19 Vac CoronaVac (adjuvanted) 600U/0.5ml dose susp for inj vials",

  # Covishield
  "covishield" = "COVID-19 Vac Covishield (ChAdOx1 S recombinant) 5x10*9 viral particles/0.5ml dose sol for inj MDV",

  # Covovax
  "covovax" = "COVID-19 Vac Covovax (adjuvanted) 5micrograms/0.5ml dose susp for inj MDV (Serum Institute of India)",

  # Not specified
  "unspecified" = "SARS-2 Coronavirus vaccine"

)


# lookup to rename coding-friendly product names to publication-friendly product names
vax_product_core_levels <- c(
  "pfizer_original",
  "pfizer_BA1",
  "pfizer_BA45",
  "pfizer_XBB15",
  "pfizer_JN1",
  "pfizer_LP81",
  "pfizer_KP2",

  "pfizer_original_children",

  "az_original",
  "az_original_half",

  "moderna_original",
  "moderna_omicron",
  "moderna_BA45",
  "moderna_XBB15",
  "moderna_JN1",
  "moderna_omicron2",

  "sanofigsk_B1",

  "novavax",

  "jansenn",
  "coronavac",
  "covishield"
)


# relabel_from_lookup <- function(x, from, to, source){
#   left_join(tibble(x=x), source, by = {{from}})[[{{to}}]]
# }


## factor levels provided in a sensible order, as this won't happen directly from opensafely ----

factor_levels <-
  lst(
    ethnicity5 = c(
      "White",
      "Mixed",
      "Asian or Asian British",
      "Black or Black British",
      "Chinese or Other Ethnic Groups"
    ),
    ethnicity16 = c(
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

    learndis_cat = c(
      "No learning disability",
      "Down's syndrome",
      "Other learning disability",
      "Learning disability register"
    ),
    ckd_stage_3to5 = c(
      "no ckd",
      "3",
      "4",
      "5",
      "ckd, without ckd3-5 code"
    ),
    ckd_rrt = c(
      "No CKD or RRT",
      "RRT (transplant)",
      "RRT (dialysis)",
      "Stage 3",
      "Stage 4",
      "Stage 5",
      "CKD, without stage 3-5 code"
    )

  )


# template for standardising demographic characteristics that are extracted multiple times
# using this in mutate like this: `mutate(!!!standardise_demographic_characteristics)`
standardise_demographic_characteristics <-
  rlang::quos(

    ## --VARIABLES--
    ## demographics
    ageband4 = cut(
      age,
      breaks = c(-Inf, 12, 50, 65, 75, 105, Inf),
      labels = c("under 12", "12-49", "50-64", "65-74", "75-104", "105+"), # under 12 and 105+ should be excluded in analysis but include here to ensure nobody slipped through the net
      right = FALSE
    ),
    ageband13 = cut(
      age,
      breaks = c(-Inf, 12, 18, 30, 40, 50, 55, 60, 65, 70, 75, 80, 85, 90, 105, Inf),
      labels = c("under 12", "12-17", "18-29", "30-39", "40-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-104", "105+"), # under 12 and 105+ should be excluded in analysis but include here to ensure nobody slipped through the net
      right = FALSE
    ),
    region = fct_collapse(
      factor(region, ordered = FALSE),
      `East of England` = "East",
      `London` = "London",
      `Midlands` = c("West Midlands", "East Midlands"),
      `North East and Yorkshire` = c("Yorkshire and The Humber", "North East"),
      `North West` = "North West",
      `South East` = "South East",
      `South West` = "South West"
    ),
    imd_quintile = cut(
      imd,
      breaks = c(0, 32844 * (1:5) / 5),
      labels = c("1 (most deprived)", "2", "3", "4", "5 (least deprived)"),
      include.lowest = TRUE,
      right = FALSE
    ),
  )

## CKD-RRT classification
standardise_primis_and_extended_characteristics <-
  rlang::quos(

    rrt_cat = factor(rrt_cat,  levels = levels(rrt_cat), ordered = FALSE),

    ckd_rrt = case_when(
      rrt_cat == "1 dialysis" ~ "RRT (dialysis)",
      rrt_cat == "2 transplant" ~ "RRT (transplant)",
      ckd_stage_3to5 == "5" ~ "Stage 5",
      ckd_stage_3to5 == "4" ~ "Stage 4",
      ckd_stage_3to5 == "3" ~ "Stage 3",
      ckd_stage_3to5 == "ckd, without stage 3-5 code" ~ "CKD, without stage 3-5 code",
      TRUE ~ "No CKD or RRT"
    ) |> factor(levels = factor_levels$ckd_rrt, ordered = FALSE),

    ckd_stage_3to5 = NULL, # remove ckd stage to save memory
    rrt_cat = NULL, # remove rrt_cat stage to save memory

    learndis_cat = factor(learndis_cat,  levels = factor_levels$learndis_cat, ordered = FALSE),

    cns_learndis = (cns | learndis),

    sickle_cell_asplenia = (sickle_cell | asplenia)
  )



# function to convert ethnicity 16 group into 5 group
ethnicity_16_to_5 <- function(x) {
  x1 <- fct_relabel(x, ~ str_extract(.x, ".*(?= - )")) # pick up everything before " - "
  x2 <- fct_recode(x1, `Chinese or Other Ethnic Groups` = "Other Ethnic Groups")
  return(x2)
}

# List of groups and subgroups for reporting summary statistics

## --VARIABLES--

level1_group <- c(

  # Level 1A (all)
  "all",

  # level 1B (age4)
  "ageband4" #,

  # # level 1C (age13)
  # "ageband13",

  # # Level 1D (eligibility)
  # "any_eligibility",
  # "age_above_eligiblity_threshold",
  # "clinical_priority",
  # "clinical_priority_only",
  # "carehome_status",

  # # Level 1E (clinical risk)
  # "primis_atrisk",
  # "crd",
  # "chd",
  # "ckd",
  # "cld",
  # "cns_learndis",
  # "diabetes",
  # "immunosuppressed",
  # "asplenia",
  # "severe_obesity",
  # "smi"
)

level2_group <- c(
  "all",

  # sociodemographic subgroups
  "ageband4",
  "ageband13",
  "sex",
  "ethnicity5",
  "ethnicity16",
  "region",
  "imd_quintile",
  "carehome_status",

  # Core clinical risk subgroups
  "primis_atrisk",
  "crd",
  "chd",
  "ckd",
  "cld",
  "cns_learndis",
  "diabetes",
  "immunosuppressed",
  "asplenia",
  "severe_obesity",
  "smi",

  # Extended subgroups
  "ckd_rrt", # RRT - CKD3-5
  "copd", # Chronic obstructive pulmonary disease
  "learndis_cat", # Learning disabilities categories
  "sickle_cell_asplenia", # Sickle Cell or asplenia
  "cirrhosis",          # Cirrhosis
  "cochlear_implant",   # Cochlear implant
  "cystic_fibrosis",    # Cystic fibrosis
  "csfl",               # Cerebrospinal fluid leak
  "homeless"           # Homeless
)

level_combos <-
  expand_grid(
    group1 = level1_group,
    group2 = level2_group
  ) |>
  filter(
    (group1 == group2) %in% c(FALSE, NA) | (group1 == "all"),
    !((group1 %in% c("ageband4", "ageband13")) & (group2 %in% c("ageband4", "ageband13"))),
    !((group1 %in% c("age_above_eligiblity_threshold", "clinical_priority_only")) & (group2 %in% c("ageband4", "ageband13"))),
    !((group1 %in% c("clinical_priority_only")) & (group2 %in% c("primis_atrisk"))),
  )

# Local run flag ----
# is this script being run locally, and if so do we need to output objects to be picked up by ehrQL scripts

localrun <- Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")

if (localrun) {

  # jsonlite::write_json(
  #   study_dates,
  #   path = here::here("analysis", "0-lib", "study_dates.json"),
  #   pretty = TRUE, auto_unbox = TRUE
  # )

  jsonlite::write_json(
  split(cohort_info, f = cohort_info$cohort_id) |> lapply(as.list),
  path = here::here("analysis", "0-lib", "cohort_info.json"),
  pretty = TRUE,
  auto_unbox = TRUE
  )
}
