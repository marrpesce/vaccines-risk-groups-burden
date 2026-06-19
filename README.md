# covid-vaccine-history

[View on OpenSAFELY](https://jobs.opensafely.org/repo/https%253A%252F%252Fgithub.com%252Fopensafely%252Fcovid-vaccine-history)

Details of the purpose and any published outputs from this project can be found at the link above.

The contents of this repository MUST NOT be considered an accurate or valid representation of the study or its purpose. 
This repository may reflect an incomplete or incorrect analysis with no further ongoing work.
The content has ONLY been made public to support the OpenSAFELY [open science and transparency principles](https://www.opensafely.org/about/#contributing-to-best-practice-around-open-science) and to support the sharing of re-usable code for other subsequent users.
No clinical, policy or safety conclusions must be drawn from the contents of this repository.

# About the OpenSAFELY framework

The OpenSAFELY framework is a Trusted Research Environment (TRE) for electronic
health records research in the NHS, with a focus on public accountability and
research quality.

Read more at [OpenSAFELY.org](https://opensafely.org).

# Licences
As standard, research projects have a MIT license. 


# Project details

This repository contains code to describe patterns of COVID-19 vaccine receipt in England since December 2020. The are two key strands:

1. Describing vaccine receipt -- dates and products -- over time across multiple vaccination campaigns, stratified by demographic and clinical characteristics;
2. Describing vaccine receipt within each campaign (now settled into a regular Spring and Autumn rhythm) stratified by demographic, clinical, and prior COVID-19 vaccine characteristics.

## Repository navigation

- The [`codelists/`](./codelists/) directory contains all the codelists used to define variables in analysis. 
- The [`analysis/`](./analysis) directory contains the executable scripts used to conduct the analysis. 
- The [`project.yaml`](./project.yaml) defines run-order and dependencies for all the analysis scripts.
- Non-disclosive model outputs, including tables, figures, etc, are available via the OpenSAFELY job server.


## Analysis scripts

The analysis scripts in the [`analysis/`](./analysis) directory are organised into sub-directories as follows:

- [`0-lib/`](./analysis/0-lib/):
  - [`design.R`](./analysis/0-lib/design.R) defines the design configurations used throughout the study - start and end dates, eligibility, products, look-up dictionaries, etc.
  It also defines R functions used throughout the codebase.
  This script is run at the start of all relevant R scripts. 
  It must also be run manually if values in either of the [`study_dates.json`](./analysis/0-lib/study_dates.json) or [`campaign_info.json`](./analysis/0-lib/campaign_info.json) files need to be updated, 
  so that the ehrQL scripts use the most up-to-date values too. 
- [`1-extract/`](./analysis/1-extract/):
  - [`dataset_definition_fixed.py`](./analysis/1-extract/dataset_definition_fixed.py) is the ehrQL script for selecting all variables that are fixed (e.g., date of death), or assumed to be fixed (e.g., ethnicity), as at the study end date.
  This information is extracted once and then joined onto other datasets where needed, to save computation time.
  - [`dataset_definition_varying.py`](./analysis/1-extract/dataset_definition_varying.py) is the ehrQL script to extract information as at the time of each COVID-19 vaccine event.
  The outputted dataset includes a set of columns for each vaccination event, with one column for each variable of interest (e.g., vaccination date, product, GP practice, deprivation level, other clinical characteristics).
  For instance, region_i (region_1, region_2, region_3, ...) represents the region of the person's registered address at the time of the i-th COVID-19 vaccine. 
  This may be modified in future to use ehrQL's new event-level data features.
  - [`dataset_definition_snapshot.py`](./analysis/1-extract/dataset_definition_snapshot.py) is the ehrQL script to extract information as at the start of a given vaccine campaign, for anyone alive and registered on the snapshot date. 
  `snapshot_date` is a parameter supplied to this script in the project.yaml file, representing the start date of the campaign of interest, which must be supplied in the format `YYYYMMDD`.
  - [`dummy_dataset_definition_....R`](./analysis/1-extract/) are the corresponding R scripts that create custom dummy data for each of the 3 dataset definitions. 
  Custom dummy data is used instead of the dummy data created by ehrQL as it provides more control over the structure in the data, such as more realistic vaccination dates or event rates.
  If a dataset definition is updated, the dummy dataset definition script must also be updated to ensure all variable names and types match.
  - [`variables.py`](./analysis/1-extract/variables_function.py) contains some function and variable definitions to be read in by the dataset definition.
  - [`codelist.py`](./analysis/1-extract/codelists.py) pulls the codelists from the [`codelists/`](./codelists/) directory to be usable in the dataset definition. 
- [`2-prepare/`](./analysis/2-prepare/):
  - [`prepare.R`](./analysis/2-prepare/prepare.R) this script imports the extracted database data (or dummy data) from the `fixed` and `varying` datasets, tidies some variables, derives some new ones, and and reshapes the time-varying data to be one-row-per-vaccine.
- [`3-history/`](./analysis/3-history/)
  - [`report_history.R`](./analysis/3-history/report_history.R) collects all processed vaccination data and reports vaccine counts and products, by week, over the observation period, across a number of subgroups.
- [`4-snapshot/`](./analysis/4-snapshot/)
  - [`report_snapshot.R`](./analysis/4-snapshot/report_snapshot.R) reports vaccine history for people on the start of a given vaccine campaign, and subsequent vaccine coverage within this campaign. 
  It imports the vaccination data from the `prepare.R` script, processes date-specific info from the `dataset_definition_snapshot.py` script, combines them, then produces a collection of outputs about the eligible vaccine population and subsequent coverage on that date. 

## Output files

The structure of the output directory (not tracked via git) is designed to match the structure of the [analysis directory](./analysis). For example, all files created by the action `report_snapshot_20210906`, which runs the [`./output/4-snapshot/report_snapshot.R`](./analysis/4-snapshot/report_snapshot.R) script for `snapshot_date=20210906`, will be saved in the `./output/4-snapshot/report_snapshot_20210906/` directory. 


- `2-prepare/` - this directory contains outputs from the preparation scripts, which performs some data cleaning, data validation checks, and outputs processed data for analysis:
  - `vax_data_quality/count_product.csv` reports the count of each vaccine product, and the earliest date that this product was given, across the entire history of the Covid-19 vaccination programme. Counts are stratified by adults / children. 
  - `vax_data_quality/count_product_campaign.csv` reports the count of each vaccine product, and the earliest date that this product was given, for each vaccination campaign separately.
  - `vax_data_quality/count_product_cooccurrrence.csv` reports the number of times that a person was recorded as being vaccinated with a particular combination of Covid-19 vaccines on the same day. For instance, "1x pfizer_original" or or "1x pfizer_original AND 2x az_original" on the same day. Co-occurrence of two or more vaccines is almost certainly due to data quality issues, and this table helps determine how best to deal with these issues.
  - `vax_data_quality/count_product_cooccurrrence_campaign.csv` as above, for each vaccination campaign separately.
  - `prepare/data_extract_fixed_skim.csv` a summary of the `extract_fixed.arrow` dataset, created by the [`dataset_definition_fixed.py`](./analysis/1-extract/dataset_definition_fixed.py) script and imported into R.
  - `prepare/data_processed_fixed_skim.csv` a summary of the post-processed `extract_fixed` dataset.
  - `prepare/data_vax_ELD_skim.csv` a summary of the dataset of patients' Covid-19 vaccination history, as contained in the `vaccinations.arrow` dataset created by the [`dataset_definition_varying.py`](./analysis/1-extract/dataset_definition_varying.py) script and imported into R, with some processing, to compare event-level data extract with patient-level data extract.
  - `prepare/data_vax_PLD_skim.csv` a summary of the dataset of patients' Covid-19 vaccination history, as contained in the `extract_varying.arrow` dataset created by the [`dataset_definition_varying.py`](./analysis/1-extract/dataset_definition_varying.py) script and imported into R, with some processing, to compare event-level data extract with patient-level data extract..
  - `prepare/data_vax_clean_skim.csv` a summary of the dataset of patients' Covid-19 vaccination history, as contained in the `extract_varying.arrow` dataset created by the [`dataset_definition_varying.py`](./analysis/1-extract/dataset_definition_varying.py) script and imported into R, with some processing and additional cleaning.
  - `prepare/data_vax_skim.csv` a summary of the dataset of patients' Covid-19 vaccination history, as contained in the `extract_varying.arrow` dataset created by the [`dataset_definition_varying.py`](./analysis/1-extract/dataset_definition_varying.py) script and imported into R, with some processing and without additional cleaning.
- `3-history/` - this directory contains outputs from the vaccine history script, which summarises Covid-19 vaccination history across all campaigns. This is mostly useful for a high-level overview of vaccine events and products over time.
  - `report_history/validation.csv` reports basic data quality information about overall vaccination dates. Total recorded vaccinations, number of missing vaccination dates, number of dates that are inaccurate due to preceding the pandemic, etc.
  - `report_history/validation_stratified.csv` as above, stratified by product type.
  - `report_history/validation_vax_count.csv` as above, stratified by dose number.
  - `report_history/vax_counts_product_campaign.csv` reports the number of vaccination events, by campaign and product type.
  - `report_history/vax_counts_product_dosenumber.csv` reports the number of vaccination events, by dose number and product type.
  - `report_history/vax_counts_stratified.csv` reports the number of vaccination events, by dose number, product type, sex, age band, ethnicity, region, and IMD group.
  - `report_history/vax_dates_[level1_variable]_[level2_variable].png` depicts a histogram of vaccination dates over all campaigns for each product, stratified by two variables.
  - `report_history/vax_intervals_[level1_variable]_[level2_variable].png` depicts a histogram of time since previous vaccination (vaccination interval) for each dose, stratified by two variables.
- [`4-snapshot/`](./analysis/4-snapshot/) - this directory contains outputs from the snapshot script, which summarises each vaccination campaign as a "snapshot". It includes information about the eligible population at the start of the campaign, the cumulative incidence of Covid-19 vaccination over the course of the campaign, and the overall burden of Covid-19-related disease within the campaign. All stratified by various subgroups.
  - `contrasts_[outcome].csv` reports and compares rates of Covid-19 vaccination, hospital admission, critical care admission, and death across various subgroups, using Incidence Rates (IRs) and Incidence Rate Ratios (IRRs)
  - `data_combined_skim.scv` a summary of the dataset combining campaign-specific data and other time-invariant data.
  - `data_snapshot_skim.scv` a summary of the dataset containing campaign-specific snapshot data, created by the [`dataset_definition_snapshot.py`](./analysis/1-extract/dataset_definition_snapshot.py) script and imported into R.
  - `km_estimates_vax_table_[level1_variable].csv` the underlying data for the Kaplan-Meier cumulative incidence curves for each level-1 variable, including further stratification by all level-2 variables.
  - `km_vax_[level1_variable]_[level2_variable].png` depicts Kaplan-Meier cumulative incidence curves for vaccine coverage over time, for each combination of level-1 and level-2 variables.
  - `km_estimates_vax_[outcome]_[milestone].csv` the underlying data for the Kaplan-Meier cumulative incidence curves for each outcome for a specific campaign milestone ("Early", "Primary", "Late"), containing full stratification by each combination of level-1 and level-2 variables.
  - `last_vax_date_[level1_variable].png` depicts a histogram of the interval between the most recent previous Covid-19 vaccination (if any) and start of the vaccination campaign.
  - `prior_vax_tablee.csv` summary data for the interval between the most recent previous Covid-19 vaccination (if any) and start of the vaccination campaign.
  - `vax_count_[level1_variable].csv` summary of the number of prior Covid-19 vaccinations received at the start of the campaign, stratified by level-1 variable.
  - `vax_count_[level1_variable].png` bar-chart depicting the the number of prior Covid-19 vaccinations received at the start of the campaign, stratified by level-1 variable.

Examples of output files, using dummy data, are provided in [`assets/output-examples/`](./assets/output-examples) directory.

The actual outputed files are available from the [project workspace page on the OpenSAFELY job server site](https://jobs.opensafely.org/echo-evaluation-of-covid-19-vaccine-histories-using-opensafely/covid-vaccine-history/):
- [Released outputs](https://jobs.opensafely.org/echo-evaluation-of-covid-19-vaccine-histories-using-opensafely/covid-vaccine-history/releases/) are visible to the project team, following strict output-checking processes.
- [Published outputs](https://jobs.opensafely.org/echo-evaluation-of-covid-19-vaccine-histories-using-opensafely/covid-vaccine-history/releases/) are visible to the public. Outputs can only be published following review by [OpenSAFELY's DATAPAST function](https://docs.opensafely.org/datapast/). 

## Adding variables

To add a new variable for stratifying analyses, searching for the `--VARIABLES--` string in the codebase (ctrl+shift+F in RStudio on Windows) will indicate where any changes may need to be made.

## Outputs

The latest version of the protocol is available as [a PDF file](./assets/ECHO-WP1-protocol-v1.2.pdf). 
