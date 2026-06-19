# Purpose:
# define codelist objects from codelist files imported by codelist.txt spec

# Import code building blocks from cohort extractor package
from ehrql import codelist_from_csv


## --VARIABLES--
# if the variable uses a codelist then it should be added below
# after updating the codelist.txt configuration and importing the codelist

# Ethnicity

ethnicity5 = codelist_from_csv(
  "codelists/opensafely-ethnicity-snomed-0removed.csv",
  column="code",
  category_column="Label_6", # it's 6 because there is an additional "6 - Not stated" but this is not represented in SNOMED, instead corresponding to no ethnicity code
)

ethnicity16 = codelist_from_csv(
  "codelists/opensafely-ethnicity-snomed-0removed.csv",
  column="code",
  category_column="Label_16",
)

#######################################################
# PRIMIS
#######################################################

#Asthma
## Asthma Diagnosis code
ast = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-ast.csv",
  column="code",
)

## Asthma Admission codes
astadm = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-astadm.csv",
  column="code",
)

## Asthma inhaler or nebuliser medication codes
astrxm1 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-astrxm1.csv",
  column="code",
)

## Asthma systemic steroid medication codes
astrxm2 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-astrxm2.csv",
  column="code",
)

# Chronic Respiratory Disease
resp_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-resp_cov.csv",
  column="code",
)

# Chronic heart disease codes
chd_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-chd_cov.csv",
  column="code",
)

# CKD
## Chronic kidney disease diagnostic codes
ckd_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-ckd_cov.csv",
  column="code",
)

## Chronic kidney disease codes - all stages
ckd15 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-ckd15.csv",
  column="code",
)

## Chronic kidney disease codes-stages 3 - 5
ckd35 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-ckd35.csv",
  column="code",
)
 ### Subsets ckd35 by stages 3-5
ckd3_snomed = ["129171000119106","140121000119100","284991000119104","285871000119106","324251000000105","324281000000104","324311000000101","324341000000100","324371000000106","324411000000105","368441000119102","433144002","691421000119108","700378005","700379002","731000119105","90741000119107","949881000000106","949901000000109","949921000000100","950061000000103","950081000000107","950101000000101","96731000119100"]

ckd4_snomed = ["129151000119102","140111000119107","285001000119105","285881000119109","324441000000106","324471000000100","368451000119100","431857002","721000119107","90751000119109","950181000000106","950211000000107","950231000000104","96721000119103"]

ckd5_snomed = ["129161000119100","140101000119109","153851000119106","285011000119108","324501000000107","324541000000105","368461000119103","433146000","711000119100","714152005","714153000","90761000119106","950251000000106","950291000000103","950311000000102","96711000119105"]



# Chronic Liver disease codes
cld = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-cld.csv",
  column="code",
)

# DB
## Diabetes diagnosis codes
diab = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-diab.csv",
  column="code",
)

## Diabetes resolved codes
dmres = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-dmres.csv",
  column="code",
)

## Gestational diabetes diagnosis codes
gdiab = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-gdiab_cod.csv",
  column = "code",
)

# Addisons disease and hypoadrenalism diagnosis codes
addis = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-addis_cod.csv",
  column = "code",
)

# Pregnancy delivery codes
pregdel = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-pregdel.csv",
  column = "code",
)

# Pregnancy codes
preg = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-preg.csv",
  column = "code",
)

# Severe Mental Illness codes
sev_mental = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-sev_mental.csv",
  column="code",
)

# Remission codes relating to Severe Mental Illness
smhres = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-smhres.csv",
  column="code",
)

# Chronic Neurological Disease including Significant Learning Disorder
cns_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-cns_cov.csv",
  column="code",
)

# Immunosuppression diagnosis codes
immdx_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-immdx_cov.csv",
  column="code",
)

# Immunosuppression medication codes
immrx = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-immrx.csv",
  column="code",
)

# Immunosuppression admin codes
immadm = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-immunosuppression-admin-codes.csv",
  column="code",
)

# Chemotherapy or radiation (Primis)
dxt_chemo = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-dxt_chemo_cod.csv",
  column="code",
)

# Chronic Neurological Disease including Significant Learning Disorder
cns_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-cns_cov.csv",
  column="code",
)

# Asplenia or Dysfunction of the Spleen codes
spln_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-spln_cov.csv",
  column="code",
)

# BMI
bmi = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-bmi.csv",
  column="code",
)

# All BMI coded terms
bmi_stage = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-bmi_stage.csv",
  column="code",
)

# Severe Obesity code recorded
sev_obesity = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-sev_obesity.csv",
  column="code",
)

# Wider Learning Disability
learndis = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-learndis.csv",
  column="code",
)


# Cancer

cancer_haem_snomed=codelist_from_csv(
    "codelists/opensafely-haematological-cancer-snomed.csv",
    column="id",
)

cancer_nonhaem_nonlung_snomed=codelist_from_csv(
    "codelists/opensafely-cancer-excluding-lung-and-haematological-snomed.csv",
    column="id",
)

cancer_lung_snomed=codelist_from_csv(
    "codelists/opensafely-lung-cancer-snomed.csv",
    column="id",
)

chemotherapy_radiotherapy_snomed = codelist_from_csv(
  "codelists/opensafely-chemotherapy-or-radiotherapy-snomed.csv", 
  column = "id"
)

cancer_nonhaem_snomed = (
  cancer_nonhaem_nonlung_snomed + 
  cancer_lung_snomed + 
  chemotherapy_radiotherapy_snomed
)

# solid organ transplant
solid_organ_transplant=codelist_from_csv(
    "codelists/opensafely-solid-organ-transplantation-snomed.csv",
    column="id",
)

# HIV/AIDS
hiv_aids=codelist_from_csv(
    "codelists/nhsd-hiv-aids-snomed.csv",
    column="code",
)


# Patients in long-stay nursing and residential care
carehome_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-longres.csv", 
    column="code",
)
carehome_nhs_refset = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-longres.csv", 
    column="code",
)

# COVID-19 
covid_icd10 = ["U071", "U072", "U109"]

# Extended subgroups

dialysis = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-dialysis_cod.csv",
    column="code",
)

kidney_transplant = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-renaltransp_cod.csv",
    column="code",
)

creatinine = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-cre_cod.csv",
    column="code",
)

copd = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-copd_cod.csv", 
    column="code",
)

down_syndrome = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-ds_cod.csv", 
    column="code",
)

learndis_register = ["416075005"]

learndis_excl_register = set(learndis) - set(learndis_register)

sickle_cell = codelist_from_csv(
    "codelists/nhsd-sickle-spl-atriskv4-snomed-ct.csv", 
    column="code",
)

cirrhosis = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-cirrhosis_cod.csv", 
    column="code",
)


cochlear_implant = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-cocimpatrisk1_cod.csv", 
    column="code",
)

remove_cochlear_implant = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-cocimprematrisk1_cod.csv", 
    column="code",
)

cystic_fibrosis = codelist_from_csv(
    "codelists/opensafely-cystic-fibrosis-snomed.csv", 
    column="code",
)

csfl = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-csflatrisk1_cod.csv", 
    column="code",
)

homeless = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-homeless_cod.csv", 
    column="code",
)

reside = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-reside_cod.csv", 
    column="code",
)

