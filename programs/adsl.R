# Name: ADSL
#
# Label: Subject Level Analysis Dataset
#
# Input: dm, ex, ds
library(admiral)
library(dplyr)
library(lubridate)
library(stringr)
library(haven)

# Load source datasets ----

# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in admiral test data

dm = read_xpt("./sdtm/dm.xpt")
ds = read_xpt("./sdtm/ds.xpt")
ex = read_xpt("./sdtm/ds.xpt")
ae = read_xpt("./sdtm/ae.xpt")
lb = read_xpt("./sdtm/lb.xpt")
sv = read_xpt("./sdtm/sv.xpt")
mh = read_xpt("./sdtm/mh.xpt")
sc = read_xpt("./sdtm/sc.xpt")
qs = read_xpt("./sdtm/qs.xpt")

#Loading supplementary data sets

suppdm = read_xpt("./sdtm/suppdm.xpt")
suppds = read_xpt("./sdtm/suppdm.xpt")
suppae = read_xpt("./sdtm/suppdm.xpt")
supplb = read_xpt("./sdtm/supplb.xpt")

#Merging supplementary data sets with the source data sets.

suppdm <- suppdm %>%
  rename_at('RDOMAIN', ~'DOMAIN')

dm <- dm %>%
  derive_vars_merged(
    dataset_add = suppdm,
    new_vars = vars(QNAM, QLABEL, QVAL, QORIG, QEVAL),
    order = vars(QNAM, QLABEL, QVAL, QORIG, QEVAL),
    mode = 'first',
    by_vars = vars(STUDYID, DOMAIN, USUBJID)
  )

suppds <- suppds %>%
  rename_at('RDOMAIN', ~'DOMAIN')

ds <- ds %>%
  derive_vars_merged(
    dataset_add = suppds,
    new_vars = vars(QNAM, QLABEL, QVAL, QORIG, QEVAL),
    order = vars(QNAM, QLABEL, QVAL, QORIG, QEVAL),
    mode = 'first',
    by_vars = vars(STUDYID, DOMAIN, USUBJID)
  )

suppae <- suppae %>%
  rename_at('RDOMAIN', ~'DOMAIN')

ae <- ae %>%
  derive_vars_merged(
    dataset_add = suppae,
    new_vars = vars(QNAM, QLABEL, QVAL, QORIG, QEVAL),
    order = vars(QNAM, QLABEL, QVAL, QORIG, QEVAL),
    mode = 'first',
    by_vars = vars(STUDYID, DOMAIN, USUBJID)
  )

supplb <- supplb %>%
  rename_at('RDOMAIN', ~'DOMAIN')

lb <- lb %>%
  derive_vars_merged(
    dataset_add = supplb,
    new_vars = vars(QNAM, QLABEL, QVAL, QORIG, QEVAL),
    order = vars(QNAM, QLABEL, QVAL, QORIG, QEVAL),
    mode = 'first',
    by_vars = vars(STUDYID, DOMAIN, USUBJID)
  )


# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values

dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
lb <- convert_blanks_to_na(lb)
sv <- convert_blanks_to_na(sv)
mh <- convert_blanks_to_na(mh)
sc <- convert_blanks_to_na(sc)
qs <- convert_blanks_to_na(qs)

# User defined functions ----

# Here are some examples of how you can create your own functions that
#  operates on vectors, which can be used in `mutate`.

# Grouping
format_racegr1 <- function(x) {
  case_when(
    x == "WHITE" ~ "White",
    x != "WHITE" ~ "Non-white",
    TRUE ~ "Missing"
  )
}

format_agegr1 <- function(x) {
  case_when(
    x < 18 ~ "<18",
    between(x, 18, 64) ~ "18-64",
    x > 64 ~ ">64",
    TRUE ~ "Missing"
  )
}

format_region1 <- function(x) {
  case_when(
    x %in% c("CAN", "USA") ~ "NA",
    !is.na(x) ~ "RoW",
    TRUE ~ "Missing"
  )
}

format_lddthgr1 <- function(x) {
  case_when(
    x <= 30 ~ "<= 30",
    x > 30 ~ "> 30",
    TRUE ~ NA_character_
  )
}

# EOSSTT mapping
format_eoxxstt <- function(x) {
  case_when(
    x %in% c("COMPLETED") ~ "COMPLETED",
    !(x %in% c("COMPLETED", "SCREEN FAILURE")) & !is.na(x) ~ "DISCONTINUED",
    x %in% c("SCREEN FAILURE") ~ NA_character_,
    TRUE ~ "ONGOING"
  )
}

# Extracting relevant dm variables

adsl <- dm %>%
  select(STUDYID, USUBJID, SUBJID, SITEID, ARM, AGE, AGEU, RACE, SEX, ETHNIC, DTHFL, RFSTDTC, RFENDTC)

# Deriving TRT01P

adsl <- adsl %>%
  mutate(TRT01P = ARM)

# Deriving DSDECOD from ds

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds,
    filter_add = (DSCAT == "DISPOSITION EVENT"),
    new_vars = vars(DSDECOD),
    order = vars(DSDECOD),
    mode = "first",
    by_vars = vars(STUDYID, USUBJID)
  )

# Deriving VISNUMEN from ds

ds <- ds %>%
  mutate(VISNUMEN = case_when(VISITNUM == 13 & DSTERM == "PROTOCOL COMPLETED" ~ 12,
                              DSTERM == "PROTOCOL COMPLETED" ~ VISITNUM))

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds,
    filter_add = (DSTERM == "PROTOCOL COMPLETED"),
    new_vars = vars(VISNUMEN),
    order = vars(VISNUMEN),
    mode = "first",
    by_vars = vars(STUDYID, USUBJID)
  )

# Deriving TRTEDT from ex

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex,
    filter_add = (DSDECOD == "FINAL LAB VISIT"),
    new_vars = vars(TRTEDT = DSSTDTC),
    order = vars(DSSTDTC),
    mode = "first",
    by_vars = vars(STUDYID, USUBJID)
  )

adsl <- adsl %>%
  xportr_write("./adam/adsl2.xpt", label = "Subject-Level Analysis Dataset")
