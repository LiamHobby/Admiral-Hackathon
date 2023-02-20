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
sv = read_xpt("./sdtm/sv.xpt")
mh = read_xpt("./sdtm/mh.xpt")
sc = read_xpt("./sdtm/sc.xpt")
vs = read_xpt("./sdtm/vs.xpt")
qs = read_xpt("./sdtm/qs.xpt")

#Loading supplementary data sets

suppdm = read_xpt("./sdtm/suppdm.xpt")
suppds = read_xpt("./sdtm/suppdm.xpt")

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
  mutate(RDOMAIN = "DS")

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

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values

dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
sv <- convert_blanks_to_na(sv)
mh <- convert_blanks_to_na(mh)
sc <- convert_blanks_to_na(sc)
vs <- convert_blanks_to_na(vs)
qs <- convert_blanks_to_na(qs)

# Extracting relevant dm variables

adsl <- dm %>%
  select(STUDYID, USUBJID, SUBJID, SITEID, ARM, AGE, AGEU, RACE, SEX, ETHNIC, DTHFL, RFSTDTC, RFENDTC)

# Deriving TRT01P

adsl <- adsl %>%
  mutate(TRT01P = ARM)

# Deriving VISNUMEN from ds

ds <- ds %>%
  mutate(VISNUMEN = case_when(VISITNUM == 13 & DSTERM == "PROTOCOL COMPLETED" ~ 12,
                              TRUE ~ VISITNUM))

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds,
    filter_add = (DSCAT == "DISPOSITION EVENT"),
    new_vars = vars(DSDECOD, VISNUMEN),
    order = vars(DSDECOD, VISNUMEN),
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

# Converting TRTEDT to SAS Date Format

adsl <- adsl %>%
  mutate(TRTEDT = format(as.Date(TRTEDT), "%d-%b-%Y"))

# Creating sv variables to add to the adsl

sv <- sv %>%
  mutate(COMP16FL = case_when(VISITNUM == 10 & VISITDY >= 70 ~ 'Y',
                              TRUE ~ 'N'),
         COMP24FL = case_when(VISITNUM == 12 & VISITDY >= 84 ~ 'Y',
                              TRUE ~ 'N'),
         COMP8FL = case_when(VISITNUM == 8 & VISITDY >= 56 ~ 'Y',
                             TRUE ~ 'N'),
         TRTSDT = case_when(VISITNUM == 3 ~ SVSTDTC),
         VISIT1DT = case_when(VISITNUM == 1 ~ SVSTDTC))

# Extracting relevant sv variables

sv <- sv %>%
  select(USUBJID, COMP16FL, COMP24FL, COMP8FL, TRTSDT, VISIT1DT)

# Initializing columns to add to the adsl

USUBJID = vector("character", 0)
COMP16FL = vector("character", 0)
COMP24FL = vector("character", 0)
COMP8FL = vector("character", 0)
TRTSDT = vector("character", 0)
VISIT1DT = vector("character", 0)

# The Boolean flags ensure that no more than 1 record is added for a subject

COMP16FL_b = FALSE
COMP24FL_b = FALSE
COMP8FL_b = FALSE
TRTSDT_b = FALSE
VISIT1DT_b = FALSE

# This nested loop goes through every element in the sv data set and extract relevant data
# i is row and j is column

for(i in 1:nrow(sv))
{
  for(j in 1:ncol(sv))
  {
    if(i == 1 & j == 1)
    {
      subject = sv[i, j]
      USUBJID <- c(subject)

      COMP16FL_b = FALSE
      COMP24FL_b = FALSE
      COMP8FL_b = FALSE
      TRTSDT_b = FALSE
      VISIT1DT_b = FALSE
    }

    if(j == 1 & sv[i, j] != subject)
    {
      if(COMP16FL_b == FALSE)
      {
        COMP16FL <- c(COMP16FL, 'N')
      }

      if(COMP24FL_b == FALSE)
      {
        COMP24FL <- c(COMP24FL, 'N')
      }

      if(COMP8FL_b == FALSE)
      {
        COMP8FL <- c(COMP8FL, 'N')
      }

      if(TRTSDT_b == FALSE)
      {
        TRTSDT <- c(TRTSDT, NA)
      }

      if(VISIT1DT_b == FALSE)
      {
        VISIT1DT <- c(VISIT1DT, NA)
      }

      subject = sv[i, j]
      USUBJID = c(USUBJID, subject)

      COMP16FL_b = FALSE
      COMP24FL_b = FALSE
      COMP8FL_b = FALSE
      TRTSDT_b = FALSE
      VISIT1DT_b = FALSE
    }

    if(sv[i, 2] == 'Y' & COMP16FL_b == FALSE)
    {
      COMP16FL <- c(COMP16FL, 'Y')
      COMP16FL_b = TRUE
    }

    if(sv[i, 3] == 'Y' & COMP24FL_b == FALSE)
    {
      COMP24FL <- c(COMP24FL, 'Y')
      COMP24FL_b = TRUE
    }

    if(sv[i, 4] == 'Y' & COMP8FL_b == FALSE)
    {
      COMP8FL <- c(COMP8FL, 'Y')
      COMP8FL_b = TRUE
    }

    if(is.na(sv[i, 5]) == FALSE & TRTSDT_b == FALSE)
    {
      TRTSDT <- c(TRTSDT, sv[i, 5])
      TRTSDT_b = TRUE
    }

    if(is.na(sv[i, 6]) == FALSE & VISIT1DT_b == FALSE)
    {
      VISIT1DT = c(VISIT1DT, sv[i, 6])
      VISIT1DT_b = TRUE
    }
  }
}

if(COMP16FL_b == FALSE)
{
  COMP16FL <- c(COMP16FL, 'N')
}

if(COMP24FL_b == FALSE)
{
  COMP24FL <- c(COMP24FL, 'N')
}

if(COMP8FL_b == FALSE)
{
  COMP8FL <- c(COMP8FL, 'N')
}

if(TRTSDT_b == FALSE)
{
  TRTSDT <- c(TRTSDT, NA)
}

if(VISIT1DT_b == FALSE)
{
  VISIT1DT <- c(VISIT1DT, NA)
}

# Converting lists in to characters because the adsl columns are characters

USUBJID <- paste(USUBJID)
TRTSDT <- paste(TRTSDT)
VISIT1DT <-paste(VISIT1DT)

# Creating the data set to merge with the adsl

sv_adsl <- cbind(USUBJID, COMP16FL, COMP24FL, COMP8FL, TRTSDT, VISIT1DT)

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = as.data.frame(sv_adsl),
    new_vars = vars(COMP16FL, COMP24FL, COMP8FL, TRTSDT, VISIT1DT),
    order = vars(COMP16FL, COMP24FL, COMP8FL, TRTSDT, VISIT1DT),
    mode = 'first',
    by_vars = vars(USUBJID)
  )

# Formatting the newly added date variables

adsl <- adsl %>%
  mutate(TRTSDT = format(as.Date(TRTSDT), "%d-%b-%Y"),
         VISIT1DT = format(as.Date(VISIT1DT), "%d-%b-%Y"))

# Deriving DISONSDT from mh

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = mh,
    filter_add = (MHCAT == "PRIMARY DIAGNOSIS"),
    new_vars = vars(DISONSDT = MHSTDTC),
    order = vars(MHSTDTC),
    mode = 'first',
    by_vars = vars(STUDYID, USUBJID)
  )

adsl <- adsl %>%
  mutate(DISONSDT = format(as.Date(DISONSDT), "%d-%b-%Y"))

# Deriving EDUCLVL from sc

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = sc,
    filter_add = (SCTESTCD == "EDLEVEL"),
    new_vars = vars(EDUCLVL = SCSTRESN),
    order = vars(SCSTRESN),
    mode = 'first',
    by_vars = vars(STUDYID, USUBJID)
  )

# Deriving variables from vs

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = vs,
    filter_add = (VSTESTCD == "HEIGHT" & VISITNUM == 1),
    new_vars = vars(HEIGHTBL = VSSTRESN),
    order = vars(VSSTRESN),
    mode = 'first',
    by_vars = vars(STUDYID, USUBJID)
  )

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = vs,
    filter_add = (VSTESTCD == "WEIGHT" & VISITNUM == 3),
    new_vars = vars(WEIGHTBL = VSSTRESN),
    order = vars(VSSTRESN),
    mode = 'first',
    by_vars = vars(STUDYID, USUBJID)
  )

# Deriving MSSETOT form qs

qs <- qs %>%
  select(USUBJID, QSORRES, QSCAT)

qs <- qs %>%
  mutate(QSORRES = as.integer(QSORRES))

USUBJID = vector("character", 0)
MMSETOT = vector("integer", 0)
n_MMSETOT = 0

for(i in 1:nrow(qs))
{
  if(i == 1)
  {
    subject = qs[1, 1]
    USUBJID = c(subject)
  }

  if(qs[i, 3] == "MINI-MENTAL STATE")
  {
    n_MMSETOT = n_MMSETOT + qs[i, 2]
  }

  if(qs[i, 1] != subject)
  {
    MMSETOT = c(MMSETOT, n_MMSETOT)
    subject = qs[i, 1]
    USUBJID = c(USUBJID, subject)
    n_MMSETOT = 0
  }
}

MMSETOT = c(MMSETOT, n_MMSETOT)

USUBJID <- paste(USUBJID)
MMSETOT <- paste(MMSETOT)

qs_adsl <- cbind(USUBJID, MMSETOT)

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = as.data.frame(qs_adsl),
    new_vars = vars(MMSETOT),
    order = vars(MMSETOT),
    mode = 'first',
    by_vars = vars(USUBJID)
  )

#adsl <- adsl %>%
#  mutate(AGEGR1N = case_when(AGE < 65 ~ 1,
#                            AGE >= 65 & AGE <= 80 ~ 2,
#                            AGE > 80 ~ 3),
#         AGEGR1 = case_when(AGEGR1N == 1 ~ "<65",
#                            AGEGR1N == 2 ~ "65-80",
#                            AGEGR1N == 3 ~ ">80"),
#         ARMN = case_when(ARM == "Placebo" ~ 0,
#                          ARM == "Xanomeline Low Dose" ~ 1,
#                          ARM == "Xanomeline High Dose" ~ 2),
#         TRT01PN = ARMN,
#         TRTDUR = TRTEDT - TRTSDT + 1,
#         )

adsl <- adsl %>%
  xportr_write("./adam/adsl2.xpt", label = "Subject-Level Analysis Dataset")
