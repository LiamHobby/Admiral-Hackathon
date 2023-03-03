# Name: ADSL
#
# Label: Subject Level Analysis Dataset
#
# Input: dm, ds, ex, sv, mh, sc, vs, qs
library(admiral)
library(dplyr)
library(haven)

# Load source datasets ----

dm = read_xpt("./sdtm/dm.xpt")
ds = read_xpt("./sdtm/ds.xpt")
ex = read_xpt("./sdtm/ex.xpt")
sv = read_xpt("./sdtm/sv.xpt")
mh = read_xpt("./sdtm/mh.xpt")
sc = read_xpt("./sdtm/sc.xpt")
vs = read_xpt("./sdtm/vs.xpt")
qs = read_xpt("./sdtm/qs.xpt")

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

### Extracting relevant dm variables

adsl <- dm %>%
  select(STUDYID, USUBJID, SUBJID, SITEID, ARM, AGE, AGEU, RACE, SEX, ETHNIC, DTHFL, RFSTDTC, RFENDTC, ARMCD)

### Deriving TRT01P

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
    new_vars = vars(DCDECOD = DSDECOD, VISNUMEN, DCREASCD = DSTERM),
    order = vars(DSDECOD, VISNUMEN, DSTERM),
    mode = "first",
    by_vars = vars(STUDYID, USUBJID)
  )

# Deriving TRTEDT from ex

ex <- ex %>%
  derive_var_extreme_flag(
    by_vars = vars(USUBJID),
    order = vars(EXENDTC),
    new_var = LAST_SUBJECT_FL,
    mode = "last"
  )

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex,
    filter_add = (LAST_SUBJECT_FL == 'Y'),
    new_vars = vars(TRTEDT = EXENDTC),
    order = vars(EXENDTC),
    mode = "first",
    by_vars = vars(USUBJID)
  ) %>% # Creating sv varables to add to the adsl
  derive_vars_merged(
    dataset_add = sv,
    filter_add = (VISITNUM == 3),
    new_vars = vars(TRTSDT = SVSTDTC),
    order = vars(SVSTDTC),
    mode = "first",
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = sv,
    filter_add = (VISITNUM == 1),
    new_vars = vars(VISIT1DT = SVSTDTC),
    order = vars(SVSTDTC),
    mode = "first",
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  derive_var_merged_exist_flag(
    dataset_add = sv,
    by_vars = vars(STUDYID, USUBJID),
    new_var = COMP16FL,
    condition = (VISITNUM == 10 & VISITDY >= 70),
    true_value = 'Y',
    false_value = 'N',
    missing_value = NA_character_
  ) %>%
  derive_var_merged_exist_flag(
    dataset_add = sv,
    by_vars = vars(STUDYID, USUBJID),
    new_var = COMP24FL,
    condition = (VISITNUM == 12 & VISITDY >= 84),
    true_value = 'Y',
    false_value = 'N',
    missing_value = NA_character_
  ) %>%
  derive_var_merged_exist_flag(
    dataset_add = sv,
    by_vars = vars(STUDYID, USUBJID),
    new_var = COMP8FL,
    condition = (VISITNUM == 8 & VISITDY >= 56),
    true_value = 'Y',
    false_value = 'N',
    missing_value = NA_character_
  ) %>% # Deriving DISONSDT from mh
  derive_vars_merged(
    dataset_add = mh,
    filter_add = (MHCAT == "PRIMARY DIAGNOSIS"),
    new_vars = vars(DISONSDT = MHSTDTC),
    order = vars(MHSTDTC),
    mode = 'first',
    by_vars = vars(STUDYID, USUBJID)
  ) %>% # Deriving EDUCLVL from sc
  derive_vars_merged(
    dataset_add = sc,
    filter_add = (SCTESTCD == "EDLEVEL"),
    new_vars = vars(EDUCLVL = SCSTRESN),
    order = vars(SCSTRESN),
    mode = 'first',
    by_vars = vars(STUDYID, USUBJID)
  ) %>% # Deriving variables from vs
  derive_vars_merged(
    dataset_add = vs,
    filter_add = (VSTESTCD == "HEIGHT" & VISITNUM == 1),
    new_vars = vars(HEIGHTBL = VSSTRESN),
    order = vars(VSSTRESN),
    mode = 'first',
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = vs,
    filter_add = (VSTESTCD == "WEIGHT" & VISITNUM == 3),
    new_vars = vars(WEIGHTBL = VSSTRESN),
    order = vars(VSSTRESN),
    mode = 'first',
    by_vars = vars(STUDYID, USUBJID)
  )

# Deriving MSSETOT form qs

qs_MSSETOT <- qs %>%
  filter(QSCAT == "MINI-MENTAL STATE") %>%
  group_by(USUBJID) %>%
  summarise(MMSETOT = sum(as.numeric(QSORRES), na.rm = TRUE)) %>%
  select(USUBJID, MMSETOT) %>%
  ungroup()

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = qs_MSSETOT,
    new_vars = vars(MMSETOT),
    order = vars(MMSETOT),
    mode = 'first',
    by_vars = vars(USUBJID)
  )

qs <- qs %>%
  select(USUBJID, QSCAT, VISITNUM)

USUBJID = vector("character", 0)
flag_ADASCog = vector("character", 0)
flag_CIBIC = vector("character", 0)

is_ADASCog = FALSE
is_CIBIC = FALSE

for(i in 1:nrow(qs))
{
  if(i == 1)
  {
    subject = qs[1, 1]
    USUBJID = c(subject)
  }

  if(qs[i, 3] == "ALZHEIMER'S DISEASE ASSESSMENT SCALE" & qs[i, 3] > 3 & is_ADASCog == FALSE)
  {
    flag_ADASCog = c(flag_ADASCog, 'Y')
    is_ADASCog = TRUE
  }

  if(qs[i, 3] == "CLINICIAN'S INTERVIEW-BASED IMPRESSION OF CHANGE (CIBIC+)" & qs[i, 3] > 3 & is_CIBIC == FALSE)
  {
    flag_CIBIC = c(flag_CIBIC, 'Y')
    is_CIBIC = TRUE
  }

  if(qs[i, 1] != subject)
  {
    subject = qs[i, 1]
    USUBJID = c(USUBJID, subject)

    if(is_ADASCog == TRUE)
    {
      is_ADASCog = FALSE
    }
    else
    {
      flag_ADASCog = c(flag_ADASCog, 'N')
    }

    if(is_CIBIC == TRUE)
    {
      is_CBIC = FALSE
    }
    else
    {
      flag_CIBIC = c(flag_CIBIC, 'N')
    }
  }
}

if(is_ADASCog == TRUE)
{
  is_ADASCog = FALSE
} else
{
  flag_ADASCog = c(flag_ADASCog, 'N')
}

if(is_CIBIC == TRUE)
{
  is_CBIC = FALSE
} else
{
  flag_CIBIC = c(flag_CIBIC, 'N')
}

USUBJID <- paste(USUBJID)

qs_adsl <- cbind(USUBJID, flag_ADASCog, flag_CIBIC)

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = as.data.frame(qs_adsl),
    new_vars = vars(flag_ADASCog, flag_CIBIC),
    order = vars(flag_ADASCog, flag_CIBIC),
    mode = 'first',
    by_vars = vars(USUBJID)
  ) %>% # Deriving VISIT4DT from sv to derive CUMDOSE
  derive_vars_merged(
    dataset_add = sv,
    filter_add = (VISITNUM == 4),
    new_vars = vars(VISIT4DT = SVSTDTC),
    order = vars(SVSTDTC),
    mode = 'first',
    by_vars = vars(STUDYID, USUBJID)
  ) %>% # Deriving VISIT12DT from sv to derive CUMDOSE
  derive_vars_merged(
    dataset_add = sv,
    filter_add = (VISITNUM == 12),
    new_vars = vars(VISIT12DT = SVSTDTC),
    order = vars(SVSTDTC),
    mode = 'first',
    by_vars = vars(STUDYID, USUBJID)
  ) %>% # Deriving DURDIS
  mutate(VISIT1DT = as.Date(VISIT1DT),
         VISIT4DT = as.Date(VISIT4DT),
         VISIT12DT = as.Date(VISIT12DT),
         DISONSDT = as.Date(DISONSDT),
         TRTSDT = as.Date(TRTSDT),
         TRTEDT = as.Date(TRTEDT))

adsl$DURDIS <- compute_duration(
  adsl$DISONSDT,
  adsl$VISIT1DT,
  in_unit = "days",
  out_unit = "months",
  floor_in = TRUE,
  add_one = TRUE,
  trunc_out = FALSE
)

# Deriving TRTDURD

adsl$TRTDURD <- compute_duration(
  adsl$TRTSDT,
  adsl$TRTEDT,
  in_unit = "days",
  out_unit = "days",
  floor_in = TRUE,
  add_one = TRUE,
  trunc_out = FALSE
)

# Deriving variables used for caluculating CUMDOSE

adsl$Interval_1 <- compute_duration(
  adsl$TRTSDT,
  adsl$VISIT4DT,
  in_unit = "days",
  out_unit = "days",
  floor_in = TRUE,
  add_one = TRUE,
  trunc_out = FALSE
)

adsl$Interval_1_Discontinued <- compute_duration(
  adsl$TRTSDT,
  adsl$TRTEDT,
  in_unit = "days",
  out_unit = "days",
  floor_in = TRUE,
  add_one = TRUE,
  trunc_out = FALSE
)

adsl$Interval_2 <- compute_duration(
  adsl$VISIT4DT,
  adsl$VISIT12DT,
  in_unit = "days",
  out_unit = "days",
  floor_in = TRUE,
  add_one = FALSE,
  trunc_out = FALSE
)

adsl$Interval_3 <- compute_duration(
  adsl$VISIT12DT,
  adsl$TRTEDT,
  in_unit = "days",
  out_unit = "days",
  floor_in = TRUE,
  add_one = FALSE,
  trunc_out = FALSE
)

adsl$Interval_2_Discontinued <- compute_duration(
  adsl$VISIT4DT,
  adsl$TRTEDT,
  in_unit = "days",
  out_unit = "days",
  floor_in = TRUE,
  add_one = FALSE,
  trunc_out = FALSE
)

# Deriving more variables

adsl <- adsl %>%
  mutate(AGEGR1N = case_when(AGE < 65 ~ 1,
                            AGE >= 65 & AGE <= 80 ~ 2,
                            AGE > 80 ~ 3),
         AGEGR1 = case_when(AGEGR1N == 1 ~ "<65",
                            AGEGR1N == 2 ~ "65-80",
                            AGEGR1N == 3 ~ ">80"),
         ARMN = case_when(ARM == "Placebo" ~ 0,
                          ARM == "Xanomeline Low Dose" ~ 1,
                          ARM == "Xanomeline High Dose" ~ 2),
         TRT01PN = case_when(ARMN == 0 ~ 2,
                             ARMN == 1 ~ 54,
                             ARMN == 2 ~ 81),
         EOSSTT = case_when(DCDECOD == "COMPLETED" ~ "COMPLETED",
                            DCDECOD != "COMPLETED" ~ "DISCONTINUED"),
         CUMDOSE = case_when(ARMN == 0 ~ 0,
                             ARMN == 1 ~ TRT01PN * TRTDURD,
                             ARMN == 2 ~ case_when(VISNUMEN > 3 & VISNUMEN <= 4 ~ case_when(EOSSTT == "COMPLETED" ~ 54 * Interval_1,
                                                                                            EOSSTT == "DISCONTINUED" ~ 54 * Interval_1_Discontinued),
                                                   VISNUMEN > 4 & VISNUMEN <= 12 ~ case_when(EOSSTT == "COMPLETED" ~ 54 * Interval_1 + 81 * Interval_2 + 54 * Interval_3,
                                                                                             EOSSTT == "DISCONTINUED" ~ 54 * Interval_1 + 81 * Interval_2_Discontinued),
                                                   VISNUMEN > 12 ~ 54 * Interval_1 + 81 * Interval_2 + 54 * Interval_3)),
         AVGDD = CUMDOSE / TRTDURD,
         BMIBL = WEIGHTBL / ((HEIGHTBL / 100) ^ 2),
         BMIBLGR1 = case_when(BMIBL < 25 ~ "<25",
                              BMIBL >= 25 & BMIBL < 30 ~ "25-<30",
                              BMIBL >= 30 ~ ">=30"),
         DISCONFL = case_when(DCREASCD == "PROTOCOL COMPLETED" ~ 'Y'),
         DSRAEFL = case_when(DCREASCD == "ADVERSE EVENT" ~ 'Y'),
         DURDSGR1 = case_when(DURDIS < 12 ~ "<12",
                              DURDIS >= 12 ~ ">=12"),
         ITTFL = case_when(ARMCD != '' ~ 'Y',
                           ARMCD == '' ~ 'N'),
         SAFFL = case_when(ITTFL == 'Y' & is.na(TRTSDT) == FALSE ~ 'Y',
                           TRUE ~ 'N'),
         EFFFL = case_when(SAFFL == 'Y' & flag_ADASCog == 'Y' & flag_CIBIC == 'Y' ~ 'Y',
                           TRUE ~ 'N'),
         RACEN = case_when(RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
                           RACE == "ASIAN - CENTRAL/SOUTH ASIAN HERITAGE" ~ 2,
                           RACE == "ASIAN - EAST ASIAN HERITAGE" ~ 3,
                           RACE == "ASIAN - JAPANESE HERITAGE" ~ 4,
                           RACE == "ASIAN - SOUTH EAST ASIAN HERITAGE" ~ 5,
                           RACE == "BLACK OR AFRICAN AMERICAN" ~ 6,
                           RACE == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 7,
                           RACE == "WHITE" ~ 8,
                           RACE == "MIXED ASIAN RACE" ~ 10,
                           RACE == "MIXED WHITE RACE" ~ 11,
                           RACE == "MIXED RACE" ~ 12),
         RFENDT = format(as.Date(RFENDTC), "%d-%b-%Y"),
         SITEGR1 = SITEID,
         TRT01A = TRT01P,
         TRT01AN = TRT01PN,
         RFSTDTC = format(as.Date(RFSTDTC), "%d-%b-%Y"),
         RFENDTC = format(as.Date(RFENDTC), "%d-%b-%Y"),
         TRTSDT = format(as.Date(TRTSDT), "%d-%b-%Y"),
         TRTEDT = format(as.Date(TRTEDT), "%d-%b-%Y"),
         VISIT1DT = format(as.Date(VISIT1DT), "%d-%b-%Y"),
         VISIT4DT = format(as.Date(VISIT4DT), "%d-%b-%Y"),
         VISIT12DT = format(as.Date(VISIT12DT), "%d-%b-%Y"),
         DISONSDT = format(as.Date(DISONSDT), "%d-%b-%Y")) %>% # Keeping only relevant adsl variables
  select(STUDYID, USUBJID, SUBJID, SITEID, SITEGR1, ARM, TRT01P, TRT01PN, TRT01A, TRT01AN, TRTSDT, TRTEDT, TRTDURD, AVGDD, CUMDOSE, AGE, AGEGR1, AGEGR1N,
         AGEU, RACE, RACEN, SEX, ETHNIC, SAFFL, ITTFL, EFFFL, COMP8FL, COMP16FL, COMP24FL, DISCONFL, DSRAEFL, DTHFL, BMIBL, BMIBLGR1, HEIGHTBL, WEIGHTBL,
         EDUCLVL, DISONSDT, DURDIS, DURDSGR1, VISIT1DT, RFSTDTC, RFENDTC, VISNUMEN, RFENDT, DCDECOD, EOSSTT, DCREASCD, MMSETOT) %>% # Removing Screen Failures
  subset(ARM != "Screen Failure") %>% # Exporting adsl
  write_xpt(path = "./adam/adsl2.xpt", label = "Subject-Level Analysis Dataset")
