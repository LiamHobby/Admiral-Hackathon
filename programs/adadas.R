library(haven)
library(admiral)
library(dplyr)




qs = read_xpt("./sdtm/qs.xpt")

adsl = read_xpt("./adam/adsl.xpt")

qs <- convert_blanks_to_na(qs)

adadas <- qs %>%
  select(USUBJID, QSDTC)

adsl_trtsdt <- adsl %>%
  select(USUBJID, TRTSDT)

TRTSDT = vector("character", 0)

for(i in 1:row(adadas))
{
  for(j in 1:row(adsl_trtsdt))
  {
    if(adadas[i, 1] == adsl_trtsdt[j, 1])
    {
      TRTSDT = c(TRTSDT, adsl_trtsdt[j, 2])
    }
  }
}



adadas <- adadas %>%
  rename(ADT = QSDTC) %>%
  mutate(ADY = case_when(ADT >= TRTSDT ~ compute_duration(
    TRTSDT,
    ADT,
    in_unit = "days",
    out_unit = "days",
    floor_in = TRUE,
    add_one = TRUE,
    trunc_out = FALSE
  ),
  ADT < TRTSDT ~ compute_duration(
    TRTSDT,
    ADT,
    in_unit = "days",
    out_unit = "days",
    floor_in = TRUE,
    add_one = FALSE,
    trunc_out = FALSE
  )))
