
# - sentiments of solidarity ------------------------------------------------- #
# - script 02: clean ISSP ---------------------------------------------------- #

### note: this script 
###       (a) cleans the Turkish ISSP data and saves it in `sav` format.

# ---------------------------------------------------------------------------- #

rm(list = ls()) # clean-up
pacman::p_load(labelled,
               haven,
               tidyverse)

d <- haven::read_dta(
  "./data/issp/issp_turkey.dta") |>
  mutate(id = 1:1666) |> 
  haven::zap_label() |> 
  labelled::remove_labels() |>  
  select(
    id,
    natident1 = V7,
    natmembr1 = V9,
    natmembr2 = V10,
    natmembr3 = V11,
    natmembr4 = V12,
    natmembr5 = V13,
    natmembr6 = V14,
    natmembr7 = V15,
    natmembr8 = V16,
    nathubrs1 = V17,
    nathubrs2 = V18,
    nathubrs3 = V19,
    nathubrs4 = V20,
    nathubrs5 = V21,
    natpride1 = V25,
    natpride2 = V26,
    natpride3 = V27,
    natpride4 = V28,
    natpride5 = V29,
    natpride6 = V30,
    natpride7 = V31,
    natpride8 = V32,
    natpride9 = V33,
    natpride10 = V34
  ) |>
  mutate(across(!id, ~ recode(.x, "8" = NA_real_, "9" = NA_real_)))

haven::write_sav(d, path = 
                   "./data/issp/issp_turkey.sav")

# ------------------------------------------------------------------------------------- #
