
# - sentiments of solidarity ------------------------------------------------- #
# - script 05: region fixed effects ------------------------------------------ #

### note: this script 
###       (a) estimates regressions by adding region fixed effects,
###       (b) generates regression tables for comparison.

# ---------------------------------------------------------------------------- #

rm(list = ls()) # clean-up
pacman::p_load(haven, 
               modelsummary, 
               fixest,
               tidyverse)

# add the data
d <- read_sav("./data/LG_output/data_posteriors.sav")

# names
d <- d |> 
  clean_names() |> 
  mutate(class = factor(
    clu_number,
    levels = c(3, 1, 4, 2),
    labels = c("Ardent",
               "Restrictive",
               "Disengaged",
               "Moderate"))) |> 
  select(id, class)

# add region
d <- d |> 
  left_join(
    readRDS("./data/data.rds"), by = "id")

# estimates
m1 <- feols(
  alloc_ethnic ~ class +
    gender + age + ethnic + educ + relig + religiosity + party + ideology |
    city,
  data = d
)
m2 <- feols(
  alloc_anyone ~ class +
    gender + age + ethnic + educ + relig + religiosity + party + ideology |
    city,
  data = d
)
m3 <- feols(
  alloc_family ~ class +
    gender + age + ethnic + educ + relig + religiosity + party + ideology |
    city,
  data = d
)
m4 <- feols(
  alloc_friend ~ class +
    gender + age + ethnic + educ + relig + religiosity + party + ideology |
    city,
  data = d
)

modelsummary(models = list(m1, m2, m3, m4), fmt = 1)

# ---------------------------------------------------------------------------- #
