
# - sentiments of solidarity ------------------------------------------------- #
# - script 04: lm replications ----------------------------------------------- #

### note: this script 
###       (a) estimates regressions by estimating the main regressions,
###       (b) generates regression tables for comparison.

# ---------------------------------------------------------------------------- #

rm(list = ls()) # clean-up
pacman::p_load(haven, 
               modelsummary,
               ggeffects,
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
               "Moderate")
  ))

# estimates
m1 <- lm(
  alloc_ethnic ~
    class + gender + age + ethnic + educ +
    relig + religiosity + party + ideology,
  data = d
)
m2 <- lm(
  alloc_anyone ~
    class + gender + age + ethnic + educ +
    relig + religiosity + party + ideology,
  data = d
)
m3 <- lm(
  alloc_family ~
    class + gender + age + ethnic + educ +
    relig + religiosity + party + ideology,
  data = d
)
m4 <- lm(
  alloc_friend ~
    class + gender + age + ethnic + educ +
    relig + religiosity + party + ideology,
  data = d
)

modelsummary(models = list(m1, m2, m3, m4), fmt = 1)

# ---------------------------------------------------------------------------- #
