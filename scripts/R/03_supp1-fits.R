
# - sentiments of solidarity ------------------------------------------------- #
# - script 03: comparisons --------------------------------------------------- #

### note: this script
###       (a) uses the ISSP data to construct latent classes,
###       (b) compares class structure to the TVNS.

# ---------------------------------------------------------------------------- #

rm(list = ls()) # clean-up
pacman::p_load(janitor,
               haven,
               hrbrthemes,
               modelsummary,
               tidyverse)

# TVNS project
d.tvns <- haven::read_sav(
  "./data/LG_output/data_posteriors.sav") |> 
  clean_names()

# ISSP project
d.issp <- haven::read_sav(
  "./data/LG_output/data_posteriors_ISSP.sav") |> 
  clean_names()

# Latent GOLD results
d.prob <- read.csv(
  "./data/LG_output/lca_probabilities-ISSP.csv")
d.fits <- read.csv(
  "./data/LG_output/lca_fit-stats-ISSP.csv")

# model fits
png(
  "./figures/supplementals_ISSP01.png",
  w = 8, 
  h = 6,
  units = "in",
  res = 500
)
d.fits |>
  ggplot(aes(x = factor(cluster), y = bic)) +
  geom_line(aes(group = factor(resid)),
            linewidth = .25,
            linetype = "dashed") +
  geom_line(aes(group = factor(alter)),
            linewidth = .25,
            linetype = "dashed") +
  geom_point(aes(color = factor(resid)), size = 2.5) +
  labs(x = "The Number of Classes", y = "Bayesian Information Criteria",
       color = "Residuals Allowed to Vary") +
  scale_color_manual(values = c("#A4C9D7", "#E4CA99")) +
  theme_ipsum_rc() + theme(legend.position = "bottom") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))
dev.off()

# latent classes
png(
  "./figures/supplementals_ISSP02.png",
  w = 12, 
  h = 6,
  units = "in",
  res = 500
)
d.prob |>
  
  mutate(outcome = recode(
    outcome,
    "1" = 5,
    "2" = 4,
    "3" = 3,
    "4" = 2,
    "5" = 1
  )) |>
  
  ## pivot long-form
  pivot_longer(
    cols = c(Cluster1, Cluster2, Cluster3, Cluster4),
    names_to = "class",
    values_to = "estimate"
  ) |>
  
  ## group and name the clusters
  mutate(class = factor(
    class,
    levels = c("Cluster4",
               "Cluster2",
               "Cluster3",
               "Cluster1"),
    labels = c("Ardent",
               "Restrictive",
               "Disengaged",
               "Moderate")
  )) |>
  
  ## group the indicators
  mutate(group = ifelse(
    variable %in% c("natident1"),
    "ID",
    ifelse(
      variable %in% c(
        "natpride1",
        "natpride2",
        "natpride3",
        "natpride4",
        "natpride5",
        "natpride6",
        "natpride7",
        "natpride8",
        "natpride9",
        "natpride10"
      ),
      "Pride",
      ifelse(
        variable %in% c(
          "natmembr1",
          "natmembr2",
          "natmembr3",
          "natmembr4",
          "natmembr5",
          "natmembr6",
          "natmembr7",
          "natmembr8"
        ),
        "Membership Criteria",
        "Hubris"
      )
    )
  )) |>
  mutate(group = factor(group,
                        levels = c(
                          "ID",
                          "Pride",
                          "Membership Criteria",
                          "Hubris"
                        ))) |>
  
  ## start drawing the plot
  ggplot(aes(
    x = variable,
    y = estimate,
    fill = factor(
      outcome,
      levels = c("1", "2", "3", "4", "5"),
      labels = c(
        "Strongly Disagree",
        "Disagree",
        "Neither Agree Nor Disagree",
        "Agree",
        "Strongly Agree"
      )
    )
  )) +
  geom_bar(stat = "identity",
           position = "stack",
           color = "black") +
  facet_grid(class ~ group, scale = "free_x", space = "free") +
  theme_ipsum_rc(grid = F) +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9)) +
  theme(legend.position = "right") +
  labs(x = "Variables", y = "Estimated Item Values", fill = "Response") +
  scale_fill_manual(values = c("#DC7684", "#E4CA99", "#EAF2F4", "#A4C9D7","#2D7F9D"))
dev.off()

# classification
d.tvns <- d.tvns |> mutate(class = factor(
  clu_number,
  levels = c(3, 1, 4, 2),
  labels = c("Ardent",
             "Restrictive",
             "Disengaged",
             "Moderate")
))

d.issp <- d.issp |> mutate(class = factor(
  clu_number,
  levels = c(4, 2, 3, 1),
  labels = c("Ardent",
             "Restrictive",
             "Disengaged",
             "Moderate")
))

table(d.tvns$class)/nrow(d.tvns)
table(d.issp$class)/nrow(d.issp)

# ---------------------------------------------------------------------------- #

