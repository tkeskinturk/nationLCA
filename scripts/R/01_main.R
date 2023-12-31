
# - sentiments of solidarity ------------------------------------------------- #
# - script 01: main ---------------------------------------------------------- #

### note: this script 
###       (a) plots latent class assignment probabilities,
###       (b) provides several descriptive statistics about the latent classes,
###       (c) plots the figures from allocation analyses.

# ---------------------------------------------------------------------------- #

rm(list = ls())
pacman::p_load(janitor, 
               modelsummary,
               tidyverse,
               hrbrthemes)
theme_set(theme_ipsum_rc())

d <- readRDS("./data/data.rds") # get data

# PART 1: LATENT GOLD FILES -------------------------------------------------- #

# import LG output

## cluster-specific response probabilities
d_prob <- read.csv("./data/LG_output/lca_probabilities.csv")
## fit statistics across multiple class assignments
d_fits <- read.csv("./data/LG_output/lca_fit-stats.csv")
## estimates from regressions with distal outcomes
d_ests <- read.csv("./data/LG_output/lca_estimates.csv")

## add posterior classifications
d <-
  inner_join(
    d,
    # import posterior probabilities
    haven::read_sav("./data/LG_output/data_posteriors.sav") |>
      janitor::clean_names() |> select(id, class = clu_number),
    by = "id")

# PART 2: MODEL FITS --------------------------------------------------------- #

png(
  "./figures/lca_bayesian-information.png",
  w = 8, 
  h = 6,
  units = "in",
  res = 500
)
d_fits |>
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

# PART 3: RESOURCE ALLOCATIONS ----------------------------------------------- #

png(
  "./figures/allocations_desc.png",
  w = 8,
  h = 6,
  units = "in",
  res = 500
)
d |>
  dplyr::select(alloc_family, alloc_friend, alloc_ethnic, alloc_anyone) |>
  pivot_longer(cols = everything(),
               names_to = "tasks",
               values_to = "decisions") |>
  mutate(
    tasks = case_when(
      tasks == "alloc_family" ~ "Family",
      tasks == "alloc_friend" ~ "Friend",
      tasks == "alloc_ethnic" ~ "Ethnic Fellow",
      tasks == "alloc_anyone" ~ "Someone from Turkey"
    ),
    tasks = factor(
      tasks,
      levels = c("Ethnic Fellow", "Someone from Turkey", "Family", "Friend")
    )
  ) |>
  ggplot(aes(x = decisions)) +
  geom_density(col = "gray20",
               fill = "#A4C9D7",
               alpha = 0.5) +
  theme_ipsum_rc() + theme(aspect.ratio = 0.75) +
  labs(x = "Allocation Decisions", y = "Density") +
  facet_wrap( ~ tasks, nrow = 2) +
  scale_y_continuous(
    limits = c(0, 0.06),
    breaks = c(0, 0.05),
    labels = c(0, 0.05)
  )
dev.off()

# PART 4: LATENT CLASS ASSIGNMENTS ------------------------------------------- #

png(
  "./figures/lca_assignments.png",
  w = 12.5,
  h = 7.5,
  units = "in",
  res = 500
)
d_prob |>
  
  ## pivot long-form
  pivot_longer(
    cols = c(Cluster1, Cluster2, Cluster3, Cluster4),
    names_to = "class",
    values_to = "estimate"
  ) |>
  
  ## group and name the clusters
  mutate(class = factor(
    class,
    levels = c("Cluster3",
               "Cluster1",
               "Cluster4",
               "Cluster2"),
    labels = c("Ardent",
               "Restrictive",
               "Disengaged",
               "Moderate")
  )) |>
  
  ## group the indicators
  mutate(group = ifelse(
    variable %in% c("natident1",
                    "natident2"),
    "Identification",
    ifelse(
      variable %in% c("natpride1",
                      "natpride2"),
      "Pride",
      ifelse(
        variable %in% c("natcultr1",
                        "natcultr2"),
        "Culture",
        ifelse(
          variable %in% c("natmembr1",
                          "natmembr2",
                          "natmembr3"),
          "Membership Criteria",
          "Hubris"
        )
      )
    )
  )) |>
  mutate(group = factor(
    group,
    levels = c(
      "Identification",
      "Pride",
      "Membership Criteria",
      "Hubris",
      "Culture"
    )
  )) |>
  
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

# PART 5: POSTERIOR CLASSIFICATIONS ------------------------------------------ #

d <- d |>
  mutate(class = factor(
    class,
    levels = c(3,
               1,
               4,
               2),
    labels = c("Ardent",
               "Restrictive",
               "Disengaged",
               "Moderate")
  ))

d |> count(class) |> mutate(n = n / nrow(d))

# PART 6: DISTAL OUTCOMES ---------------------------------------------------- #

png(
  "./figures/allocations_regs.png",
  w = 8,
  h = 6,
  units = "in",
  res = 500
)
d_ests |>
  mutate(
    term = factor(term,
                  levels = c("Restrictive", "Disengaged", "Moderate")),
    depvar = case_when(
      depvar == 1 ~ "Ethnic Fellow",
      depvar == 2 ~ "Someone from Turkey",
      depvar == 3 ~ "Family",
      depvar == 4 ~ "Friend"
    ),
    depvar = factor(
      depvar,
      levels = c("Ethnic Fellow", "Someone from Turkey", "Family", "Friend")
    )
  ) |>
  ggplot(aes(x = term, y = estimate)) +
  
  geom_linerange(
    aes(ymin = conf.low,
        ymax = conf.high),
    col = "#A4C9D7",
    alpha = 0.5,
    linewidth = 1.5
  ) +
  geom_point(col = "black", size = 1) +
  facet_wrap( ~ depvar, nrow = 2) +
  theme_ipsum_rc(grid = 'Y') +
  labs(x = "Classes", y = "Δ in Average Allocation to Strangers") +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-7.5, 17.5),
                     breaks = c(-5, 0, 5, 10, 15))
dev.off()

# PART 7: NETWORK TIES ------------------------------------------------------- #

# binarize ties
d <- d |> 
  mutate_at(.vars = c('ties_immg',
                      'ties_alev',
                      'ties_lgbt',
                      'ties_noaf'),
            .funs = ~ifelse(. == 1, 0, 1))

d_ties <- 
  bind_rows(
    d |> 
      summarize(mean = mean(ties_immg),
                .by = "class") |> 
      mutate(tie = "Refugees"),
    d |> 
      filter(relig != "alevi") |> 
      summarize(mean = mean(ties_alev),
                .by = "class") |> 
      mutate(tie = "Alevis"),
    d |> 
      summarize(mean = mean(ties_lgbt),
                .by = "class") |> 
      mutate(tie = "LGBTQ"),
    d |> 
      filter(relig != "other") |> 
      summarize(mean = mean(ties_noaf),
                .by = "class") |> 
      mutate(tie = "Religious Nones")
  )

png(
  "./figures/lca_networks-by-classes.png",
  w = 8,
  h = 6,
  units = "in",
  res = 500
)
ggplot(d_ties,
       aes(x = fct_rev(class), y = mean)) +
  geom_segment(
    aes(xend = fct_rev(class),
        y = 0,
        yend = mean)) +
  geom_point(size = 4.5, col = "#A4C9D7") +
  facet_wrap(~ tie, nrow = 2, scales = "fixed") +
  coord_flip() +
  labs(x = "",
       y = "% with Ties to National Others") +
  theme_ipsum_rc(grid = "Xx")
dev.off()

# ---------------------------------------------------------------------------- #
