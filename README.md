# Replication: Sentiments of Solidarity

This repository provides the replication files for the paper *Sentiments of Solidarity: Varying Conceptions of Nationhood in Turkey*, forthcoming in *Nations and Nationalism*.

The pre-print version of the paper is stored at [SocArXiv]().

## Notes on Replication

1) *Data Sources*. This study uses original data collected by the authors.

   See `./data/data.rds` for the full data.

2) *Latent Gold Analyses*. We used [Latent Gold 6.0](https://www.statisticalinnovations.com/latent-gold-6-0/) in estimating latent class analyses, as well as regression models with distal outcomes.

## The Steps for Replication

1) Import the `./data/data.sav` file into the Latent GOLD software,
2) Run the `./scripts/LG/code-LCA-fits` code and export the relevant outputs. See `./data/LG_output` for the exports. We modified the csv files for easy coding in R.
3) Export the posterior probabilities, as specified in the LG code in (2).
4) Run the `./scripts/LG/code_regressions` to estimate the regressions with distal outcomes.
5) Once the output files are saved into the relevant folders, run the `.scripts/R/01_main.R` to produce all figures that we present in the main article.
6) All supplemental materials can be produced following (1)-(5) for relevant files.

Note that you are advised to use `renv.lock` and the related [{{renv}}](https://rstudio.github.io/renv/articles/renv.html) conventions to make sure that you use the R packages that we used in the analyses.
