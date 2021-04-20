# -*- coding: utf-8-unix -*-

library(here)
library(tidyverse)
options(scipen = 999)

# Set envinronment for config file ----

Sys.setenv(R_CONFIG_ACTIVE = "V2030_ve0_rm")

# Read themes and colors for plotting ----

source(here("scripts", "accessibility", "colors.R"), encoding = "utf-8")
source(here("scripts", "accessibility", "themes.R"), encoding = "utf-8")
source(here("scripts", "accessibility", "translations.R"), encoding = "utf-8")

# Create output folder ----

dir.create(here("results", config::get("projected_scenario")),
           showWarnings = FALSE)
dir.create(here("figures", config::get("projected_scenario")),
           showWarnings = FALSE)

# Combine data from different Helmet-scenarios ----

source(here("scripts", "accessibility", "combine_agent_data.R"), encoding = "utf-8")

# Plot results ----

# The analysis files should be self-contained i.e.
# the order in which the files are ran does not matter

analysis_files <- list.files(path = here("scripts", "accessibility", "plotting"),
                             pattern = ".R")

for (f in analysis_files) {
  message(sprintf("Running analysis in %s...", f))
  source(here("scripts", "accessibility", "plotting", f), encoding = "utf-8")
}
