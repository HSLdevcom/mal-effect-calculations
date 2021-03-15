# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
options(scipen=999)

# Read themes and colors for plotting ----
source(here("R", "colors.R"), encoding = "utf-8")
source(here("R", "themes.R"), encoding = "utf-8")

# Set envinronment for config file ----
Sys.setenv(R_CONFIG_ACTIVE = "V2030_ve0_rm")

# Create output folder ----
dir.create(here("results", get("projected_scenario")), 
           showWarnings = FALSE)

# Combine data for projected and baseline scenario ----
source(here("R", "combine_agent_data.R"), encoding = "utf-8")

# Plot results ----
# The analysis files should be self-contained i.e. 
# the order in which the files are ran does not matter
 
analysis_files <- list.files(path = here("R"), 
                             pattern = "^plot_")
for (f in analysis_files) {
  message(sprintf("Running analysis in %s...", f))
  source(here("R", f), encoding = "utf-8")
}
