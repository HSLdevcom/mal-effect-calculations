library(tidyverse)
library(config)
library(here)
source(here("scripts", "accessibility", "translations.R"),
       encoding = "utf-8")
source(here("scripts", "accessibility", "helpers.R"),
       encoding = "utf-8")

# Factorize tables with helpers ----

agent_data_factors <- function(df){
  df %>%
    add_factors("age_group", levels_age_groups) %>%
    add_factors("area", levels_areas) %>%
    translate_vars("age_group", levels_age_groups) %>%
    translate_vars("area", levels_areas) %>%
    translate_vars("gender", levels_genders)
}

tour_data_factors <- function(df){
  df %>%
    add_factors("mode", levels_modes) %>%
    translate_vars("mode", levels_modes)
}

# Load data ----

agents <- read_helmet_files("present_scenario", "agents")
agents_0 <- read_helmet_files("baseline_scenario", "agents")
agents_1 <- read_helmet_files("projected_scenario", "agents")

tours <- read_helmet_files("present_scenario", "tours")
tours_0 <- read_helmet_files("baseline_scenario", "tours")
tours_1 <- read_helmet_files("projected_scenario", "tours")

# Calculate income group ----

agents <- agents %>%
  add_inc_group() %>%
  agent_data_factors()

agents_0 <- agents_0 %>%
  add_inc_group() %>%
  agent_data_factors()

agents_1 <- agents_1 %>%
  add_inc_group() %>%
  agent_data_factors()

tours <- tours %>%
  tour_data_factors()

tours_0 <- tours_0 %>%
  tour_data_factors()

tours_1 <- tours_1 %>%
  tour_data_factors()

# Join tours to agents data ----

res_var <- c("total_access",
             "sustainable_access",
             "gen_cost",
             "cost")

agents <- agents %>%
  join_tours(tours, res_var)

agents_0 <- agents_0 %>%
  join_tours(tours_0, res_var)

agents_1 <- agents_1 %>%
  join_tours(tours_1, res_var)

# Write to file ----

save(agents, agents_0, agents_1,
     tours, tours_0, tours_1,
     file = here(
       "results",
       config::get("projected_scenario"),
       "agents.Rdata"
     ))
