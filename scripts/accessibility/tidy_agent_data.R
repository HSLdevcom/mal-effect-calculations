library(tidyverse)
library(config)
library(here)
source(here("scripts", "accessibility", "translations.R"),
       encoding = "utf-8")

# Helper functions ----

# Read files wrapper for agent data
read_helmet_files <- function(scenario, table) {
  read_delim(
    file.path(config::get("helmet_data"),
              config::get(scenario),
              paste0(table, ".txt")),
    delim = "\t",
    col_names = TRUE)
}

# Add income deciles for agents
# Separate under 18 years old from calculation
add_inc_group <- function(df) {
  df0 <- df %>%
    filter(!age_group %in% "age_7-17") %>%
    arrange(income) %>%
    mutate(income_group = ceiling(10 * row_number() / n()))

  df1 <- df %>%
    filter(age_group %in% "age_7-17") %>%
    mutate(income_group = -1)

  df <- bind_rows(df0, df1) %>%
    mutate(income_group = as.factor(income_group))
}

# Factorize grouping variable for plotting
add_factors <- function(df, colname, factors) {
  df %>%
    dplyr::mutate(
      !!colname := forcats::as_factor(!!sym(colname)),
      !!colname := forcats::fct_relevel(!!sym(colname), !!!factors)
      )
}

# Translate factors
translate_vars <- function(df, colname, factors) {
  df %>%
    dplyr::mutate(
      !!colname := forcats::fct_recode(!!sym(colname), !!!factors)
    )
}

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

# Write to file ----

save(agents, agents_0, agents_1,
     tours, tours_0, tours_1,
     file = here(
       "results",
       config::get("projected_scenario"),
       "agents.Rdata"
     ))
