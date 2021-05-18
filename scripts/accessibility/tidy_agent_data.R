library(tidyverse)
library(config)
library(here)
source(here("scripts", "accessibility", "translations.R"),
       encoding = "utf-8")

# Helper functions ----

# Read files wrapper for agent data
read_helmet_files <- function(name) {
  read_delim(file.path(config::get("helmet_data"),
                       config::get(name),
                       "agents.txt"),
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
add_factors <- function(df) {
  df <- df %>%
    mutate(age_group = forcats::as_factor(age_group),
           age_group = forcats::fct_relevel(age_group, !!!levels_age_groups)
    )

  df <- df %>%
    mutate(area = forcats::as_factor(area),
           area = forcats::fct_relevel(area, !!!levels_areas)
    )
}

# Translate factors
translate_variables <- function(df) {
  df <- df %>%
    mutate(
      area = forcats::fct_recode(area,!!!levels_areas),
      age_group = forcats::fct_recode(age_group,!!!levels_age_groups),
      gender = forcats::fct_recode(gender,!!!levels_genders)
    )
}

# Load data ----

agents <- read_helmet_files("present_scenario")
agents_0 <- read_helmet_files("baseline_scenario")
agents_1 <- read_helmet_files("projected_scenario")

# Calculate income group ----

agents <- agents %>%
  add_inc_group() %>%
  add_factors() %>%
  translate_variables()

agents_0 <- agents_0 %>%
  add_inc_group() %>%
  add_factors() %>%
  translate_variables()

agents_1 <- agents_1 %>%
  add_inc_group() %>%
  add_factors() %>%
  translate_variables()

# Write to file ----

save(agents,
     agents_0,
     agents_1,
     file = here(
       "results",
       config::get("projected_scenario"),
       "agents.Rdata"
     ))
