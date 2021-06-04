library(tidyverse)
library(config)
library(here)

# Load data ----


file_path <- here("data", config::get("housing_cost"))
housing_cost <- file_path %>%
  readxl::read_xlsx(sheet = "Taul1")

# Column names to lowercase ----

names(housing_cost) <- tolower(names(housing_cost))

# Select only columns needed in analysis ----

housing_cost <- housing_cost %>%
  select(sij2019, kuntanimi, askust_kalib_hlo, asmenot_kalib_hlo)

# Remove aakkoset ----
# Helmet model uses names without

housing_cost <- housing_cost %>%
  mutate(kuntanimi = str_replace_all(kuntanimi, "ä", "a"),
         kuntanimi = str_replace_all(kuntanimi, "ö", "o"))

# Write to file ----

save(housing_cost,
     file = here(
       "results",
       config::get("projected_scenario"),
       "housing_costs.Rdata"))
