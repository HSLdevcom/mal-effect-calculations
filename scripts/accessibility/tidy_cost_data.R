library(tidyverse)
library(here)

# Load data ----

file_path <- here("data", config::get("housing_cost"))
housing_cost <- file_path %>%
  readxl::read_xlsx(sheet = "Taul1")

file_path <- here("data", config::get("income_data"))
income_data <- file_path %>%
  readxl::read_xlsx(sheet = "tulotaso_korjattu")

# Column names to lowercase ----

names(housing_cost) <- tolower(names(housing_cost))

names(income_data) <- tolower(names(income_data))

# Select only columns needed in analysis ----

housing_cost <- housing_cost %>%
  select(sij2019, kotitaloudet, kuntanimi, askust_kalib_hlo, asmenot_kalib_hlo)

income_data <- income_data %>%
  select(sij2019, kuntanimi, hr_ktu, hr_mtu)

# Remove aakkoset ----
# Helmet model uses names without

housing_cost <- housing_cost %>%
  mutate(kuntanimi = str_replace_all(kuntanimi, "ä", "a"),
         kuntanimi = str_replace_all(kuntanimi, "ö", "o"))

income_data <- income_data %>%
  mutate(kuntanimi = str_replace_all(kuntanimi, "ä", "a"),
         kuntanimi = str_replace_all(kuntanimi, "ö", "o"))

# Incomes to eur per month ----

income_data <- income_data %>%
  mutate(hr_ktu = hr_ktu / 12,
         hr_mtu = hr_mtu / 12)

# Filter too low number of residents ----

housing_cost <- housing_cost %>%
  filter(kotitaloudet > 10)

income_data <- income_data %>%
  filter(sij2019 %in% housing_cost$sij2019)

# Write to file ----

save(housing_cost,
     income_data,
     file = here(
       "results",
       config::get("projected_scenario"),
       "housing_costs_income.Rdata"))
