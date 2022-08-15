library(tidyverse)
library(here)
source(here("scripts", "accessibility", "helpers.R"),
       encoding = "utf-8")
source(here::here("scripts", "basemap", "functions_map.R"),
       encoding = "utf-8")

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

file_path <-
  here("results",
       config::get("projected_scenario"),
       "housing_costs_income.Rdata")

load(file_path)

zones <- readr::read_rds(here::here("results", "zones.rds"))

zones <- zones %>%
  dplyr::rename(zone = SIJ2019)

# Calc cost per month ----

agents_1 <- agents_1 %>%
  mutate(cost = cost * 30)

# Summarise by area ----

zones_agents <- agents_1 %>%
  group_mean("number", c("cost", "income"))

zones <- zones %>%
  left_join(zones_agents,
            by = c("zone" = "number"),
            suffix = c("","1")
  ) %>%
  left_join(housing_cost,
            by = c("zone" = "sij2019"),
            suffix = c("","1")) %>%
  left_join(income_data,
            by = c("zone" = "sij2019"),
            suffix = c("","1"))

# Calculate measures ----
zones <- zones %>%
  mutate(housing_transport_cost = cost + asmenot_kalib_hlo,
         income_cost = housing_transport_cost / hr_mtu)

# Plot costs in zones ----

# transport costs

ggplot() +
  geom_sf(mapping = aes(fill = cost),
          data = zones, color = NA) +
  scale_fill_steps(
    name = "eur / asukas / kk",
    limits = c(0, 400),
    breaks = seq(100, 300, 100),
    high = hsl_cols("blue"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray"),
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Liikkumisen suorat kustannukset (eur)",
    subtitle = config::get("projected_name")
  ) +
  theme_mal_map()

ggsave_map(
  here(
    "figures",
    config::get("projected_scenario"),
    paste0("cost_transport_", config::get("projected_scenario") ,".png")
  )
)

# housing + transport costs
ggplot() +
  geom_sf(mapping = aes(fill = housing_transport_cost),
          data = zones, color = NA) +
  scale_fill_steps(
    name = "eur / asukas / kk",
    limits = c(400, 1000),
    breaks = seq(500, 900, 100),
    high = hsl_cols("red"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray"),
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Liikkumisen ja asumisen suorat menot (eur)",
    subtitle = config::get("projected_name")
  ) +
  theme_mal_map()

ggsave_map(
  here(
    "figures",
    config::get("projected_scenario"),
    paste0("cost_housing_transport_", config::get("projected_scenario") ,".png")
  )
)

# income ----
ggplot() +
  geom_sf(mapping = aes(fill = income_cost),
          data = zones, color = NA) +
  scale_fill_steps(
    name = "%",
    labels = scales::label_percent(accuracy = 1, suffix = ""),
    limits = c(0.2, 0.5),
    breaks = seq(0.25, 0.45, 0.05),
    high = hsl_cols("red"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray"),
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Osuus tuloista liikkumiseen ja asumiseen",
    subtitle = config::get("projected_name")
  ) +
  theme_mal_map()

ggsave_map(
  here(
    "figures",
    config::get("projected_scenario"),
    paste0("cost_income_", config::get("projected_scenario") ,".png")
  )
)
