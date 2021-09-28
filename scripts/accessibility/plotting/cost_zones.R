library(tidyverse)
library(config)
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

agents <- agents %>%
  mutate(cost = cost * 30)

# Summarise by area ----

zones_agents <- agents %>%
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
    n.breaks = 6,
    high = hsl_cols("blue"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray")
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Liikkumisen suorat kustannukset (eur)",
    subtitle = config::get("present_name")
  ) +
  theme_mal_map()

ggsave(
  here(
    "figures",
    config::get("projected_scenario"),
    "cost_transport.png"
  ),
  width = dimensions_long[1],
  height = dimensions_long[2],
  units = "cm"
)

# housing costs
ggplot() +
  geom_sf(mapping = aes(fill = asmenot_kalib_hlo),
          data = zones, color = NA) +
  scale_fill_steps(
    name = "eur / asukas / kk",
    n.breaks = 6,
    high = hsl_cols("red"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray")
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Asumisen menot (eur)",
    subtitle = config::get("present_name")
  ) +
  theme_mal_map()

ggsave(
  here(
    "figures",
    config::get("projected_scenario"),
    "cost_housing.png"
  ),
  width = dimensions_long[1],
  height = dimensions_long[2],
  units = "cm"
)

# housing + transport costs
ggplot() +
  geom_sf(mapping = aes(fill = housing_transport_cost),
          data = zones, color = NA) +
  scale_fill_steps(
    name = "eur / asukas / kk",
    n.breaks = 6,
    high = hsl_cols("red"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray")
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Liikkumisen ja asumisen suorat menot (eur)",
    subtitle = config::get("present_name")
  ) +
  theme_mal_map()

ggsave(
  here(
    "figures",
    config::get("projected_scenario"),
    "cost_housing_transport.png"
  ),
  width = dimensions_long[1],
  height = dimensions_long[2],
  units = "cm"
)

# income ----
ggplot() +
  geom_sf(mapping = aes(fill = hr_mtu),
          data = zones, color = NA) +
  scale_fill_steps(
    name = "eur / asukas / kk",
    n.breaks = 6,
    high = hsl_cols("green"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray")
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Mediaanitulot (eur)",
    subtitle = config::get("present_name")
  ) +
  theme_mal_map()

ggsave(
  here(
    "figures",
    config::get("projected_scenario"),
    "median_income.png"
  ),
  width = dimensions_long[1],
  height = dimensions_long[2],
  units = "cm"
)

# income ----
ggplot() +
  geom_sf(mapping = aes(fill = income_cost),
          data = zones, color = NA) +
  scale_fill_steps(
    name = "%",
    labels = scales::percent,
    breaks = seq(0.20, 0.50, 0.05),
    high = hsl_cols("red"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray")
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Osuus tuloista liikkumiseen ja asumiseen",
    subtitle = config::get("present_name")
  ) +
  theme_mal_map()

ggsave(
  here(
    "figures",
    config::get("projected_scenario"),
    "cost_income.png"
  ),
  width = dimensions_map[1],
  height = dimensions_map[2],
  units = "cm"
)
