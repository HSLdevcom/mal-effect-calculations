library(tidyverse)
library(config)
library(here)
library(sf)
source(here("scripts", "accessibility", "helpers.R"),
       encoding = "utf-8")
source(here("scripts", "accessibility", "translations.R"),
       encoding = "utf-8")
source(here::here("scripts", "basemap", "functions_map.R"),
       encoding = "utf-8")

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

zones <- readr::read_rds(here::here("results", "zones.rds"))

zones <- zones %>%
  dplyr::rename(zone = SIJ2019)

# Parameters for plotting ----

pks <-
  c("Helsingin kantakaupunki",
    "Muu Helsinki",
    "Espoo, Vantaa, Kau")

kehys <-
  c("Kehyskunnat (raide)",
    "Kehyskunnat (muut)"
  )

# Join tours to agents data ----

agents <- agents %>%
  join_purpose_tours(tours, "sustainable_access", "ho")

agents_1 <- agents_1 %>%
  join_purpose_tours(tours_1, "sustainable_access", "ho")

# Filter persons with no car use ----

agents <- agents %>%
  filter(is_car_user)

agents_1 <- agents_1 %>%
  filter(is_car_user)

# Calculate tour access ----

agents <- agents %>%
  filter(nr_tours > 0) %>%
  mutate(tour_access = sustainable_access / nr_tours)

agents_1 <- agents_1 %>%
  filter(nr_tours > 0) %>%
  mutate(tour_access = sustainable_access / nr_tours)

# Group agents tables ----

limit_pks <- agents %>%
  filter(area %in% pks) %>%
  summarise(limit = quantile(tour_access, probs = 0.05, na.rm = TRUE)) %>%
  pull(limit)

limit_kehys <- agents %>%
  filter(area %in% kehys) %>%
  summarise(limit = quantile(tour_access, probs = 0.05, na.rm = TRUE)) %>%
  pull(limit)

calc_low_access <- function(df, pks, limit_pks, limit_kehys) {
  df %>%
    mutate(
      low_access = if_else(
      area %in% pks,
      tour_access < limit_pks,
      tour_access < limit_kehys
    )) %>%
    group_by(number) %>%
    summarise(nr_agents = n(),
              low_access = sum(low_access, na.rm = TRUE)) %>%
    mutate(share = low_access / nr_agents) %>%
    ungroup()
}

low_access <- agents %>%
  calc_low_access(pks, limit_pks, limit_kehys) %>%
  mutate(scenario = config::get("present_name"))

low_access_1 <- agents_1 %>%
  calc_low_access(pks, limit_pks, limit_kehys) %>%
  mutate(scenario = config::get("projected_name"))

# Combine to shapefile ----

zones <- zones %>%
  left_join(low_access_1, by = c("zone" = "number"))

# Plot accessibility ----

zones_centroids <- zones %>%
  st_centroid() %>%
  filter(low_access > 30) %>%
  rename(Asukkaat = "low_access")

ggplot() +
  geom_sf(
    data = zones,
    fill = NA,
    color = "lightgray",
    size = 0.01
    ) +
  geom_basemap() +
  geom_sf(
    data = zones_centroids,
    mapping = aes(size = Asukkaat),
    colour = hsl_cols("red"),
    alpha = 0.5,
    shape = 16,
    stroke = 0
  ) +
  scale_size(range = c(0, 4)) +
  annotate_map(
    title = "Vertailuarvon alle jäävä väestö",
    subtitle = config::get("projected_name")
  ) +
  theme_mal_map() +
  coord_sf(
    xlim = c(bbox$xmin - 500, bbox$xmax + 500),
    ylim = c(bbox$ymin + 20000, bbox$ymax + 500),
    expand = FALSE)

ggsave(
  here("figures",
       get("projected_scenario"),
       "zones_access_poor_no_car.png"
  ),
  width = dimensions_map[1],
  height = dimensions_map[2],
  units = "cm"
)
