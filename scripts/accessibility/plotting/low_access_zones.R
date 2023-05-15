library(tidyverse)
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
    "Muu pääkaupunkiseutu")

kehys <-
  c("Junaliikenteen kehyskunnat",
    "Bussiliikenteen kehyskunnat"
    )

# Join tours to agents data ----

agents <- agents %>%
  join_purpose_tours(tours, "sustainable_access", "ho")

agents_1 <- agents_1 %>%
  join_purpose_tours(tours_1, "sustainable_access", "ho")

# Calculate tour access ----

agents <- agents %>%
  # After left_join, if an agent did not make ho tours, nr_tours_ho is NA.
  filter(!is.na(nr_tours_ho)) %>%
  filter(nr_tours_ho > 0) %>%
  mutate(tour_access_ho = sustainable_access_ho / nr_tours_ho)

agents_1 <- agents_1 %>%
  # After left_join, if an agent did not make ho tours, nr_tours_ho is NA.
  filter(!is.na(nr_tours_ho)) %>%
  filter(nr_tours_ho > 0) %>%
  mutate(tour_access_ho = sustainable_access_ho / nr_tours_ho)

# Group agents tables ----

limit_pks <- agents %>%
  filter(area %in% pks) %>%
  summarise(limit = quantile(tour_access_ho, probs = 0.05, na.rm = TRUE)) %>%
  pull(limit)

limit_kehys <- agents %>%
  filter(area %in% kehys) %>%
  summarise(limit = quantile(tour_access_ho, probs = 0.05, na.rm = TRUE)) %>%
  pull(limit)

calc_low_access <- function(df, pks, limit_pks, limit_kehys) {
  df %>%
    mutate(
      low_access = if_else(
        area %in% pks,
        tour_access_ho < limit_pks,
        tour_access_ho < limit_kehys
      )) %>%
    group_by(number, area) %>%
    summarise(nr_agents = n(),
              low_access = sum(low_access, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(share = low_access / nr_agents)
}

low_access <- agents %>%
  filter(!is_car_user) %>%
  calc_low_access(pks, limit_pks, limit_kehys) %>%
  mutate(scenario = config::get("present_name"))

low_access_1 <- agents_1 %>%
  filter(!is_car_user) %>%
  calc_low_access(pks, limit_pks, limit_kehys) %>%
  mutate(scenario = config::get("projected_name"))

# Combine to shapefile ----

zones <- zones %>%
  left_join(low_access_1, by = c("zone" = "number"))

# Plot accessibility ----

zones_centroids <- zones %>%
  st_centroid() %>%
  filter(low_access > 5)

zones_centroids_pks <- zones_centroids %>%
  dplyr::filter(area %in% pks)
zones_centroids_kehys <- zones_centroids %>%
  dplyr::filter(area %in% kehys)

ggplot() +
  geom_sf(data = st_union(dplyr::filter(zones, area %in% kehys)), fill = "#dddddc", linewidth = NA) +
  geom_basemap() +
  geom_sf(
    data = zones_centroids_pks,
    mapping = aes(size = low_access),
    colour = hsl_cols("red"),
    alpha = 0.5,
    shape = 16,
    stroke = 0
  ) +
  scale_size(name = "asukasta", range = c(0, 4)) +
  coord_sf_mal() +
  annotate_map(
    title = "Saavutettavuusköyhät autottomat asukkaat pääkaupunkiseudulla",
    subtitle = config::get("projected_name")
  ) +
  theme_mal_map()

ggsave_map(
  here("figures",
       config::get("projected_scenario"),
       "zones_access_poor_no_car_pks.png"
  )
)

ggplot() +
  geom_sf(data = st_union(dplyr::filter(zones, area %in% pks)), fill = "#dddddc", linewidth = NA) +
  geom_basemap() +
  geom_sf(
    data = zones_centroids_kehys,
    mapping = aes(size = low_access),
    colour = hsl_cols("red"),
    alpha = 0.5,
    shape = 16,
    stroke = 0
  ) +
  scale_size(name = "asukasta", range = c(0, 4)) +
  coord_sf_mal() +
  annotate_map(
    title = "Saavutettavuusköyhät autottomat asukkaat kehyskunnissa",
    subtitle = config::get("projected_name")
  ) +
  theme_mal_map()

ggsave_map(
  here("figures",
       config::get("projected_scenario"),
       "zones_access_poor_no_car_kehys.png"
  )
)
