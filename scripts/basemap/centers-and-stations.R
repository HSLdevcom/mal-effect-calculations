# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

region <- readr::read_rds(here::here("results", "region.rds"))

centers <- here::here("data", "UML keskusalueet", "K_yhdistetty.shp") %>%
  sf::read_sf() %>%
  sf::st_transform(3879) %>%
  dplyr::summarise() %>%
  sf::st_cast("POLYGON") %>%
  dplyr::mutate(scenario = "2018", center = TRUE) %>%
  dplyr::select(scenario, center)

stations <- here::here("data", "RautatiePiste_Maastokartta100k_MML_HS15.gpkg") %>%
  sf::read_sf() %>%
  dplyr::rename(geometry = geom) %>%
  sf::st_buffer(1000) %>%
  dplyr::summarise() %>%
  sf::st_cast("POLYGON") %>%
  dplyr::mutate(scenario = "2018", station = TRUE) %>%
  dplyr::select(scenario, station)

stations_2040_ve0 <- here::here("data", "Lansimetron_jatkeen_asemat.shp") %>%
  sf::read_sf() %>%
  sf::st_buffer(1000) %>%
  dplyr::summarise() %>%
  sf::st_cast("POLYGON") %>%
  dplyr::mutate(scenario = "2040_ve0", station = TRUE) %>%
  dplyr::select(scenario, station)

all <- dplyr::bind_rows(centers, stations, stations_2040_ve0) %>%
  sf::st_intersection() %>%
  sf::st_difference() %>%
  dplyr::mutate(across(c(center, station), ~ tidyr::replace_na(., FALSE))) %>%
  dplyr::filter(sf::st_geometry_type(.) != "POINT") %>%
  dplyr::filter(sf::st_intersects(., sf::st_as_sf(region), sparse = FALSE))


readr::write_rds(all, file = here::here("results", "centers-and-stations.rds"))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = center),
          data = all, color = NA) +
  geom_basemap() +
  scale_fill_manual("Alue",
                    values = c("#BFD7AC", "#3E8606"),
                    labels = c("Raskas raideliikenne", "Seudun keskus")) +
  coord_sf_mal() +
  annotate_map(
    title = "Seudun keskukset ja raskas raideliikenne",
    subtitle = NULL
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_centers-and-stations.png"))
