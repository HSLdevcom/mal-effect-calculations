# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
library(readxl)

source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")

# Data --------------------------------------------------------------------

health_codes <- sprintf("06%02d", 10:19)
building_file <- here::here(
  "data", "sotepe_alueet",
  "Sotepe rakennukset HELMET-alue.xlsx"
)

health_centers <- readxl::read_excel(
  building_file,
  sheet = "Toimipaikat"
) %>%
  dplyr::mutate(
    id = sprintf("HC-%04d", dplyr::row_number())
  ) %>%
  dplyr::transmute(
    id,
    municipality = as.character(Kunta),
    status = as.character(Tila),
    name = as.character(Kohde),
    address = as.character(Osoite),
    purpose = as.character(`Käyttötarkoitus`),
    latitude = as.numeric(Leveysaste),
    longitude = as.numeric(Pituusaste)
  ) %>%
  dplyr::filter(
    stringr::str_sub(purpose, 1, 4) %in% health_codes,
    status %in% c("Nykyinen", "Suunniteltu")
  ) %>%
  dplyr::mutate(
    purpose_code = stringr::str_sub(purpose, 1, 4),
    purpose_name = stringr::str_trim(stringr::str_remove(purpose, "^[0-9]+\\s+"))
  )

missing_coordinates <- health_centers %>%
  dplyr::filter(is.na(latitude) | is.na(longitude))

health_centers <- health_centers %>%
  dplyr::filter(!is.na(latitude), !is.na(longitude)) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  sf::st_transform(3879)

zones <- readr::read_rds(here::here("results", "zones.rds")) %>%
  dplyr::select(zone = SIJ2023, geometry)

# Assign each facility to the nearest TAZ. Nearest-feature matching also
# handles facilities located just outside a TAZ polygon boundary.
health_centers <- health_centers %>%
  sf::st_join(zones, join = sf::st_nearest_feature, left = TRUE)

stopifnot(inherits(health_centers, "sf"))
stopifnot(all(!sf::st_is_empty(health_centers)))
stopifnot(all(!is.na(health_centers$zone)))

readr::write_rds(
  health_centers,
  file = here::here("results", "health-centers.rds")
)
readr::write_rds(
  missing_coordinates,
  file = here::here("results", "health-centers-missing-coordinates.rds")
)

# Service-area summaries --------------------------------------------------

service_areas <- readxl::read_excel(
  here::here("data", "sotepe_alueet", "Terveysasemien aluejako.xlsx"),
  sheet = "Terveysasemien aluejako"
) %>%
  dplyr::select(Terveysasema, `Terveysasema-alue`, Peruspiiri, `Osa-alue`) %>%
  dplyr::distinct()

service_area_summary <- service_areas %>%
  dplyr::count(Terveysasema, `Terveysasema-alue`, name = "subareas")

readr::write_rds(
  service_areas,
  file = here::here("results", "health-centre-service-areas.rds")
)

# Map ---------------------------------------------------------------------

health_centers_map <- ggplot() +
  geom_sf(data = zones, fill = "#F3F1E8", color = "#D0CCC0", linewidth = 0.12) +
  geom_basemap(show_roads = TRUE) +
  geom_sf(
    data = health_centers,
    aes(shape = status, color = purpose_name),
    size = 2.2,
    stroke = 0.35
  ) +
  scale_shape_manual(values = c("Nykyinen" = 16, "Suunniteltu" = 17)) +
  scale_color_brewer(palette = "Dark2", name = "Palvelutyyppi") +
  coord_sf_mal() +
  annotate_map(
    title = "Helsingin seudun terveys- ja hyvinvointikeskukset",
    subtitle = "Nykyiset ja suunnitellut kohteet sijoittelualueilla"
  ) +
  theme_mal_map()

ggsave_map(
  here::here("figures", "map_health-centers.png"),
  plot = health_centers_map
)

# Graphs ------------------------------------------------------------------

graph_service_areas <- service_area_summary %>%
  ggplot(aes(x = forcats::fct_reorder(Terveysasema, subareas), y = subareas)) +
  geom_col(fill = "#3E8606") +
  coord_flip() +
  labs(
    title = "Terveysasemien palvelualueet",
    x = "Terveysasema",
    y = "Osa-alueiden lukumäärä"
  ) +
  theme_mal_graph()

ggsave_graph(
  here::here("figures", "graph_health-centre-service-areas.png"),
  plot = graph_service_areas
)

message(
  "Health-centre workflow complete: ", nrow(health_centers),
  " mapped facilities; ", nrow(missing_coordinates),
  " health-related records without coordinates."
)