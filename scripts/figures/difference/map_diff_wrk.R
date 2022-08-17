# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

scenario0 <- "2040_ve0u"
scenario1 <- "2040_ve2"

results0 <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario0))) %>%
  dplyr::select(zone, total_wrk, land_area) %>%
  dplyr::rename(wrk0 = total_wrk)

results1 <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario1))) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(zone, total_wrk) %>%
  dplyr::rename(wrk1 = total_wrk)

results <- results0 %>%
  dplyr::left_join(results1, by = "zone") %>%
  dplyr::mutate(
    # One scenario
    wrk_density0 = wrk0 / land_area,
    wrk_density1 = wrk1 / land_area,
    # Workplace dfferences
    wrk_diff = wrk1 - wrk0,
    wrk_diff_rel = wrk_diff / wrk0,
    wrk_density_diff = wrk_density1 - wrk_density0,
  ) %>%
  dplyr::mutate(
    wrk_diff_rel = dplyr::if_else(is.nan(wrk_diff_rel), 0, wrk_diff_rel)
  )

quantile_ <- function(x, probs = c(0, 0.01, 0.02, 0.05, 0.95, 0.98, 0.99, 1), ...) {
  quantile(x = x, probs = probs, ...)
}

quantile_(results$wrk_density_diff)


# Plot density ---------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = wrk_density_diff),
          data = results, color = NA) +
  scale_fill_distiller(
    palette = "Spectral",
    name = "työpaikkoja per km2",
    labels = scales::label_number(accuracy = 1),
    direction = -1,
    # limits = c(-3000, 3000),
    limits = c(-250, 250),
    oob = scales::squish
  ) +
  scale_color_manual(
    values = NA
  ) +
  geom_basemap() +
  geom_sf(data = results, fill = NA, color = "#333333", size = 0.1) +
  coord_sf_mal() +
  annotate_map(
    title = "Työpaikkatiheyden absoluuttinen muutos",
    subtitle = sprintf("%d %s \U2192 %d %s",
                       scenarios$year[scenarios$scenario == scenario0],
                       scenarios$name[scenarios$scenario == scenario0],
                       scenarios$year[scenarios$scenario == scenario1],
                       scenarios$name[scenarios$scenario == scenario1])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_diff_wrk-density_%s_%s.png", scenario0, scenario1)))


# Plot share -----------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = wrk_diff_rel),
          data = results, color = NA) +
  scale_fill_distiller(
    name = "%",
    type = "div",
    limits = c(-0.2, 0.2),
    labels = scales::label_percent(accuracy = 1, suffix = ""),
    oob = scales::oob_squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Suhteellinen muutos työpaikoissa",
    subtitle = sprintf("%d %s \U2192 %d %s",
                       scenarios$year[scenarios$scenario == scenario0],
                       scenarios$name[scenarios$scenario == scenario0],
                       scenarios$year[scenarios$scenario == scenario1],
                       scenarios$name[scenarios$scenario == scenario1])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_diff_rel_wrk_%s_%s.png", scenario0, scenario1)))
