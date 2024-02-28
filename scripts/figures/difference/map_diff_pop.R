# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

scenario0 <- "2040_ve0"
scenario1 <- "2040_suunnitelma"

results0 <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario0))) %>%
  dplyr::select(zone, total_pop, land_area) %>%
  dplyr::rename(pop0 = total_pop)

results1 <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario1))) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(zone, total_pop) %>%
  dplyr::rename(pop1 = total_pop)

results <- results0 %>%
  dplyr::left_join(results1, by = "zone") %>%
  dplyr::mutate(
    # One scenario
    pop_density0 = pop0 / land_area,
    pop_density1 = pop1 / land_area,
    # Population differences
    pop_diff = pop1 - pop0,
    pop_diff_rel = pop_diff / pop0,
    pop_density_diff = pop_density1 - pop_density0,
  ) %>%
  dplyr::mutate(
    pop_diff_rel = dplyr::if_else(is.nan(pop_diff_rel), 0, pop_diff_rel)
)

quantile_ <- function(x, probs = c(0, 0.01, 0.02, 0.05, 0.95, 0.98, 0.99, 1), ...) {
  quantile(x = x, probs = probs, ...)
}

quantile_(results$pop_density_diff)


# Plot pop_density_diff ---------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = pop_density_diff),
          data = results, color = NA) +
  scale_fill_distiller(
    palette = "Spectral",
    name = "asukasta per km2",
    labels = scales::label_number(accuracy = 1),
    direction = -1,
    # limits = c(-5000, 5000),
    limits = c(-1000, 1000),
    oob = scales::squish
  ) +
  scale_color_manual(
    values = NA
  ) +
  geom_basemap() +
  geom_sf(data = results, fill = NA, color = "#333333", linewidth = 0.05) +
  coord_sf_mal() +
  annotate_map(
    title = "Väestötiheyden absoluuttinen muutos",
    subtitle = sprintf("%d %s \U2192 %d %s",
                       scenarios$year[scenarios$scenario == scenario0],
                       scenarios$name[scenarios$scenario == scenario0],
                       scenarios$year[scenarios$scenario == scenario1],
                       scenarios$name[scenarios$scenario == scenario1])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_diff_pop-density_%s_%s.png", scenario0, scenario1)))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = pop_diff_rel),
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
    title = "Suhteellinen muutos väkiluvussa",
    subtitle = sprintf("%d %s \U2192 %d %s",
                       scenarios$year[scenarios$scenario == scenario0],
                       scenarios$name[scenarios$scenario == scenario0],
                       scenarios$year[scenarios$scenario == scenario1],
                       scenarios$name[scenarios$scenario == scenario1])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_diff_rel_pop_%s_%s.png", scenario0, scenario1)))
