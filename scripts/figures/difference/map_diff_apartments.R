# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

scenario0 <- "2018"
scenario1 <- "2040_ve0"

results0 <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario0))) %>%
  dplyr::select(zone, detach) %>%
  dplyr::mutate(apartments0 = 1 - detach)

results1 <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario1))) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(zone, detach) %>%
  dplyr::mutate(apartments1 = 1 - detach)

results <- results0 %>%
  dplyr::left_join(results1, by = "zone") %>%
  dplyr::mutate(
    # Detached differences
    apartments_diff = apartments1 - apartments0,
    apartments_diff_rel = apartments_diff / apartments0,
  )

quantile_ <- function(x, probs = c(0, 0.01, 0.02, 0.05, 0.95, 0.98, 0.99, 1), ...) {
  quantile(x = x, probs = probs, ...)
}

quantile_(results$apartments0)
quantile_(results$apartments1)
quantile_(results$apartments_diff)


# Plot apartments_diff ---------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = apartments_diff),
          data = results, color = NA) +
  scale_fill_distiller(
    palette = "Spectral",
    name = "%-yks.",
    labels = scales::label_percent(accuracy = 1, suffix = ""),
    direction = -1,
    limits = c(-0.75, 0.75),
    oob = scales::squish
  ) +
  scale_color_manual(
    values = NA
  ) +
  geom_basemap() +
  geom_sf(data = results, fill = NA, color = "#333333", linewidth = 0.1) +
  coord_sf_mal() +
  annotate_map(
    title = "Kerrostaloasuntojen osuuden absoluuttinen muutos",
    subtitle = sprintf("%d %s \U2192 %d %s",
                       scenarios$year[scenarios$scenario == scenario0],
                       scenarios$name[scenarios$scenario == scenario0],
                       scenarios$year[scenarios$scenario == scenario1],
                       scenarios$name[scenarios$scenario == scenario1])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_diff_apartments_%s-%s.png", scenario0, scenario1)))
