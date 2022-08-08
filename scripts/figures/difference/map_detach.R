# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

scenario0 <- "2018"
scenario1 <- "2040_ve0u"

results0 <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario0))) %>%
  dplyr::select(zone, detach) %>%
  dplyr::rename(detach0 = detach)

results1 <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", scenario1))) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(zone, detach) %>%
  dplyr::rename(detach1 = detach)

results <- results0 %>%
  dplyr::left_join(results1, by = "zone") %>%
  dplyr::mutate(
    # Detached differences
    detach_diff = detach1 - detach0,
    detach_diff_rel = detach_diff / detach0,
  )

quantile_ <- function(x, probs = c(0, 0.01, 0.02, 0.05, 0.95, 0.98, 0.99, 1), ...) {
  quantile(x = x, probs = probs, ...)
}

quantile_(results$detach0)
quantile_(results$detach1)
quantile_(results$detach_diff)


# Plot detach_diff ---------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = detach_diff),
          data = results, color = NA) +
  # geom_sf(mapping = aes(color = ''),
  #         data = dplyr::filter(results, detach_diff < 0.0001), fill = "#ffffff") +
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
  # guides(fill = guide_colorbar(order = 1),
  #        color = guide_legend("Ei muutosta\ntai muutos on\nnegatiivinen",
  #                             order = 100,
  #                             override.aes = list(color = "#333333"),
  #                             keywidth = unit(0.6, "cm"),
  #                             keyheight = unit(0.6, "cm"))) +
  geom_basemap() +
  geom_sf(data = results, fill = NA, color = "#333333", size = 0.1) +
  coord_sf_mal() +
  annotate_map(
    title = "Pientalojen osuuden absoluuttinen muutos",
    subtitle = sprintf("%d %s \U2192 %d %s",
                       scenarios$year[scenarios$scenario == scenario0],
                       scenarios$name[scenarios$scenario == scenario0],
                       scenarios$year[scenarios$scenario == scenario1],
                       scenarios$name[scenarios$scenario == scenario1])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_diff_detach_%s_%s.png", scenario0, scenario1)))

# Plot detach_2018 ------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = detach0),
          data = results, color = NA) +
  scale_fill_distiller(
    palette = "OrRd",
    name = "%",
    labels = scales::label_percent(accuracy = 1, suffix = " %"),
    limits = c(0, 1),
    oob = scales::squish
  ) +
  scale_color_manual(
    values = NA
  ) +
  geom_basemap() +
  geom_sf(data = results, fill = NA, color = "#333333", size = 0.1) +
  coord_sf_mal() +
  annotate_map(
    title = "Pientalojen osuus",
    subtitle = sprintf("%d %s", scenarios$year[scenarios$scenario == scenario0],
                       scenarios$name[scenarios$scenario == scenario0])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_detach_%s.png", scenario0)))


# Plot detach_2040_ve0 ------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = detach1),
          data = results, color = NA) +
  scale_fill_distiller(
    palette = "OrRd",
    name = "%",
    labels = scales::label_percent(accuracy = 1, suffix = " %"),
    limits = c(0, 1),
    oob = scales::squish
  ) +
  scale_color_manual(
    values = NA
  ) +
  geom_basemap() +
  geom_sf(data = results, fill = NA, color = "#333333", size = 0.1) +
  coord_sf_mal() +
  annotate_map(
    title = "Pientalojen osuus",
    subtitle = sprintf("%d %s", scenarios$year[scenarios$scenario == scenario1],
                       scenarios$name[scenarios$scenario == scenario1])
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", sprintf("map_detach_%s.png", scenario1)))
