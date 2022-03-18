# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results0 <- readr::read_rds(here::here("results", "zones_2040_ve0.rds")) %>%
  dplyr::select(zone, malpakka) %>%
  dplyr::rename(malpakka0 = malpakka)


results1 <- readr::read_rds(here::here("results", "zones_2040_ve1.rds")) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(zone, malpakka) %>%
  dplyr::rename(malpakka1 = malpakka)

results <- results0 %>%
  dplyr::left_join(results1, by = "zone") %>%
  dplyr::mutate(diff_malpakka = malpakka1 - malpakka0)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = diff_malpakka),
          data = results, color = NA) +
  scale_fill_fermenter(
    palette = "PRGn",
    name = expression(e[t]),
    direction = 1,
    limits = c(-0.45, 0.45),
    labels = scales::label_number(accuracy = 0.1, decimal.mark = ","),
    breaks = seq(-0.35, 0.35, 0.1),
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Muutos kestävien kulkutapojen mahdollistamassa tonttitehokkuudessa",
    subtitle = "2040 Vertailupohja \U2192 2040 1. luonnos"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_diff_malpakka_2040_ve0-2040_ve1.png"))
