# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "squares.rds"))
ensi <- readr::read_rds(here::here("results", "ensi.rds"))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(mapping = aes(fill = kem2_pop),
          data = results, color = NA) +
  geom_sf(mapping = aes(),
          data = ensi, color = NA, fill = "#3E8606", alpha = 0.5) +
  geom_basemap() +
  scale_fill_steps(
    name = "kerros-m2",
    labels = scales::label_number(accuracy = 1),
    breaks = c(250, 500, 1000),
    limits = c(0, 1500),
    low = "#ffffff",
    high = "#000000",
    oob = scales::squish
  ) +
  coord_sf_mal() +
  annotate_map(
    title = "Asuntotuotannon kohdistuminen ensisijaisesti kehitettäville vyöhykkeille",
    subtitle = "2040 Vertailupohja"
  ) +
  theme_mal_map()

ggsave_map(here::here("figures", "map_ensi-kem2_2040_ve0.png"))
