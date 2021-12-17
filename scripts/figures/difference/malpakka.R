# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")

zones_all <- dplyr::bind_rows(
  "2018 Nykytila" = readr::read_rds(here::here("results", "zones_2018.rds")),
  "2040 Vertailupohja" = readr::read_rds(here::here("results", "zones_2040_ve0.rds")),
  .id = "scenario") %>%
  dplyr::mutate(scenario = forcats::as_factor(scenario)) %>%
  sf::st_drop_geometry()

translations <- here::here("utilities", "areas.tsv") %>%
  readr::read_tsv(col_types = "cc")

zones_all_helsinki_region <- zones_all %>%
  dplyr::mutate(area = "helsinki_region")

zones_all <- zones_all %>%
  dplyr::bind_rows(zones_all_helsinki_region) %>%
  dplyr::mutate(area = factor(area, levels = translations$level, labels = translations$label))

means <- zones_all %>%
  dplyr::group_by(scenario, area) %>%
  dplyr::summarise(malpakka = mean(malpakka),
                  .groups = "drop")

ggplot() +
  facet_grid(~ area) +
  geom_boxplot(data = zones_all, aes(x = scenario, y = malpakka, fill = scenario), outlier.shape = 1) +
  geom_point(data = means, aes(x = scenario, y = malpakka), shape = 4, size = 3) +
  scale_fill_manual(
    name = NULL,
    values = c("#3E8606", "#7DAD58", "#BFD7AC")
  ) +
  scale_y_continuous(
    breaks = seq(0, 10, 2)
  ) +
  labs(
    title = "MALPAKKA-mittarin hajonta ja keskiarvo alueittain",
    x =  NULL,
    y = "MALPAKKA"
  ) +
  theme_mal_graph() +
  theme(
    panel.grid.minor.x = element_blank(),
    axis.text.x=element_blank(),

  )

ggsave_graph(here::here("figures", "malpakka_distibution.png"), width = 297, height = 210)
