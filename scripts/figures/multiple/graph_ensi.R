# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

translations <- here::here("utilities", "areas.tsv") %>%
  readr::read_tsv(col_types = "cc")

results <- readr::read_rds(here::here("results", "squares_areas_ensi_apartments.rds")) %>%
  dplyr::mutate(area = factor(area, levels = translations$level, labels = translations$label)) %>%
  dplyr::arrange(area) %>%
  dplyr::mutate(scenario = "2040 2. luonnos")

# Plot --------------------------------------------------------------------

ggplot(results, aes(x = area, y = apartment_share_ensi)) +
  geom_col(position = position_dodge2(), fill = "#3E8606") +
  geom_text(
    aes(label = scales::label_percent(accuracy = 1, suffix = "")(apartment_share_ensi)),
    position = position_dodge2(width = 0.9),
    vjust = -0.5,
    size = points2mm(8),
    color = "#333333"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1, suffix = ""),
    limits = c(0, 1),
    expand = expansion(mult = c(0.025, 0.1))
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  labs(
    title = "Ennustettu asuntotuotannon kohdistuminen ensisijaisille vyöhykkeille",
    x =  NULL,
    y = "%"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_ensi.png"))
