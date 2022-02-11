# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "squares.rds")) %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(diff_pop = total_pop - total_pop_2019) %>%
  dplyr::filter(diff_pop >= 0) %>%
  dplyr::mutate(keskus = !is.na(luokka)) %>%
  dplyr::group_by(area, keskus) %>%
  dplyr::summarise(diff_pop = sum(diff_pop), .groups = "drop_last") %>%
  dplyr::mutate(share = diff_pop / sum(diff_pop)) %>%
  dplyr::ungroup()

results_all <- results %>%
  dplyr::group_by(keskus) %>%
  dplyr::summarise(diff_pop = sum(diff_pop), .groups = "drop") %>%
  dplyr::mutate(area = "helsinki_region",
                share = diff_pop / sum(diff_pop))

results <- results %>%
  dplyr::bind_rows(results_all) %>%
  dplyr::filter(keskus) %>%
  dplyr::mutate(scenario = "2040 Vertailupohja")


# Translate data ----------------------------------------------------------

translations <- here::here("utilities", "areas.tsv") %>%
  readr::read_tsv(col_types = "cc")

results <- results %>%
  dplyr::mutate(area = factor(area, levels = translations$level, labels = translations$label)) %>%
  dplyr::arrange(area)


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = area, y = share)) +
  geom_col(aes(fill = scenario), position = position_dodge2()) +
  geom_text(
    aes(label = scales::label_percent(accuracy = 1, suffix = "")(share)),
    position = position_dodge2(width = 0.9),
    vjust = -0.5,
    size = points2mm(8),
    color = "#333333"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1, suffix = ""),
    limits = c(0, 1),
    expand = expansion(mult = 0.1)
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("2018 Nykytila" = "#3E8606", "2040 Vertailupohja" = "#7DAD58", "2040 Varjo" = "#BFD7AC")
  ) +
  labs(
    title = "Seudun keskuksiin ja raskaan raideliikenteen piiriin\nsijoittuvien uusien asukkaiden osuus",
    x =  NULL,
    y = "%"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_uml.png"))
