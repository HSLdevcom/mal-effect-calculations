# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

cba <- read_tsv_helmet("T:/JohannaP/cba_2040_ve1_2040_ve0.txt", first_col_name = "zone")

results <- readr::read_rds(here::here("results", "zones_2040_ve0.rds")) %>%
  dplyr::select(zone, area, total_pop) %>%
  dplyr::left_join(cba, by = "zone") %>%
  dplyr::group_by(area) %>%
  dplyr::summarise(
    car_time = sum((car_work_time + car_leisure_time) * total_pop),
    transit_time = sum((transit_work_time + transit_leisure_time) * total_pop)
  ) %>%
  dplyr::mutate(scenario = "2040_ve1")


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = area, y = car_time)) +
  geom_col(aes(fill = scenario), position = position_dodge2()) +
  geom_text(
    aes(label = scales::label_number(accuracy = 1)(car_time)),
    position = position_dodge2(width = 0.9),
    vjust = -0.5,
    size = points2mm(8),
    color = "#333333"
  ) +
  scale_y_continuous(
    labels = scales::label_number(),
    expand = expansion(mult = 0.1)
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#3E8606", "#7DAD58", "#BFD7AC")
  ) +
  labs(
    title = "Matka-ajan muutos henkilöautolla",
    x =  NULL,
    y = "minuuttia"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_cba_car-time.png"))
