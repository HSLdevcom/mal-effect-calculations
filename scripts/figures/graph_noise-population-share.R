# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds")) %>%
  dplyr::mutate(noise_population_share = noise_population / total_pop)


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = area, y = noise_population_share)) +
  geom_col(aes(fill = scenario), position = position_dodge2()) +
  geom_text(
    aes(label = scales::label_percent(accuracy = 1, suffix = "")(noise_population_share)),
    position = position_dodge2(width = 0.9),
    vjust = -0.5,
    size = points2mm(8)
  ) +
  scale_y_continuous(
    labels = scales::label_percent(suffix = ""),
    limits = c(0, 1)
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#3E8606", "#7DAD58", "#BFD7AC")
  ) +
  labs(
    title = "Meluvyöhykkeillä asuvien osuus alueen asukkaista",
    x =  NULL,
    y = "%"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_noise-population-share.png"))
