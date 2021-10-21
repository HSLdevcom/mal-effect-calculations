# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds"))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = area, y = sustainable_accessibility)) +
  geom_col(aes(fill = scenario), position = position_dodge2()) +
  geom_text(
    aes(label = scales::label_number(accuracy = 1, big.mark = "")(sustainable_accessibility)),
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
    title = "Saavutettavuus asukkaiden näkökulmasta",
    x = NULL,
    y = NULL
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_sustainable-accessibility.png"))
