# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds"))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = area, y = workplace_accessibility_scaled)) +
  geom_col(aes(fill = scenario), position = position_dodge2()) +
  geom_errorbar(
    mapping = aes(ymin = workplace_accessibility_lower, ymax = workplace_accessibility_upper),
    position =  position_dodge2(width = 0.9, padding = 0.66),
    color = "#333333",
    size = 0.35
  ) +
  geom_text(
    aes(y = workplace_accessibility_scaled / 2, label = scales::label_number(accuracy = 1)(workplace_accessibility_scaled)),
    position = position_dodge2(width = 0.9),
    size = points2mm(8),
    color = "#333333"
  ) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 1),
    limits = c(0, 100),
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
    title = "Työpaikkojen kasautuminen",
    x = NULL,
    y = "indeksi"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_workplace-accessibility.png"))
