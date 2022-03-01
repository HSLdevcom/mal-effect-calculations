# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "vdfs_all.rds"))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = vdf, y = share)) +
  geom_col(aes(fill = scenario), position = position_dodge2()) +
  geom_text(
    aes(y = share / 2, label = scales::label_percent(accuracy = 1, suffix = "", decimal.mark = ",")(share)),
    position = position_dodge2(width = 0.9),
    size = points2mm(8),
    color = "#333333"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1, suffix = ""),
    expand = expansion(mult = 0.1)
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(20)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#007AC9", "#54A5DA", "#AAD3ED")
  ) +
  labs(
    title = "Moottoriajoneuvoliikenteen kilometrisuorite väylätyypeittäin",
    x = NULL,
    y = "%"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_vehicle-kms-vdf.png"))
