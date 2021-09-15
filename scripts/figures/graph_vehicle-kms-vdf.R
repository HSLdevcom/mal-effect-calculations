# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "vdfs_all.rds"))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = vdf, y = share)) +
  geom_col(aes(fill = scenario), position = position_dodge2()) +
  geom_text(
    aes(label = scales::label_percent(accuracy = 1, suffix = "", decimal.mark = ",")(share)),
    position = position_dodge2(width = 0.9),
    vjust = -0.5,
    size = points2mm(8)
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1, suffix = "")
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
