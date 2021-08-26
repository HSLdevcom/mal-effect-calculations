# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "vdfs_all.rds"))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = scenario, y = share)) +
  facet_grid(
    cols = vars(vdf),
    switch = "both",
    labeller = labeller(.cols = scales::label_wrap(10))
  ) +
  geom_col(aes(fill = vdf)) +
  geom_text(
    aes(label = scales::label_percent(accuracy = 0.1, suffix = "", decimal.mark = ",")(share)),
    position = position_dodge(width = 0.9),
    vjust = -0.5
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1, suffix = "")
  ) +
  scale_x_discrete(
    expand = expansion(mult = 0.4),
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    values = c("#e6b9b8", "#c0504d", "#632523")
  ) +
  labs(
    title = "Moottoriajoneuvoliikenteen kilometrisuorite väylätyypeittäin",
    x = NULL,
    y = "%"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", colour = "#333333", size = 10),
    plot.title = element_text(colour = "#64BE1E"),
    legend.position = "none",
    legend.text = element_text(size = rel(1.0)),
    plot.caption = element_text(size = rel(1.0)),
    strip.placement = "outside",
    panel.grid.major.x = element_blank(),
    #strip.switch.pad.grid = unit(0, "cm"),
    panel.spacing = unit(0, "lines")
  )

ggsave_graph(here::here("figures", "graph_vehicle-kms-vdf.png"))
