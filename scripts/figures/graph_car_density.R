# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds"))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = scenario, y = car_density)) +
  facet_grid(cols = vars(area), switch = "both", labeller = labeller(.cols = scales::label_wrap(10))) +
  geom_col(aes(fill = area)) +
  scale_y_continuous(
    labels = scales::label_number()
  ) +
  scale_x_discrete(
    expand = expansion(mult = 0.4),
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    values = c("#424242", "#558ed5", "#95b3d7", "#b3a2c7", "#fac090", "#fcd5b5")
  ) +
  labs(
    title = "Henkilöautotiheys (autoa / 1000 asukasta)",
    x = NULL,
    y = NULL
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

ggsave_graph(here::here("figures", "graph_car_density.png"))
