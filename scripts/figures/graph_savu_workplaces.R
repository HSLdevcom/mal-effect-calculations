# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", "areas_all.rds"))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = scenario, y = goodness_share)) +
  facet_grid(cols = vars(area), switch = "both", labeller = labeller(.cols = scales::label_wrap(10))) +
  geom_col(aes(fill = area)) +
  scale_y_continuous(
    labels = scales::label_percent(suffix = " %")
  ) +
  scale_x_discrete(
    expand = expansion(mult = 0.4),
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    values = c("#424242", "#558ed5", "#95b3d7", "#b3a2c7", "#fac090", "#fcd5b5")
  ) +
  labs(
    title = "Työpaikkojen sijoittuminen kestävän liikkumisen kannalta hyville saavutettavuusvyöhykkeille",
    x = NULL,
    y = NULL
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_savu_workplaces.png"))
