# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)


# Data --------------------------------------------------------------------

translations <- here::here("utilities", "areas.tsv") %>%
  readr::read_tsv(col_types = "cc")

results <- readr::read_rds(here::here("results", "zones_2018.rds")) %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(area, savu_goodness, .drop = FALSE) %>%
  dplyr::summarise(
    goodness_wrk = sum(total_wrk)
  ) %>%
  dplyr::mutate(
    total_wrk = sum(goodness_wrk),
    share = goodness_wrk / total_wrk
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(savu_goodness == "SAVU hyvä") %>%
  # Add total row for whole region.
  dplyr::add_row(area = "helsinki_region",
                 goodness_wrk = sum(.$goodness_wrk),
                 total_wrk = sum(.$total_wrk),
                 share = goodness_wrk / total_wrk,
                 .before = 1) %>%
  dplyr::mutate(area = factor(area, levels = translations$level, labels = translations$label))

results1 <- results
results1$scenario <- "2023"
results2 <- results
results2$scenario <- "2040 Pohja"
results3 <- results
results3$scenario <- "2040 Luonnos"

results <- dplyr::bind_rows(results1, results2, results3) %>%
  dplyr::mutate(scenario = forcats::as_factor(scenario))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = scenario, y = share)) +
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

ggsave_graph(here::here("figures", "graph_savu_workplaces.png"))
