# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

translations <- here::here("utilities", "areas.tsv") %>%
  readr::read_tsv(col_types = "cc")

results <- here::here("data",
                      "helmet_4.0.4_2018_results",
                      "workforce_accessibility_per_area.txt") %>%
  readr::read_tsv(
    col_names = c("area", "value"),
    col_types = "cd",
    skip = 1
  )

results <- results %>%
  dplyr::filter(area != "peripheral") %>%
  dplyr::add_row(area = "helsinki_region",
                 value = 0,
                 .before = 1) %>%
  dplyr::mutate(area = factor(area, levels = translations$level, labels = translations$label))

results1 <- results
results1$scenario <- "2023"
results2 <- results
results2$scenario <- "2040 Pohja"
results3 <- results
results3$scenario <- "2040 Luonnos"

results <- dplyr::bind_rows(results1, results2, results3) %>%
  dplyr::mutate(scenario = forcats::as_factor(scenario)) %>%
  dplyr::mutate(value = value / 100)


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = scenario, y = value)) +
  facet_grid(cols = vars(area), switch = "both", labeller = labeller(.cols = scales::label_wrap(10))) +
  geom_col(aes(fill = area)) +
  scale_y_continuous(
    labels = scales::label_number(scale = 0.001)
  ) +
  scale_x_discrete(
    expand = expansion(mult = 0.4),
    labels = scales::label_wrap(5)
  ) +
  scale_fill_manual(
    values = c("#424242", "#558ed5", "#95b3d7", "#b3a2c7", "#fac090", "#fcd5b5")
  ) +
  labs(
    title = "Työvoimasaavutettavuus",
    x = NULL,
    y = "tuhatta henkilöä"
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

ggsave_graph(here::here("figures", "graph_workforce-accessibility.png"))
