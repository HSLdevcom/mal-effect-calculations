# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

emission_statistics <- here::here("utilities", "co2_statistics.tsv") %>%
  readr::read_tsv(col_types = "id") %>%
  tibble::deframe()

results <- readr::read_rds(here::here("results", "emissions_all.rds")) %>%
  dplyr::add_row(
    scenario = "2005 LIPASTO",
    vehicle = factor("Kaikki ajoneuvotyypit"),
    emission = emission_statistics["2005"]
  ) %>%
  dplyr::add_row(
    scenario = "2005 LIPASTO",
    vehicle = factor("total"),
    emission = emission_statistics["2005"]
  ) %>%
  dplyr::mutate(vehicle = forcats::fct_rev(vehicle))

tavoite <- tibble::tribble(
  ~year, ~emission, ~label,
  2005, emission_statistics["2005"], 0.0,
  2030, 0.5 * emission_statistics["2005"], -0.5,
  2045, 0, -1.0
)

results <- results %>%
  dplyr::mutate(
    year = dplyr::case_when(
      scenario == "2005 LIPASTO" ~ 2005L,
      scenario == "2018 Nykytila" ~ 2018L,
      scenario == "2040 Vertailupohja" ~ 2040L,
      TRUE ~ NA_integer_
    )
  )

results_total <- results %>%
  dplyr::filter(vehicle == "total") %>%
  dplyr::filter(scenario != "2005 LIPASTO") %>%
  dplyr::mutate(label = -(emission_statistics["2005"] - emission) / emission_statistics["2005"])

results <- results %>%
  dplyr::filter(vehicle != "total")


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = year, y = emission)) +
  geom_col(
    aes(fill = vehicle),
    position = position_stack(),
    width = 5
  ) +
  geom_line(
    data = tavoite,
    color = "#dc0451",
    linetype = "dashed"
  ) +
  geom_point(
    data = tavoite,
    color = "#dc0451",
    size = 3
  ) +
  geom_text(
    data = tavoite,
    aes(label = scales::label_percent(suffix = " %", accuracy = 1)(label)),
    color = "#dc0451",
    vjust = -1,
    size = points2mm(8),
    fontface = "bold"
  ) +
  geom_text(
    data = results_total,
    aes(label = scales::label_percent(suffix = " %", accuracy = 1)(label)),
    vjust = -0.5,
    size = points2mm(8),
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 10^(-9))
  ) +
  scale_x_continuous(
    breaks = c(2005, 2018, 2030, 2040, 2045),
    minor_breaks = seq(from = 2000, to = 2050, by = 5),
    limits = c(2000, 2050),
    expand = expansion(0, 0)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#e0e0e0", "#333333", "#AAD3ED", "#007AC9", "#F7C8E6", "#f092cd")
  ) +
  labs(
    title = "Moottoriajoneuvoliikenteen CO2-päästöjen kehitys Helsingin seudulla",
    x =  NULL,
    y = "tuhatta tonnia CO2-ekv. vuodessa"
  ) +
  theme_mal_graph() +
  theme(legend.position = "right",
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line())

ggsave_graph(here::here("figures", "graph_co2_time.png"))
