# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

emission_statistics <- here::here("utilities", "co2_statistics.tsv") %>%
  readr::read_tsv(col_types = "id") %>%
  tibble::deframe()

results <- readr::read_rds(here::here("results", "emissions_all.rds")) %>%
  dplyr::filter(vehicle == "total") %>%
  dplyr::add_row(
    scenario = factor("2005"),
    vehicle = factor("total"),
    emission = emission_statistics["2005"]
  ) %>%
  dplyr::mutate(scenario = forcats::fct_relevel(scenario, "2005"),
                decrease = -(emission_statistics["2005"] - emission) / emission_statistics["2005"],
                label = sprintf("%s\n(%s)",
                                scales::label_number(scale = 10^(-9), accuracy = 1)(emission),
                                scales::label_percent(accuracy = 1, suffix = " %")(decrease)),
                label = dplyr::if_else(scenario == 2005, scales::label_number(scale = 10^(-9), accuracy = 1)(emission), label))


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = scenario, y = emission, fill = scenario)) +
  geom_col() +
  geom_errorbar(
    mapping = aes(ymin = emission_lower, ymax = emission_upper),
    color = "#333333",
    width = 0.35,
    size = 0.35
  ) +
  geom_text(
    aes(y = emission / 2, label = label, color = scenario),
    position = position_dodge2(width = 0.9),
    size = points2mm(8)
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 10^(-9)),
    expand = expansion(mult = 0.05)
  ) +
  scale_x_discrete(
    labels = sub(" ", "\n", levels(results$scenario))
  ) +
  scale_fill_manual(
    guide = "none",
    values = c("#e0e0e0", mal_fill)
  ) +
  scale_color_manual(
    guide = "none",
    values = c("#333333", mal_color)
  ) +
  labs(
    title = "Moottoriajoneuvoliikenteen CO2-päästöt Helsingin seudulla",
    x =  NULL,
    y = "tuhatta tonnia CO2-ekv. vuodessa"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures", "graph_co2-total.png"))
