# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)


# Data --------------------------------------------------------------------

emission_statistics <- here::here("utilities", "co2_statistics.tsv") %>%
  readr::read_tsv(col_types = "id") %>%
  tibble::deframe()

results <- readr::read_rds(here::here("results", "emissions_all.rds")) %>%
  dplyr::add_row(
    scenario = factor("2005"),
    vehicle = factor("Kaikki ajoneuvotyypit"),
    emission = emission_statistics["2005"]
  ) %>%
  dplyr::add_row(
    scenario = factor("2005"),
    vehicle = factor("total"),
    emission = emission_statistics["2005"]
  ) %>%
  dplyr::mutate(vehicle = forcats::fct_rev(vehicle),
                scenario = forcats::fct_relevel(scenario, "2005"))

results_total <- results %>%
  dplyr::filter(vehicle == "total")

results <- results %>%
  dplyr::filter(vehicle != "total")


# Plot --------------------------------------------------------------------

ggplot(results, aes(x = scenario, y = emission)) +
  geom_col(aes(fill = vehicle), position = position_stack()) +
  geom_text(
    aes(label = scales::label_number(scale = 10^(-9), accuracy = 1)(emission), group = vehicle, color = vehicle),
    position = position_stack(vjust = 0.5),
    size = points2mm(8),
    show.legend = FALSE
  ) +
  geom_text(data = results_total,
            aes(label = scales::label_number(scale = 10^(-9), accuracy = 1)(emission)),
            vjust = -0.5,
            size = points2mm(8),
            fontface = "bold",
            color = "#333333") +
  scale_y_continuous(
    labels = scales::label_number(scale = 10^(-9)),
    expand = expansion(mult = c(0.025, 0.1))
  ) +
  scale_x_discrete(
    labels = function(x) sub(" ", "\n", x)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#e0e0e0", "#333333", "#AAD3ED", "#007AC9", "#F7C8E6", "#f092cd")
  ) +
  scale_color_manual(
    name = NULL,
    values = c("#333333", "#ffffff", "#33333300", "#33333300", "#33333300", "#333333")
  ) +
  labs(
    title = "Moottoriajoneuvoliikenteen CO2-päästöt kulkuneuvoittain Helsingin seudulla",
    x =  NULL,
    y = "tuhatta tonnia CO2-ekv. vuodessa"
  ) +
  theme_mal_graph() +
  theme(legend.position = "right")

ggsave_graph(here::here("figures", "graph_co2_uusi.png"))
