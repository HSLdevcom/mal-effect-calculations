library(tidyverse)
library(here)
source(here("scripts", "accessibility", "helpers.R"),
       encoding = "utf-8")

load(here("results", config::get("projected_scenario"), "agents.Rdata"))

# Calculate each agent's monthly indirect cost before averaging by area.
area_indirect_cost <- function(agent_data) {
  agent_data %>%
    mutate(indirect_cost = (gen_cost - cost) * 30) %>%
    group_mean("area", "indirect_cost")
}

agent_sums <- full_join(
  area_indirect_cost(agents_0),
  area_indirect_cost(agents_1),
  by = "area",
  suffix = c("_0", "_1")
) %>%
  mutate(cost_dif = indirect_cost_1 - indirect_cost_0)

ggplot(agent_sums, aes(x = area, y = cost_dif)) +
  geom_col(fill = hsl_cols("train")) +
  geom_abline(slope = 0) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", accuracy = 0.01),
    expand = expansion(mult = c(0.025, 0.1))
  ) +
  scale_x_discrete(labels = scales::label_wrap(5)) +
  labs(
    y = "euroa asukasta kohden kuukaudessa",
    x = NULL,
    title = "Muutos asukkaan matkojen epäsuorissa kustannuksissa",
    subtitle = paste0(config::get("baseline_name"), " \U2192 ",
                      config::get("projected_name"))
  ) +
  theme_mal_graph()

ggsave_graph(
  here("figures", config::get("projected_scenario"),
       "graph_diff_indirect_cost_areas.png"),
  width = 150, height = 84
)
