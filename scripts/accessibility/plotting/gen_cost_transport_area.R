library(tidyverse)
library(here)
source(here("scripts", "accessibility", "helpers.R"),
       encoding = "utf-8")

# Read agent costs; housing data are not needed for this chart ----

load(here("results", config::get("projected_scenario"), "agents.Rdata"))

# Monthly direct and indirect costs by area ----

summarise_transport_costs <- function(agent_data, scenario_name) {
  agent_data %>%
    mutate(indirect_cost = (gen_cost - cost) * 30,
           cost = cost * 30) %>%
    group_mean("area", c("cost", "indirect_cost")) %>%
    mutate(scenario = scenario_name)
}

areas <- bind_rows(
  summarise_transport_costs(agents, config::get("present_name")),
  summarise_transport_costs(agents_0, config::get("baseline_name")),
  summarise_transport_costs(agents_1, config::get("projected_name"))
) %>%
  mutate(scenario = factor(scenario,
                           levels = c(config::get("present_name"),
                                      config::get("baseline_name"),
                                      config::get("projected_name"))))

results <- areas %>%
  pivot_longer(c(cost, indirect_cost), names_to = "type", values_to = "value") %>%
  mutate(type = factor(type,
                       levels = c("indirect_cost", "cost"),
                       labels = c("Liikkumisen epäsuora kustannus",
                                  "Liikkumisen suora kustannus")))

# Match the original area's facets, scenario opacity and value labels ----

ggplot(results, aes(x = scenario, y = value)) +
  facet_grid(cols = vars(area), switch = "both",
             labeller = labeller(.cols = scales::label_wrap(10))) +
  geom_col(fill = "white") +
  geom_col(aes(fill = type, alpha = forcats::fct_rev(scenario))) +
  geom_text(
    aes(label = scales::label_number(accuracy = 1)(value), group = type),
    position = position_stack(vjust = 0.5),
    size = points2mm(8),
    color = "#333333"
  ) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 10),
    expand = expansion(mult = c(0.025, 0.1))
  ) +
  scale_x_discrete(labels = NULL) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Liikkumisen suora kustannus" = hsl_cols("green"),
      # HSL train purple (#8C4799), also used by hsl_purples_fill.
      "Liikkumisen epäsuora kustannus" = hsl_cols("train")
    ),
    guide = guide_legend(order = 1)
  ) +
  scale_alpha_discrete(
    name = NULL,
    range = c(0.333, 1),
    guide = guide_legend(order = 2, reverse = TRUE)
  ) +
  labs(
    title = "Liikkumisen keskimääräiset kustannukset",
    x = NULL,
    y = "eur / asukas / kk"
  ) +
  theme_mal_graph()

ggsave_graph(
  here("figures", config::get("projected_scenario"),
       "graph_gen_cost_transport_areas.png"),
  width = 234,
  height = 107
)
