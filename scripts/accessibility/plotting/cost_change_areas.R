library(tidyverse)
library(here)
source(here("scripts", "accessibility", "helpers.R"),
       encoding = "utf-8")

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

# Group agents tables ----

res_var <- c("cost")
group_var <- c("area")

agent_sums_0 <- agents_0 %>%
  group_mean(group_var, res_var)

agent_sums_1 <- agents_1 %>%
  group_mean(group_var, res_var)

# Join tables ----

agent_sums <- full_join(agent_sums_0,
                        agent_sums_1,
                        by = group_var,
                        suffix = c("0", "1"))

# Calc differences ----

agent_sums <- agent_sums %>%
  mutate(
    projected = 30 * cost1,
    baseline = 30 * cost0,
    cost_dif = projected - baseline
  )

# Plot ----

max_value <- max(agent_sums$cost_dif, na.rm = TRUE)

agent_sums %>%
  ggplot(aes(x = area, y = cost_dif)) +
  geom_col(
    fill = "#3E8606"
  ) +
  geom_abline(slope = 0) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", accuracy = 0.01),
    expand = expansion(mult = c(0.025, 0.1))
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  labs(
    y = "euroa asukasta kohden kuukaudessa",
    x = NULL,
    title = "Muutos asukkaan matkojen suorissa kustannuksissa",
    subtitle = paste0(config::get("baseline_name"), " \U2192 ",  config::get("projected_name"))
  ) +
  theme_mal_graph()

ggsave_graph(
  here("figures",
       config::get("projected_scenario"),
       "graph_diff_cost_areas.png"
  ),
  width = 150, height = 84
)
