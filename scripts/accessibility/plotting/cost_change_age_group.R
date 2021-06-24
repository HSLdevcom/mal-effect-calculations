library(tidyverse)
library(config)
library(here)
source(here("scripts", "accessibility", "helpers.R"),
       encoding = "utf-8")

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

# Group agents tables ----

res_var <- c("cost", "nr_tours")
group_var <- c("area", "age_group")

agent_sums <- agents %>%
  group_mean(group_var, res_var)

agent_sums_0 <- agents_0 %>%
  group_mean(group_var, res_var)

agent_sums_1 <- agents_1 %>%
  group_mean(group_var, res_var)

# Join tables ----

agent_sums <- full_join(agent_sums,
                        agent_sums_0,
                        by = group_var,
                        suffix = c("", "0"))

agent_sums <- full_join(agent_sums,
                        agent_sums_1,
                        by = group_var,
                        suffix = c("", "1"))

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
  ggplot(aes(x = age_group, y = cost_dif)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "white",
    fill = hsl_cols("blue"),
    width = 0.8
  ) +
  ylim(-max_value, max_value) +
  facet_wrap( ~ area, nrow = 1) +
  theme_wide +
  geom_abline(slope = 0) +
  labs(
    y = "kustannus (eur / asukas / kk)",
    x = NULL,
    title = paste0(
      "Muutos asukkaan matkojen kustannuksissa: ",
      config::get("projected_name"),
      " - ",
      config::get("baseline_name")),
    subtitle = "Kaikki matkaryhmät"
  )

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "cost_change_age_group.png"
  ),
  width = dimensions_wide[1],
  height = dimensions_wide[2],
  units = "cm"
)
