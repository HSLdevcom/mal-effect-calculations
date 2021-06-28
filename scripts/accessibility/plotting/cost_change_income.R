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

res_var <- c("cost")
group_var <- c("area", "income_group")

agent_sums_0 <- agents_0 %>%
  filter(income_group %in% 1:10) %>%
  group_mean(group_var, res_var)

agent_sums_1 <- agents_1 %>%
  filter(income_group %in% 1:10) %>%
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
income_names <- c("alin 20 %", rep("", 3), "ylin 20 %")

agent_sums %>%
  ggplot(aes(x = income_group, y = cost_dif)) +
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
  scale_x_discrete(labels = income_names) +
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
       "cost_change_income_group.png"
  ),
  width = dimensions_wide[1],
  height = dimensions_wide[2],
  units = "cm"
)
