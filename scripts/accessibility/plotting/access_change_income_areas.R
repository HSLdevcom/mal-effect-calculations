library(tidyverse)
library(config)
library(here)
source(here("scripts", "accessibility", "helpers.R"),
       encoding = "utf-8")

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

# Join tours to agents data ----

agents_0 <- agents_0 %>%
  join_purpose_tours(tours_0, "total_access", "hw")

agents_1 <- agents_1 %>%
  join_purpose_tours(tours_1, "total_access", "hw")

# Group agents tables ----

res_var <- c("total_access_hw", "nr_tours_hw")
group_var <- c("income_group", "area")

agent_sums_0 <- agents_0 %>%
  filter(income_group %in% 1:10) %>%
  group_sum(group_var, res_var)

agent_sums_1 <- agents_1 %>%
  filter(income_group %in% 1:10) %>%
  group_sum(group_var, res_var)

# Join tables ----

agent_sums <- full_join(agent_sums_0,
                        agent_sums_1,
                      by = group_var,
                      suffix = c("0", "1"))

# Calc differences ----

agent_sums <- agent_sums %>%
  mutate(
    projected = total_access_hw1 / nr_tours_hw1,
    baseline = total_access_hw0 / nr_tours_hw0,
    util_dif = projected - baseline
  )

# Plot ----

income_names <- c("alin 20 %", rep("", 3), "ylin 20 %")
max_gap <- max(abs(agent_sums$util_dif)) + 1

agent_sums %>%
  ggplot(aes(x = income_group, y = util_dif)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    fill = hsl_cols("blue"),
    color = "white",
    width = 0.8
  ) +
  facet_wrap( ~ area, nrow = 1) +
  theme_wide +
  ylim(-max_gap, max_gap) +
  geom_abline(slope = 0) +
  scale_x_discrete(labels = income_names) +
  labs(
    y = "hyöty (eur / kiertomatka)",
    x = "Skenaarion tulodesiilit",
    title = paste0(
      "Muutos asukkaan tekemien matkojen saavutettavuudessa: ",
      config::get("projected_name")
      ),
    subtitle = "Kotiperäiset työmatkat"
    )

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "access_change_income_areas.png"
       ),
  width = dimensions_wide[1],
  height = dimensions_wide[2],
  units = "cm"
)
