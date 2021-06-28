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
  join_purpose_tours(tours_0, "total_access", "ho")

agents_1 <- agents_1 %>%
  join_purpose_tours(tours_1, "total_access", "ho")

# Group agents tables ----

res_var <- c("total_access_ho", "nr_tours_ho")
group_var <- c("area", "age_group")

agent_sums_0 <- agents_0 %>%
  group_sum(group_var, res_var)

agent_sums_1 <- agents_1 %>%
  group_sum(group_var, res_var)

# Join tables ----

agent_sums <- full_join(agent_sums_0,
                        agent_sums_1,
                        by = group_var,
                        suffix = c("0", "1"))

# Calc differences ----

agent_sums <- agent_sums %>%
  mutate(
    projected = total_access_ho1 / nr_tours_ho1,
    baseline = total_access_ho0 / nr_tours_ho0,
    util_dif = projected - baseline
  )

# Plot ----

max_gap <- max(abs(agent_sums$util_dif)) + 1

agent_sums %>%
  ggplot(aes(x = age_group, y = util_dif, fill = gender)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "white",
    fill = hsl_cols("blue"),
    width = 0.8
  ) +
  facet_wrap( ~ area, nrow = 1) +
  theme_wide +
  ylim(-max_gap, max_gap) +
  geom_abline(slope = 0) +
  labs(
    y = "hyöty (eur / kiertomatka)",
    x = "Ikäryhmät",
    title = paste0(
      "Muutos asukkaan tekemien matkojen saavutettavuudessa: ",
      config::get("projected_name")
    ),
    subtitle = "Kotiperäiset muut matkat"
  )

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "access_change_age_group_areas.png"
       ),
  width = dimensions_wide[1],
  height = dimensions_wide[2],
  units = "cm"
)
