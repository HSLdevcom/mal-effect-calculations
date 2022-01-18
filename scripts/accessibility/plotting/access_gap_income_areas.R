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

agents <- agents %>%
  join_purpose_tours(tours, "total_access", "hw")

agents_0 <- agents_0 %>%
  join_purpose_tours(tours_0, "total_access", "hw")

agents_1 <- agents_1 %>%
  join_purpose_tours(tours_1, "total_access", "hw")

# Group agents tables ----

res_var <- c("total_access_hw", "nr_tours_hw")
group_var <- c("income_group", "area")

agent_sums <- agents %>%
  filter(income_group %in% 1:10) %>%
  group_sum(group_var, res_var)

agent_sums_0 <- agents_0 %>%
  filter(income_group %in% 1:10) %>%
  group_sum(group_var, res_var)

agent_sums_1 <- agents_1 %>%
  filter(income_group %in% 1:10) %>%
  group_sum(group_var, res_var)

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
    present = total_access_hw / nr_tours_hw,
    projected = total_access_hw1 / nr_tours_hw1,
    baseline = total_access_hw0 / nr_tours_hw0
  )

# Plot ----

income_names <- c("alin 20 %", rep("", 3), "ylin 20 %")

gap <- agent_sums %>%
  select(income_group, area, projected, baseline, present) %>%
  gather("scenario", "utility", projected, baseline, present) %>%
  group_by(scenario, area) %>%
  mutate(
    utility_dif = utility - mean(utility, na.rm = TRUE),
    scenario = case_when(
      scenario %in% "projected" ~ config::get("projected_name"),
      scenario %in% "baseline" ~ config::get("baseline_name"),
      scenario %in% "present" ~ config::get("present_name")
    ),
    scenario = factor(scenario, levels = c(config::get("present_name"),
                                           config::get("baseline_name"),
                                           config::get("projected_name")))
  )

max_gap <- max(abs(gap$utility_dif)) + 1

gap %>%
  ggplot(aes(x = income_group, y = utility_dif, fill = scenario)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "white",
    width = 0.8
  ) +
  facet_wrap( ~ area, nrow = 1) +
  scale_fill_manual(values = hsl_pal("blues")(3)) +
  scale_x_discrete(labels = income_names) +
  theme_wide +
  ylim(-max_gap, max_gap) +
  geom_abline(slope = 0) +
  labs(fill = "Skenaario",
       y = "eur / kiertomatka",
       x = NULL,
       title = "Saavutettavuusero suhteessa alueen keskiarvoon",
       subtitle = "Kotiperäiset työmatkat")

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "access_gap_income_areas.png"
       ),
  width = dimensions_wide[1],
  height = dimensions_wide[2],
  units = "cm"
)
