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
  join_purpose_tours(tours, "total_access", "ho")

agents_0 <- agents_0 %>%
  join_purpose_tours(tours_0, "total_access", "ho")

agents_1 <- agents_1 %>%
  join_purpose_tours(tours_1, "total_access", "ho")

# Group agents tables ----

res_var <- c("total_access_ho", "nr_tours_ho")
group_var <- c("area", "age_group")

agent_sums <- agents %>%
  group_sum(group_var, res_var)

agent_sums_0 <- agents_0 %>%
  group_sum(group_var, res_var)

agent_sums_1 <- agents_1 %>%
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
    present = total_access_ho / nr_tours_ho,
    projected = total_access_ho1 / nr_tours_ho1,
    baseline = total_access_ho0 / nr_tours_ho0
  )

# Plot ----

gap <- agent_sums %>%
  select(age_group, area, projected, baseline, present) %>%
  gather("scenario", "utility", projected, baseline, present) %>%
  group_by(scenario, area) %>%
  mutate(
    utility_dif = utility - mean(utility, na.rm = TRUE),
    scenario = case_when(
      scenario %in% "projected" ~ config::get("projected_name"),
      scenario %in% "baseline" ~ config::get("baseline_name"),
      scenario %in% "present" ~ config::get("present_name")
    )
  )

max_gap <- max(abs(gap$utility_dif), na.rm = TRUE) + 1

gap %>%
  ggplot(aes(x = age_group, y = utility_dif, fill = scenario)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "white",
    width = 0.8
  ) +
  facet_wrap( ~ area, nrow = 1) +
  scale_fill_manual(values = hsl_pal("blues")(3)) +
  theme_wide +
  ylim(-max_gap, max_gap) +
  geom_abline(slope = 0) +
  labs(fill = "Skenaario",
       y = "eur / kiertomatka",
       x = NULL,
       title = "Saavutettavuusero suhteessa alueen keskiarvoon",
       subtitle = "Kotiperäiset muut matkat")

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "age_group_access_gap_areas.png"
       ),
  width = dimensions_wide[1],
  height = dimensions_wide[2],
  units = "cm"
)
