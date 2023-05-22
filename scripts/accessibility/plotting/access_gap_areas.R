library(tidyverse)
library(here)
source(here("scripts", "accessibility", "helpers.R"),
       encoding = "utf-8")

# Parameters for plotting ----

areas <-
  c("Helsingin kantakaupunki",
    "Muu Helsinki",
    "Muu pääkaupunkiseutu")

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
group_var <- c("area")

agent_sums <- agents %>%
  group_sum(group_var, res_var) %>%
  filter(area %in% areas)

agent_sums_0 <- agents_0 %>%
  group_sum(group_var, res_var) %>%
  filter(area %in% areas)

agent_sums_1 <- agents_1 %>%
  group_sum(group_var, res_var) %>%
  filter(area %in% areas)

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

gap <- agent_sums %>%
  select(area, projected, baseline, present) %>%
  gather("scenario", "utility", projected, baseline, present) %>%
  group_by(scenario) %>%
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

gap %>%
  ggplot(aes(x = area, y = utility_dif, fill = scenario)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "white",
    width = 0.8
  ) +
  scale_fill_manual(values = hsl_pal("blues")(3)) +
  theme_fig +
  geom_abline(slope = 0) +
  labs(fill = "Skenaario",
       y = "eur / kiertomatka",
       x = NULL,
       title = "Saavutettavuusero suhteessa seudun keskiarvoon",
       subtitle = "Kotiperäiset työmatkat") +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"))

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "area_access_gap_areas.png"
  ),
  width = dimensions_fig[1],
  height = dimensions_fig[2],
  units = "cm"
)
