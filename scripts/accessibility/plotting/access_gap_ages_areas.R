library(tidyverse)
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
    ),
    scenario = factor(scenario, levels = c(config::get("present_name"),
                                           config::get("baseline_name"),
                                           config::get("projected_name")))
  )

max_gap <- max(abs(gap$utility_dif), na.rm = TRUE) + 1

gap %>%
  ggplot(aes(x = age_group, y = utility_dif, group = scenario)) +
  facet_grid(cols = vars(area), switch = "both", labeller = labeller(.cols = scales::label_wrap(10)), margin = 10) +
  geom_col(fill = "white", position = position_dodge2()) +
  geom_col(aes(fill = age_group, alpha = scenario), position = position_dodge2()) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", accuracy = 0.01)
  ) +
  scale_x_discrete(
    labels = NULL
  ) +
  scale_fill_brewer(
    palette = "Set2",
    name = "Ikäryhmä",
    guide = guide_legend(order = 1)
  ) +
  scale_alpha_discrete(
    name = "Skenaario",
    range = c(0.333, 1),
    guide = guide_legend(reverse = FALSE, order = 2)
  ) +
  geom_abline(slope = 0) +
  labs(y = "euroa kiertomatkaa kohden",
       x = NULL,
       title = "Saavutettavuuden ero alueen keskiarvoon\nkotiperäisillä muilla matkoilla ikäryhmittäin") +
  theme_mal_graph() +
  theme(strip.background = element_rect(fill = NA, colour = "grey40", linewidth = 0.5),
        panel.spacing.x = unit(1, unit = "mm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.title = element_text(face = "bold"))

ggsave_graph(
  here("figures",
       config::get("projected_scenario"),
       "access_gap_age_group_areas.png"
       ),
  width = 150, height = 84
)
