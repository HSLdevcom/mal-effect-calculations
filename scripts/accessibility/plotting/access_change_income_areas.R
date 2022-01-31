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

income_names <- c("1 (alin 20 %)", "2", "3", "4", "5 (ylin 20 %)")
max_gap <- max(abs(agent_sums$util_dif)) + 1

agent_sums %>%
  ggplot() +
  geom_col(
    aes(x = area, y = util_dif, fill = income_group),
    position = position_dodge2()
  ) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",")
  ) +
  scale_x_discrete(
    labels = scales::label_wrap(5)
  ) +
  scale_fill_brewer(
    palette = "Dark2",
    name = NULL,
    labels = income_names
  ) +
  geom_abline(slope = 0) +
  labs(
    y = "euroa kiertomatkaa kohden",
    x = NULL,
    title = "Asukkaan tekemien matkojen saavutettavuuden muutos\nkotiperäisillä työmatkoilla tuloluokan mukaan",
    subtitle = sprintf("%s \U2192 %s", config::get("baseline_name"), config::get("projected_name"))
    ) +
  theme_mal_graph()

ggsave_graph(
  here("figures",
       config::get("projected_scenario"),
       "access_change_income_areas.png"
       )
)
