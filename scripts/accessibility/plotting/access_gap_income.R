library(tidyverse)
library(config)
library(here)

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

# Group agents tables ----

res_var <- c("total_access", "nr_tours")
group_var <- c("income_group")

sum_agents <- function(df){
  df <- df %>%
    group_by(!!!syms(group_var)) %>%
    summarise(across(all_of(res_var), sum, na.rm = TRUE)) %>%
    ungroup()
}

agents <- agents %>%
  sum_agents()

agents_0 <- agents_0 %>%
  sum_agents()

agents_1 <- agents_1 %>%
  sum_agents()

# Join tables ----

agents <- full_join(agents,
                    agents_0,
                    by = group_var,
                    suffix = c("", "0"))

agents <- full_join(agents,
                    agents_1,
                    by = group_var,
                    suffix = c("", "1"))

# Calc differences ----

agents <- agents %>%
  mutate(
    present = total_access / nr_tours,
    projected = total_access1 / nr_tours1,
    baseline = total_access0 / nr_tours0
  )

# Plot ----
income_names <- c("alin 10 %", rep("", 8), "ylin 10 %")

# Plot ----

gap <- agents %>%
  select(income_group, projected, baseline, present) %>%
  gather("scenario", "utility", projected, baseline, present) %>%
  group_by(scenario) %>%
  mutate(
    utility_dif = utility - median(utility, na.rm = TRUE),
    scenario = case_when(
      scenario %in% "projected" ~ config::get("projected_name"),
      scenario %in% "baseline" ~ config::get("baseline_name"),
      scenario %in% "present" ~ config::get("present_name")
    )
  )

gap %>%
  ggplot(aes(x = income_group, y = utility_dif, fill = scenario)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "white",
    width = 0.8
  ) +
  scale_fill_manual(values = hsl_pal("blues")(3)) +
  scale_x_discrete(labels = income_names) +
  theme_fig +
  geom_abline(slope = 0) +
  labs(fill = "Skenaario",
       y = "eur / kiertomatka",
       x = NULL,
       title = "Saavutettavuusero suhteessa alueen keskiarvoon")

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "access_gap_income.png"
       ),
  width = dimensions_fig[1],
  height = dimensions_fig[2],
  units = "cm"
)
