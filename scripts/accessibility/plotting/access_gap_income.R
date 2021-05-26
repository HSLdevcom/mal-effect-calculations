library(tidyverse)
library(config)
library(here)

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

# Join tours to agents data ----

join_filter_tours <- function(agents, tours, res_var){
  agents %>%
    left_join(
      tours %>%
        filter(purpose_name == "hw") %>%
        group_by(person_id) %>%
        summarise(across(all_of(res_var), sum, na.rm = TRUE),
                  nr_tours = n()),
      by = c("id" = "person_id")
    )
}

agents <- agents %>%
  join_filter_tours(tours, "total_access") %>%
  filter(income_group %in% 1:10)

agents_0 <- agents_0 %>%
  join_filter_tours(tours_0, "total_access") %>%
  filter(income_group %in% 1:10)

agents_1 <- agents_1 %>%
  join_filter_tours(tours_1, "total_access") %>%
  filter(income_group %in% 1:10)

# Group agents tables ----

res_var <- c("total_access", "nr_tours")
group_var <- c("income_group")

group_sum <- function(df){
  df <- df %>%
    group_by(!!!syms(group_var)) %>%
    summarise(across(all_of(res_var), sum, na.rm = TRUE)) %>%
    ungroup()
}

agents <- agents %>%
  group_sum()

agents_0 <- agents_0 %>%
  group_sum()

agents_1 <- agents_1 %>%
  group_sum()

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
       title = "Saavutettavuusero suhteessa alueen keskiarvoon",
       subtitle = "Kotiperäiset työmatkat")

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "access_gap_income.png"
       ),
  width = dimensions_fig[1],
  height = dimensions_fig[2],
  units = "cm"
)
