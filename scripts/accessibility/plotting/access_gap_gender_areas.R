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
        filter(purpose_name == "ho") %>%
        group_by(person_id) %>%
        summarise(across(all_of(res_var), sum, na.rm = TRUE),
                  nr_tours = n()),
      by = c("id" = "person_id")
    )
}

agents <- agents %>%
  join_filter_tours(tours, "total_access")

agents_0 <- agents_0 %>%
  join_filter_tours(tours_0, "total_access")

agents_1 <- agents_1 %>%
  join_filter_tours(tours_1, "total_access")

# Group agents tables ----

res_var <- c("total_access", "nr_tours")
group_var <- c("area", "gender")

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

gap <- agents %>%
  select(gender, area, projected, baseline, present) %>%
  gather("scenario", "utility", projected, baseline, present) %>%
  group_by(scenario, area) %>%
  mutate(
    utility_dif = utility - mean(utility, na.rm = TRUE),
    scenario = case_when(
      scenario %in% "projected" ~ get("projected_name"),
      scenario %in% "baseline" ~ get("baseline_name"),
      scenario %in% "present" ~ get("present_name")
    )
  )

max_gap <- max(abs(gap$utility_dif)) + 1

gap %>%
  ggplot(aes(x = gender, y = utility_dif, fill = scenario)) +
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
       title = "Saavutettavuusero suhteessa alueen keskiarvoon")

ggsave(
  here("figures",
       get("projected_scenario"),
       "access_gap_gender_areas.png"
       ),
  width = dimensions_wide[1],
  height = dimensions_wide[2],
  units = "cm"
)
