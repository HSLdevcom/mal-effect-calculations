library(tidyverse)
library(config)
library(here)

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

# Group agents tables ----

agents <- agents %>%
  select(id, gender)

tours <- tours %>%
  left_join(agents, by = c("person_id"="id"))

tour_sums <- tours %>%
  group_by(mode, gender) %>%
  summarise(nr_tours = n()) %>%
  group_by(gender) %>%
  mutate(share = nr_tours / sum(nr_tours))

# Plot ----

mode_colors <- hsl_cols("green" ,"lightgreen", "pink", "lightblue")

tour_sums %>%
  ggplot(aes(x = gender, y = share,
             label = round(100 * share, 0), fill = mode)) +
  geom_bar(
    stat = "identity",
    position = "stack",
    color = "white",
    width = 0.8
  ) +
  theme_fig +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent) +
  geom_text(size = 3,
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = mode_colors) +
  labs(fill = "Kulkutapa",
       y = "osuus kieromatkoista",
       x = "sukupuoli")

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "mode_share_gender.png"
       ),
  width = dimensions_fig[1],
  height = dimensions_fig[2],
  units = "cm"
)
