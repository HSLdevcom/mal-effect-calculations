library(tidyverse)
library(config)
library(here)

# Read files ----

file_path <-
  file.path(config::get("helmet_data"),
            config::get("present_scenario"),
            "agents.txt")
agents0 <- read_delim(file_path, delim = "\t")

file_path <-
  file.path(config::get("helmet_data"),
            config::get("baseline_scenario"),
            "agents.txt")
agents1 <- read_delim(file_path, delim = "\t")

file_path <-
  file.path(config::get("helmet_data"),
            config::get("projected_scenario"),
            "agents.txt")
agents2 <- read_delim(file_path, delim = "\t")

# Calc differences ----

calc_cumulative <- function(df, name){
  df %>%
    mutate(across(everything(), ~ replace_na(.x, 0))) %>%
    arrange(total_access) %>%
    mutate(
      cumulative_util = cumsum(total_access) / sum(total_access),
      cumulative_persons = 1:nrow(.) / nrow(.),
      scenario = name
    )
}

cumulative0 <- agents0 %>%
  calc_cumulative(config::get("present_name"))

cumulative1 <- agents1 %>%
  calc_cumulative(config::get("baseline_name"))

cumulative2 <- agents2 %>%
  calc_cumulative(config::get("projected_name"))

cumulative_dist <- bind_rows(cumulative0,
                             cumulative1,
                             cumulative2)

# Plot ----

cumulative_dist %>%
  ggplot(aes(
    x = cumulative_persons,
    y = cumulative_util,
    group = scenario,
    col = scenario
  )) +
  geom_line() +
  scale_color_manual(values = hsl_pal("grey_red_blue")(3)) +
  geom_abline(slope = 1,
              intercept = 0,
              col = hsl_cols("lightgray")) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
  theme_fig +
  labs(fill = "Skenaario",
       y = "Kumulatiivinen osuus matkojen hyödyistä",
       x = "Kumulatiivinen osuus henkilöistä",
       title = "Saavutettavuuden Lorenz-käyrä")

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "lorenz_curve_util.png"
       ),
  width = dimensions_fig[1],
  height = dimensions_fig[2],
  units = "cm"
)
