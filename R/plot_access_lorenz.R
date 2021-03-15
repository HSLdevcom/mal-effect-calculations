library("tidyverse")
library("config")
library("here")

# Read files ----

file_path <- file.path(get("helmet_data"), get("present_scenario"), "agents.txt")
agents0 <- read_delim(file_path, delim = "\t", col_names = TRUE) 

file_path <- file.path(get("helmet_data"), get("baseline_scenario"), "agents.txt")
agents1 <- read_delim(file_path, delim = "\t", col_names = TRUE) 

file_path <- file.path(get("helmet_data"), get("projected_scenario"), "agents.txt")
agents2 <- read_delim(file_path, delim = "\t", col_names = TRUE) 


# Calc differences ----

cumulative0 <- agents0 %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  arrange(total_access) %>%
  mutate(cumulative_util = cumsum(total_access) / sum(total_access),
         cumulative_persons = 1:nrow(.) / nrow(.),
         scenario = get("projected_name"))

cumulative1 <- agents1 %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  arrange(total_access) %>%
  mutate(cumulative_util = cumsum(total_access) / sum(total_access),
         cumulative_persons = 1:nrow(.) / nrow(.),
         scenario = get("projected_name"))

cumulative2 <- agents2 %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  arrange(total_access) %>%
  mutate(cumulative_util = cumsum(total_access) / sum(total_access),
         cumulative_persons = 1:nrow(.) / nrow(.),
         scenario = get("baseline_name"))

cumulative_dist <- bind_rows(cumulative0, cumulative1, cumulative2)

# Palma coefficient ----

sum(cumulative0 %>% 
      filter(cumulative_persons < 0.1) %>%
      select(cumulative_util)) / sum(cumulative0 %>% 
                                       filter(cumulative_persons < 0.4) %>%
                                       select(cumulative_util))

sum(cumulative1 %>% 
      filter(cumulative_persons < 0.1) %>%
      select(cumulative_util)) / sum(cumulative1 %>% 
                                       filter(cumulative_persons < 0.4) %>%
                                       select(cumulative_util))

sum(cumulative2 %>% 
      filter(cumulative_persons < 0.1) %>%
      select(cumulative_util)) / sum(cumulative2 %>% 
                                       filter(cumulative_persons < 0.4) %>%
                                       select(cumulative_util))

# Plot ----

cumulative_dist %>%
  ggplot(aes(x = cumulative_persons, y = cumulative_util, 
             group = scenario, col = scenario)) +
  geom_line() +
  scale_color_manual(values = hsl_pal("grey_red_blue")(3)) +
  geom_abline(slope = 1, intercept = 0, col = hsl_cols("lightgray")) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
  theme_fig +
  labs(fill = "Scenario",
       y = "Cumulative share of expected utility",
       x = "Cumulative share of persons",
       title = "Lorenz curve for accessibility") +
  ggsave(here("results", get("projected_scenario"), "lorenz_curve_util.png"),
         width = dimensions_fig[1], 
         height = dimensions_fig[2], 
         units = "cm")

