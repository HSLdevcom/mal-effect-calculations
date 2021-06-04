library(tidyverse)
library(config)
library(here)

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

# Filter and group agents tables ----

agent_sums <- agents %>%
  filter(income_group %in% 1:10) %>%
  group_by(area, income_group) %>%
  summarise(persons = n())

# Plot ----

income_names <- c("alin 10 %", rep("", 8), "ylin 10 %")

agent_sums %>%
  ggplot(aes(x = income_group, y = persons)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    fill = hsl_cols("blue"),
    color = "white",
    width = 0.8
  ) +
  facet_wrap( ~ area, nrow = 1) +
  theme_wide +
  geom_abline(slope = 0) +
  scale_x_discrete(labels = income_names) +
  labs(
    y = "eur / asukas",
    x = "Skenaarion tulodesiilit",
    title = paste0(
      "Asukkaiden lukumäärä tulosdesiileissä: ",
      config::get("projected_name")
    )
  )

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "persons_income_areas.png"
       ),
  width = dimensions_wide[1],
  height = dimensions_wide[2],
  units = "cm"
)
