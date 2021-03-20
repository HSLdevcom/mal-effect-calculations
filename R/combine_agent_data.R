library("tidyverse")
library("config")
library("here")

# Helper functions ----

# Read files wrapper for agent data
read <- function(name) {
  read_delim(file.path(get("helmet_data"), get(name), "agents.txt"),
             delim = "\t",
             col_names = TRUE) %>%
    select(-"X1")
}

# Grouping wrapper for agent data
group <- function(df, group_var) {
  df %>%
    mutate(persons = 1) %>%
    group_by(!!!syms(group_var)) %>%
    summarise_all(sum, na.rm = TRUE) %>%
    ungroup()
}

# Add income deciles for agents
# Separate under 18 years old from calculation
add_inc_group <- function(df) {
  df0 <- df %>%
    filter(!age_group %in% "age_7-17") %>%
    arrange(income) %>%
    mutate(income_group = ceiling(10 * row_number() / n()))

  df1 <- df %>%
    filter(age_group %in% "age_7-17") %>%
    mutate(income_group = -1)

  df <- bind_rows(df0, df1) %>%
    mutate(income_group = as.factor(income_group))
}

# Load data ----

agents <- read("present_scenario")
agents_0 <- read("baseline_scenario")
agents_1 <- read("projected_scenario")

# Calculate income group ----

agents <- agents %>%
  add_inc_group()

agents_0 <- agents_0 %>%
  add_inc_group()

agents_1 <- agents_1 %>%
  add_inc_group()

# Group data for join ----
grouping_vars <- c("number", "area", "municipality", "age_group", "gender", "income_group")

agents <- agents %>%
  group(grouping_vars)

agents_0 <- agents_0 %>%
  group(grouping_vars)

agents_1 <- agents_1 %>%
  group(grouping_vars)

# Join agents tables ----
agents <- left_join(agents, agents_0,
                    by = grouping_vars, suffix = c("", "0"))

agents <- left_join(agents, agents_1,
                    by = grouping_vars, suffix = c("", "1"))

# Factorize grouping variable for plotting ----

agents <- agents %>%
  mutate(age_group = forcats::as_factor(age_group),
         age_group = forcats::fct_relevel(age_group, c("age_7-17",
                                                       "age_18-29",
                                                       "age_30-49",
                                                       "age_50-64",
                                                       "age_65-99")))

agents <- agents %>%
  mutate(area = forcats::as_factor(area),
         area = forcats::fct_relevel(area, c("helsinki_cbd",
                                             "helsinki_other",
                                             "espoo_vant_kau",
                                             "surrounding")))

# Write to file ----
agents %>%
  write_rds(here("results", get("projected_scenario"), "agents.rds"))
