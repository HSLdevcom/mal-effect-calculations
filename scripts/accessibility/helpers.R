library(tidyverse)
library(config)
library(here)

# Read files wrapper for agent data ----
read_helmet_files <- function(scenario, table) {
  read_delim(
    file.path(config::get("helmet_data"),
              config::get(scenario),
              paste0(table, ".txt")),
    delim = "\t",
    col_names = TRUE)
}

# Add income deciles for agents ----
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

# Factorize grouping variable for plotting ----
add_factors <- function(df, colname, factors) {
  df %>%
    dplyr::mutate(
      !!colname := forcats::as_factor(!!sym(colname)),
      !!colname := forcats::fct_relevel(!!sym(colname), !!!factors)
    )
}

# Translate factors ----
translate_vars <- function(df, colname, factors) {
  df %>%
    dplyr::mutate(
      !!colname := forcats::fct_recode(!!sym(colname), !!!factors)
    )
}

# Group and summarise with mean or sum ----
group_mean <- function(df, group_var, res_var){
  df <- df %>%
    group_by(!!!syms(group_var)) %>%
    summarise(across(all_of(res_var), mean, na.rm = TRUE)) %>%
    ungroup()
}

group_sum <- function(df, group_var, res_var){
  df <- df %>%
    group_by(!!!syms(group_var)) %>%
    summarise(across(all_of(res_var), sum, na.rm = TRUE)) %>%
    ungroup()
}

# Filter outliers ----
filter_outliers <- function(df, var){
  df %>%
    filter(
      !(abs(!!sym(var) - median(!!sym(var), na.rm = TRUE))
        > 2*sd(!!sym(var), na.rm = TRUE))
    )
}

# Join tour data to agents ----
join_tours <- function(agents, tours, res_var){
  tours <- tours %>%
    group_by(person_id) %>%
    summarise(across(all_of(res_var), sum, na.rm = TRUE),
              nr_tours = n())

  agents %>%
    left_join(tours, by = c("id" = "person_id"))
}

# Join tour data to agents with spesific purpose ----
join_purpose_tours <- function(agents, tours, res_var, purpose){
  tours <- tours %>%
    filter(purpose_name %in% purpose) %>%
    group_by(person_id) %>%
    summarise(across(all_of(res_var), sum, na.rm = TRUE),
              nr_tours = n())

  agents %>%
    left_join(tours,
              by = c("id" = "person_id"),
              suffix = c("", paste0("_", purpose))
    )
}
