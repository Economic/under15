library(tidyverse)
library(epiextractr)
library(epidatatools)
library(lubridate)
library(haven)

org <- load_org(2021:2022) |>
  mutate(my_wage = wageotc) |>
  filter(my_wage > 0) |>
  mutate(month_date = ym(paste(year, month)))
max_date <- org |>
  summarize(max(month_date)) |>
  pull()

min_date <- max_date - months(11)

create_slice <- function(threshold) {
  org |>
    filter(month_date >= min_date & month_date <= max_date) |>
    mutate(low_wage = my_wage < threshold) |>
    mutate(all = 1) |>
    mutate(all = labelled(all, c("All workers" = 1))) |>
    mutate(union = labelled(union, c("In a union" = 1, "Not in a union" = 0))) |>
    mutate(part_time = if_else(ftptstat >= 6 & ftptstat <= 10, 1, 0)) |>
    mutate(part_time = labelled(part_time, c("Works part-time" = 1, "Works full-time" = 0))) |>
    mutate(new_educ = ifelse(educ <= 4, educ, 4)) |>
    mutate(new_educ = labelled(new_educ, c(
      "Less than high school diploma" = 1,
      "High school diploma" = 2,
      "Some college" = 3,
      "College or advanced degree" = 4
    ))) |>
    mutate(new_age = case_when(
      age >= 16 & age <= 24 ~ 1,
      age >= 25 & age <= 34 ~ 2,
      age >= 35 & age <= 44 ~ 3,
      age >= 45 & age <= 54 ~ 4,
      age >= 55 & age <= 64 ~ 5,
      age >= 65 ~ 6
    )) |> 
    mutate(new_age = labelled(new_age , c(
      "Ages 16-24" = 1,
      "Ages 25-34" = 2,
      "Ages 35-44" = 3,
      "Ages 45-54" = 4,
      "Ages 55-64" = 5,
      "Ages 65 and above" = 6
    ))) |>
    summarize_groups(
      all|wbhao|female|union|part_time|new_educ|new_age, 
      low_wage_share = weighted.mean(low_wage, w = orgwgt),
      low_wage_count = round(sum(low_wage * orgwgt / 12) / 1000)*1000
    ) |>
    mutate(dates = paste(format(min_date, "%B %Y"), "through", format(max_date, "%B %Y"))) |>
    mutate(low_wage_threshold = threshold)
}

results <- map_dfr(15, create_slice) |>
  filter(group_value_label != "Other") |>
  transmute(
    category = group_value_label, 
    category_group = group_name, 
    low_wage_share = round(low_wage_share * 100),
    low_wage_threshold,
    low_wage_count, 
    dates
  ) |>
  mutate(priority = case_when(
    category_group == "all" ~ 100,
    category_group == "wbhao" ~ 99,
    category_group == "female" ~ 98,
    category == "Ages 65 and above" ~ 1,
    category_group == "new_age" ~ 2,
    TRUE ~ 0
  )) |>
  mutate(category_group = case_when(
    category_group == "all" ~ "All workers",
    category_group == "female" ~ "Gender",
    category_group == "union" ~ "Union status",
    category_group == "wbhao" ~ "Race and ethnicity",
    category_group == "part_time" ~ "Part-time status",
    category_group == "new_educ" ~ "Education",
    category_group == "new_age" ~ "Age group"
  )) |>
  arrange(low_wage_threshold, desc(priority), category_group, category) 

results |>
  print(n=Inf)

write_csv(results, "low_wage_data.csv")
