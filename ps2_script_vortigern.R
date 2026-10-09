library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get  = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

# Make raw data more readable
unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR" ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "TXUR" ~ "Texas",
      symbol == "NYUR" ~ "New York",
    )
  )

# Fill in month with missing data from govt shutdown
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

# Create a new column with the monthly change in the unemployment rate 
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

# Create new df containing only observations from 2015 onward
unemp_recent <- unemp |> 
  filter(date >= as.Date("2015-01-01"))

# Find mean, min, max unemployment rate across the whole sample (full set, and recent set)
unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

# Find Extrema 
unemp_top_increases <- unemp |> 
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE)
  
unemp_top_decreases <- unemp |> 
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE)

# Extrema Excluding COVID
unemp_top_increases_no_covid <- unemp |> 
  filter(date < as.Date("2020-01-01") | date > as.Date("2021-12-01")) |> 
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE)
  
unemp_top_decreases_no_covid <- unemp |> 
  filter(date < as.Date("2020-01-01") | date > as.Date("2021-12-01")) |> 
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE)

# Line Chart, Combined
ggplot(unemp, aes(x = date, y = unemployment_rate, color = factor(region))) +
  geom_line() +
  labs(title = "US Unemployment Rates, Cumulative and by Region", x = "Time (Years)", y = "Unemployment Rate (%)", color = "Region") +
  theme_minimal()

# Multiple Line Charts
ggplot(unemp, aes(x = date, y = unemployment_rate, color = factor(region))) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~region, axes = "all") + 
  labs(title = "US Unemployment Rate by Region", x = "Time (Years)", y = "Unemployment Rate (%)", color = "Region") +
  theme_minimal() 

# Bar Chart
ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate)) + 
  geom_bar(stat = "identity") +
  labs(title = "Average Unemployent Rate by Region (1976-2026)", x = "Region", y = "Mean Unemployment Rate (%)")



