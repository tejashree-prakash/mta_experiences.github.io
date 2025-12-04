cleaned_data
================

load packages

``` r
library(tidyverse)
library(lubridate)
library(readxl)
```

Here I will clean the 2019_subway_rider_data

``` r
subway_rider_data = 
  read_csv("data/2019_Subway_Rider_Survey_20251121.csv") |> 
  janitor::clean_names()
```

    ## Rows: 10704 Columns: 19
    ## ── Column specification ───────────────────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (18): subway_line_used_most_often, use_of_subway_frequency, get_to_subwa...
    ## lgl  (1): is_subway_affordable
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
subway_drops =
  subway_rider_data |> 
  select(-survey_stop_borough, -survey_stop_location) |> 
  drop_na() |> 
  mutate(survey_id = row_number()) |> 
  relocate(survey_id, .before = 1)
```

Now I will specifically clean the data within the columns

Cleaned rider frequency table:

``` r
rider_data =
  subway_drops |>
  mutate(
    use_of_subway_frequency = case_when(
      use_of_subway_frequency == "Everyday / Almost everyday" ~ "Every day",
      use_of_subway_frequency == "More than once a week, varies" ~ "Several times a week",
      use_of_subway_frequency %in% c("Weekdays, Mon - Fri", "~5 days per week") ~ "5 days per week",
      use_of_subway_frequency == "Once a week" ~ "Once a week",
      use_of_subway_frequency == "Once a month" ~ "Once a month",
      use_of_subway_frequency == "Almost never" ~ "Almost never",
      use_of_subway_frequency == "Never" ~ "Never",
      TRUE ~ NA_character_
    ),
    use_of_subway_frequency = 
      factor(
        use_of_subway_frequency,
        levels = c("Never", 
                   "Almost never", 
                   "Once a month", 
                   "Once a week",
                   "Several times a week", 
                   "5 days per week", 
                   "Every day"),
        ordered = TRUE)
    )


rider_data =
  subway_drops |> 
  mutate(average_length_subway_ride = 
           factor(average_length_subway_ride,
                  levels = c("< 20 min", 
                             "20 - 40 min", 
                             "40 - 60 min",
                             "60 - 90 min", 
                             "90 min - 2 hours"),
                  ordered = TRUE))


rider_data =
  subway_drops |>
  mutate(
    overall_satisfaction = 
      recode(
        overall_satisfaction,
        "Not satisfied" = "Unsatisfied"),
    overall_satisfaction = 
      factor(
        overall_satisfaction,
        levels = c("Highly unsatisfied",
                 "Unsatisfied",
                 "Neutral",
                 "Somewhat satisfied",
                 "Very satisfied"),
      ordered = TRUE)
    )


rider_data =
  subway_drops |> 
  mutate(
    frequency_of_delays = factor(
      frequency_of_delays,
      levels = c("Everyday",
                 "A few times a week",
                 "Rarely",
                 "Almost never",
                 "Never"),
      ordered = TRUE)
    )
```

Making some of the variables as a character and some as a factor!

``` r
rider_data = 
  rider_data |> 
  mutate(
    zip_code = as.character(zip_code)
    )


rider_data =
  rider_data |> 
  mutate(
    across(c(subway_line_used_most_often,
             get_to_subway_via,
             primary_use_of_subway,
             alternative_transport), as.factor)
  )
```

``` r
rider_data = 
  rider_data %>%
  mutate(
    top_three_complaints = 
      str_remove_all(top_three_complaints, "\\[|\\]|'")
  )
```

Converting zip codes into boroughs. I used
<https://www.nyc.gov/assets/planning/download/office/data-maps/nyc-population/census2000/sf1p11.xls>
to get majority of the borough’s zip codes. Any remaining ones, I looked
up on Google and cross-checked. There were four zip codes that I could
not find a match for, and were likely miswritten, so I removed those
data rows.

``` r
zip_code_data = 
  read_csv("data/nyc_zipcodes.csv") %>%
  select(borough, zipcode)
```

    ## Rows: 343 Columns: 2
    ## ── Column specification ───────────────────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): borough, zipcode
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#Make sure character variables 
zip_code_data <- zip_code_data %>%
  mutate(zipcode = as.character(zipcode)) #make sure it's a character variable 
rider_data <- rider_data %>%
  mutate(zip_code = as.character(zip_code))

#Join
rider_data_full <- rider_data %>%
  left_join(zip_code_data, by = c("zip_code" = "zipcode"))
```

    ## Warning in left_join(., zip_code_data, by = c(zip_code = "zipcode")): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 6 of `x` matches multiple rows in `y`.
    ## ℹ Row 109 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence
    ##   this warning.

``` r
#Fill borough where missing
rider_data_full <- rider_data_full %>%
  mutate(borough = coalesce(borough, as.character(zip_code)))

#Remove invalid zip codes
rider_data_full <- rider_data_full %>%
  filter(survey_id != 2410) %>% 
  filter(survey_id != 944) %>% 
  filter(survey_id != 241) %>% 
  filter(survey_id != 919) 
```

# final data set is rider_data_full
