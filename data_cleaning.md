cleaned_data
================

load packages

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

Here I will clean the 2019_subway_rider_data

``` r
subway_rider_data = 
  read_csv("data/2019_subway_rider_data.csv") |> 
  janitor::clean_names()
```

    ## Rows: 10704 Columns: 19
    ## ── Column specification ────────────────────────────────────────────────────────
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

converting complaints into long format:

``` r
rider_data =
  rider_data |> 
  mutate(
    top_three_complaints = 
      str_remove_all(top_three_complaints, "\\[|\\]|'")
  ) |> 
  separate_rows(top_three_complaints, sep = ",") |> 
  mutate(top_three_complaints = 
           str_trim(top_three_complaints)) |> 
  rename(complaint = top_three_complaints)
```

Making some of the variables as a character and some as a factor!

``` r
rider_data = 
  rider_data |> 
  mutate(
    is_subway_affordable = if_else(
      is_subway_affordable == "Yes", TRUE, FALSE),
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

\<\<\<\<\<\<\< HEAD Converting zip codes into boroughs. I used
<https://www.nyc.gov/assets/planning/download/office/data-maps/nyc-population/census2000/sf1p11.xls>
to get majority of the borough’s zip codes. Any remaining ones, I looked
up on Google. There were three zip codes that I could not find a match
for, and were likely miswritten, so I removed those data rows.

``` r
zip_code_data = 
  read_csv("data/nyc_zipcodes.csv") %>%
  select(borough, zipcode)
```

    ## New names:
    ## Rows: 343 Columns: 6
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (2): borough, zipcode lgl (4): ...3, ...4, ...5, ...6
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`
    ## • `` -> `...6`

``` r
#Make sure character type
rider_data <- rider_data %>%
  mutate(zip_code = as.character(zip_code))
zip_code_data <- zip_code_data %>%
  mutate(zipcode = as.character(zipcode))

#Join
rider_data_full <- rider_data %>%
  left_join(zip_code_data, by = c("zip_code" = "zipcode"))
```

    ## Warning in left_join(., zip_code_data, by = c(zip_code = "zipcode")): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 14 of `x` matches multiple rows in `y`.
    ## ℹ Row 199 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
#If there's no match, we want to keep the zip_code in the borough column, so we use coalesce to handle this
rider_data_full <- rider_data_full %>%
  mutate(borough = coalesce(borough, as.character(zip_code)))

#manually go through remaining zip codes - removing 11135 11290 12105 as they are not real zip codes
rider_data_full <- rider_data_full %>%
  filter(zip_code != 12105) %>%
  filter(zip_code != 11290) %>% 
  filter(zip_code != 11135) 
```

I think that maybe top 3 complaints shouldn’t be separated into
individual rows because now it’s double or triple counting all of the
other responses in other columns.

should approximate delay duration be an ordered factor?

final data set is rider_data_full

should approximate delay duration be an ordered factor?

anyways final data set is rider_data
