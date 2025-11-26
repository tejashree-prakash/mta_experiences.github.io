Frequency_of_delays
================

This is an interactive visualization representing the frequency of
delays by subway line.

Packages (need to install gt package to use gt library first)

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

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(plotly)
```

    ## 
    ## Attaching package: 'plotly'
    ## 
    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter
    ## 
    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(knitr)
library(gt)
```

Load data.

``` r
raw <- read_csv("data/2019_subway_rider_data.csv") |> 
  clean_names()
```

    ## Rows: 10704 Columns: 19
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (18): subway_line_used_most_often, use_of_subway_frequency, get_to_subwa...
    ## lgl  (1): is_subway_affordable
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Clean dataset.

``` r
# split multi-line answers (e.g., "A/C" → "A", "C")
cleaned <- raw |> 
  mutate(
    subway_line = subway_line_used_most_often |> 
      str_replace_all("[^A-Za-z0-9]", "/") |>   # normalize separators
      str_split("/")                            # split into multiple lines
  ) |> 
  unnest(subway_line) |> 
  mutate(
    subway_line = str_trim(subway_line),
    subway_line = str_to_upper(subway_line)
  ) |> 
  filter(subway_line %in% c(
    "1","2","3","4","5","6","7",
    "A","B","C","D","E","F","M",
    "G","J","Z","N","Q","R","W","L","S"
  ))

# Convert column naming to an ordered factor for plotting:
cleaned <- cleaned |> 
  mutate(
    frequency_of_delays = factor(
      frequency_of_delays,
      levels = c(
        "Never",
        "Rarely",
        "Once a month",
        "A few times a month",
        "A few times a week",
        "Everyday"
      ),
      ordered = TRUE
    )
  )

#convert to a numeric midpoint in minutes (for average delay per line):
cleaned <- cleaned |> 
  mutate(
    delay_min = case_when(
      approximate_delay_duration == "< 10 min" ~ 5,
      approximate_delay_duration == "10 - 20 min" ~ 15,
      approximate_delay_duration == "20 - 40 min" ~ 30,
      approximate_delay_duration == "40 - 60 min" ~ 50,
      approximate_delay_duration == "Over 60 min" ~ 70,
      approximate_delay_duration == "Over 40 min" ~ 50,
      TRUE ~ NA_real_
    )
  )
```

Create a join-ready NYC subway line directory (subway lines & their
color families):

``` r
subway_directory <- tribble(
  ~subway_line, ~group_color, ~hex_color,
  "1","Red Line","#EE352E",
  "2","Red Line","#EE352E",
  "3","Red Line","#EE352E",

  "4","Green Line","#00933C",
  "5","Green Line","#00933C",
  "6","Green Line","#00933C",

  "7","Purple Line","#B933AD",

  "A","Blue Line","#0039A6",
  "C","Blue Line","#0039A6",
  "E","Blue Line","#0039A6",

  "B","Orange Line","#FF6319",
  "D","Orange Line","#FF6319",
  "F","Orange Line","#FF6319",
  "M","Orange Line","#FF6319",

  "N","Yellow Line","#FCCC0A",
  "Q","Yellow Line","#FCCC0A",
  "R","Yellow Line","#FCCC0A",
  "W","Yellow Line","#FCCC0A",

  "G","Light Green Line","#6CBE45",

  "J","Brown Line","#996633",
  "Z","Brown Line","#996633",

  "L","Gray Line","#A7A9AC",

  "S","Shuttle","#808183"
)

# Display directory table
kable(subway_directory, caption = "NYC Subway Lines and Color Groupings")
```

| subway_line | group_color      | hex_color |
|:------------|:-----------------|:----------|
| 1           | Red Line         | \#EE352E  |
| 2           | Red Line         | \#EE352E  |
| 3           | Red Line         | \#EE352E  |
| 4           | Green Line       | \#00933C  |
| 5           | Green Line       | \#00933C  |
| 6           | Green Line       | \#00933C  |
| 7           | Purple Line      | \#B933AD  |
| A           | Blue Line        | \#0039A6  |
| C           | Blue Line        | \#0039A6  |
| E           | Blue Line        | \#0039A6  |
| B           | Orange Line      | \#FF6319  |
| D           | Orange Line      | \#FF6319  |
| F           | Orange Line      | \#FF6319  |
| M           | Orange Line      | \#FF6319  |
| N           | Yellow Line      | \#FCCC0A  |
| Q           | Yellow Line      | \#FCCC0A  |
| R           | Yellow Line      | \#FCCC0A  |
| W           | Yellow Line      | \#FCCC0A  |
| G           | Light Green Line | \#6CBE45  |
| J           | Brown Line       | \#996633  |
| Z           | Brown Line       | \#996633  |
| L           | Gray Line        | \#A7A9AC  |
| S           | Shuttle          | \#808183  |

NYC Subway Lines and Color Groupings

Frequency of delays across all trains. The most delayed trains are 1. 2.
3.

Frequency of delays across line groupings (by color e.g. red line =
1,2,3; yellow line = n,q,r,w) The most delayed grouped line are 1. 2. 3.

Comment on the local vs. express comparison based on results.
