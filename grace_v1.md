r_final_proj
================

## data import

### health ranking data

``` r
ny_hr2020 = read_excel('data1/ny_hr20.xlsx', sheet = 'Outcomes & Factors SubRankings', skip = 1) %>% 
  janitor::clean_names() %>% 
  slice(-c(1)) %>% 
  select(-ends_with("z"), -fips)
```

    ## New names:
    ## • `Z-Score` -> `Z-Score...4`
    ## • `Rank` -> `Rank...5`
    ## • `Z-Score` -> `Z-Score...6`
    ## • `Rank` -> `Rank...7`
    ## • `Z-Score` -> `Z-Score...8`
    ## • `Rank` -> `Rank...9`
    ## • `Z-Score` -> `Z-Score...10`
    ## • `Rank` -> `Rank...11`
    ## • `Z-Score` -> `Z-Score...12`
    ## • `Rank` -> `Rank...13`
    ## • `Z-Score` -> `Z-Score...14`
    ## • `Rank` -> `Rank...15`

``` r
ny_hr2021 = read_excel('data1/ny_hr21.xlsx', sheet = 'Outcomes & Factors SubRankings', skip = 1) %>% 
  janitor::clean_names() %>% 
  slice(-c(1)) %>% 
  select(-ends_with("z"), -fips)
```

    ## New names:
    ## • `Z-Score` -> `Z-Score...4`
    ## • `Rank` -> `Rank...5`
    ## • `Z-Score` -> `Z-Score...6`
    ## • `Rank` -> `Rank...7`
    ## • `Z-Score` -> `Z-Score...8`
    ## • `Rank` -> `Rank...9`
    ## • `Z-Score` -> `Z-Score...10`
    ## • `Rank` -> `Rank...11`
    ## • `Z-Score` -> `Z-Score...12`
    ## • `Rank` -> `Rank...13`
    ## • `Z-Score` -> `Z-Score...14`
    ## • `Rank` -> `Rank...15`

``` r
ny_hr2022 = read_excel('data1/ny_hr22.xlsx', sheet = 'Outcomes & Factors SubRankings', skip = 1) %>% 
  janitor::clean_names() %>% 
  slice(-c(1)) %>% 
  select(-ends_with("z"), -fips)
```

### covid data

``` r
ny_test = read_csv('data1/ny_covidtest.csv') %>% 
janitor::clean_names() %>% 
rename(date_mdy = test_date)
```

    ## Rows: 72197 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): Test Date, County, Test % Positive, Geography
    ## dbl (4): New Positives, Cumulative Number of Positives, Total Number of Test...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ny_death = read_csv("data1/ny_coviddeaths.csv") %>% 
janitor::clean_names() %>% 
rename(date_mdy = report_date)
```

    ## Rows: 57728 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Report Date, County
    ## dbl (2): Place of Fatality, Deaths by County of Residence
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ny_vax = read_csv("data1/ny_covidvax.csv") %>% 
janitor::clean_names() %>% 
select(-region) %>%
rename(date_mdy = report_as_of)
```

    ## Rows: 43462 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Region, County, Report as of
    ## dbl (2): First Dose, Series Complete
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#ny_covidcomb = left_join(
```
