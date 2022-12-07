r_final_proj
================

# Part I: Observing/Cleaning Data

## 2020 Census Data: Load & Clean (2021 data N/A as it is relased on 12/8)

1.  Load Data
2.  Choose variables
3.  Create a data frame named ny_df

``` r
census_api_key("aa512886c5449a582d837da8d3a07af66a043fe5")

census_data <- load_variables(2020, "acs5", cache=T)
fwrite(census_data, "census_variables.csv")

vars <- c(tpop = 'P001001',
          medage = 'P013001',
          wpop = 'P003002',
          bpop = 'P003003',
          apop = 'P003005',
          hpop = 'P004003')

ny_df <- get_decennial(state = "ny", 
                       geography = "county",
                       variables = vars,
                       geometry = T,
                       output = "wide")

ny_df = ny_df %>%
  mutate(
    county = gsub(" County, New York","",NAME))

ny_df$race_div <- 1 - (((ny_df$wpop*(ny_df$wpop-1))+
                          (ny_df$bpop*(ny_df$bpop-1))+
                          (ny_df$hpop*(ny_df$hpop-1))+
                          (ny_df$apop*(ny_df$apop-1)))/
                         (ny_df$tpop*(ny_df$tpop-1)))

ny_df = ny_df %>% 
  select(tpop, medage, county, race_div)
```

## 2019-2022 Health Ranking (HR) Data: Load & Clean

### 2019 HR Data

``` r
hr2019 = read_excel('data/ny_hr19.xls', sheet = 'Outcomes & Factors SubRankings', skip = 1) %>% 
  janitor::clean_names() %>% 
  slice(-c(1)) %>% 
  select(-starts_with("z"), -fips, -state) %>% 
  rename(longevity_r = rank_5,
         qol_r = rank_7,
         health_beh_r = rank_9,
         clinical_care_r = rank_11,
         ses_r = rank_13,
         env_r = rank_15)
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
demo2019 = read_excel('data/ny_hr19.xls', sheet = 'Additional Measure Data', skip = 1) %>% 
  janitor::clean_names() %>% 
  slice(-c(1)) %>% 
  select('county', 
         'segregation_index_2',
         'household_income',
         'percent_hispanic',
         'percent_african_american') %>% 
  rename(median_income = household_income,
         segregation_score = segregation_index_2,
         p_hispanic = percent_hispanic,
         p_black = percent_african_american) %>%
    mutate(segregation = case_when(segregation_score < 30 ~ 0,
                                 segregation_score >= 30 ~ 1),
           p_minority = p_black + p_hispanic)
```

    ## New names:
    ## • `95% CI - Low` -> `95% CI - Low...5`
    ## • `95% CI - High` -> `95% CI - High...6`
    ## • `# Deaths` -> `# Deaths...10`
    ## • `95% CI - Low` -> `95% CI - Low...12`
    ## • `95% CI - High` -> `95% CI - High...13`
    ## • `# Deaths` -> `# Deaths...17`
    ## • `95% CI - Low` -> `95% CI - Low...19`
    ## • `95% CI - High` -> `95% CI - High...20`
    ## • `# Deaths` -> `# Deaths...24`
    ## • `95% CI - Low` -> `95% CI - Low...26`
    ## • `95% CI - High` -> `95% CI - High...27`
    ## • `95% CI - Low` -> `95% CI - Low...32`
    ## • `95% CI - High` -> `95% CI - High...33`
    ## • `95% CI - Low` -> `95% CI - Low...35`
    ## • `95% CI - High` -> `95% CI - High...36`
    ## • `95% CI - Low` -> `95% CI - Low...38`
    ## • `95% CI - High` -> `95% CI - High...39`
    ## • `95% CI - Low` -> `95% CI - Low...50`
    ## • `95% CI - High` -> `95% CI - High...51`
    ## • `95% CI - Low` -> `95% CI - Low...53`
    ## • `95% CI - High` -> `95% CI - High...54`
    ## • `# Uninsured` -> `# Uninsured...55`
    ## • `% Uninsured` -> `% Uninsured...56`
    ## • `95% CI - Low` -> `95% CI - Low...57`
    ## • `95% CI - High` -> `95% CI - High...58`
    ## • `# Uninsured` -> `# Uninsured...59`
    ## • `% Uninsured` -> `% Uninsured...60`
    ## • `95% CI - Low` -> `95% CI - Low...61`
    ## • `95% CI - High` -> `95% CI - High...62`
    ## • `95% CI - Low` -> `95% CI - Low...67`
    ## • `95% CI - High` -> `95% CI - High...68`
    ## • `95% CI - Low` -> `95% CI - Low...76`
    ## • `95% CI - High` -> `95% CI - High...77`
    ## • `95% CI - Low` -> `95% CI - Low...80`
    ## • `95% CI - High` -> `95% CI - High...81`
    ## • `95% CI - Low` -> `95% CI - Low...84`
    ## • `95% CI - High` -> `95% CI - High...85`
    ## • `95% CI - Low` -> `95% CI - Low...88`
    ## • `95% CI - High` -> `95% CI - High...89`
    ## • `95% CI - Low` -> `95% CI - Low...107`
    ## • `95% CI - High` -> `95% CI - High...108`

``` r
total2019 = merge(x = hr2019, y = demo2019, by = "county", all.x = TRUE) %>% 
  mutate(year = 2019)
```

### 2020 HR Data

``` r
hr2020 = read_excel('data/ny_hr20.xlsx', sheet = 'Outcomes & Factors SubRankings', skip = 1) %>% 
  janitor::clean_names() %>% 
  slice(-c(1)) %>% 
  select(-starts_with("z"), -fips, -state) %>% 
  rename(longevity_r = rank_5,
         qol_r = rank_7,
         health_beh_r = rank_9,
         clinical_care_r = rank_11,
         ses_r = rank_13,
         env_r = rank_15)

demo2020 = read_excel('data/ny_hr20.xlsx', sheet = 'Additional Measure Data', skip = 1) %>% 
  janitor::clean_names() %>% 
  slice(-c(1)) %>% 
  select('county', 
         'median_household_income', 
         'segregation_index_2',
         'percent_black',
         'percent_hispanic') %>% 
  rename(median_income = median_household_income, 
         segregation_score = segregation_index_2,
         p_black = percent_black,
         p_hispanic = percent_hispanic) %>% 
    mutate(segregation = case_when(segregation_score < 30 ~ 0,
                                 segregation_score >= 30 ~ 1),
           p_minority = p_black + p_hispanic)

total2020 = merge(x = hr2020, y = demo2020, by = "county", all.x = TRUE) %>% 
  mutate(year = 2020)
```

### 2021 HR Data

``` r
hr2021 = read_excel('data/ny_hr21.xlsx', sheet = 'Outcomes & Factors SubRankings', skip = 1) %>% 
  janitor::clean_names() %>% 
  slice(-c(1)) %>% 
  select(-starts_with("z"), -fips, -state) %>% 
  rename(longevity_r = rank_5,
         qol_r = rank_7,
         health_beh_r = rank_9,
         clinical_care_r = rank_11,
         ses_r = rank_13,
         env_r = rank_15)
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
demo2021 = read_excel('data/ny_hr21.xlsx', sheet = 'Additional Measure Data', skip = 1) %>% 
  janitor::clean_names() %>% 
  slice(-c(1)) %>% 
  select('county', 
         'median_household_income', 
         'segregation_index_2',
         'percent_black',
         'percent_hispanic') %>% 
  rename(median_income = median_household_income, 
         segregation_score = segregation_index_2,
         p_black = percent_black,
         p_hispanic = percent_hispanic) %>% 
    mutate(segregation = case_when(segregation_score < 30 ~ 0,
                                 segregation_score >= 30 ~ 1),
           p_minority = p_black + p_hispanic)
```

    ## New names:
    ## • `95% CI - Low` -> `95% CI - Low...5`
    ## • `95% CI - High` -> `95% CI - High...6`
    ## • `# Deaths` -> `# Deaths...22`
    ## • `95% CI - Low` -> `95% CI - Low...24`
    ## • `95% CI - High` -> `95% CI - High...25`
    ## • `# Deaths` -> `# Deaths...41`
    ## • `95% CI - Low` -> `95% CI - Low...43`
    ## • `95% CI - High` -> `95% CI - High...44`
    ## • `# Deaths` -> `# Deaths...60`
    ## • `95% CI - Low` -> `95% CI - Low...62`
    ## • `95% CI - High` -> `95% CI - High...63`
    ## • `95% CI - Low` -> `95% CI - Low...80`
    ## • `95% CI - High` -> `95% CI - High...81`
    ## • `95% CI - Low` -> `95% CI - Low...83`
    ## • `95% CI - High` -> `95% CI - High...84`
    ## • `95% CI - Low` -> `95% CI - Low...86`
    ## • `95% CI - High` -> `95% CI - High...87`
    ## • `95% CI - Low` -> `95% CI - Low...96`
    ## • `95% CI - High` -> `95% CI - High...97`
    ## • `95% CI - Low` -> `95% CI - Low...115`
    ## • `95% CI - High` -> `95% CI - High...116`
    ## • `95% CI - Low` -> `95% CI - Low...133`
    ## • `95% CI - High` -> `95% CI - High...134`
    ## • `# Uninsured` -> `# Uninsured...135`
    ## • `% Uninsured` -> `% Uninsured...136`
    ## • `95% CI - Low` -> `95% CI - Low...137`
    ## • `95% CI - High` -> `95% CI - High...138`
    ## • `# Uninsured` -> `# Uninsured...139`
    ## • `% Uninsured` -> `% Uninsured...140`
    ## • `95% CI - Low` -> `95% CI - Low...141`
    ## • `95% CI - High` -> `95% CI - High...142`
    ## • `95% CI - Low` -> `95% CI - Low...148`
    ## • `95% CI - High` -> `95% CI - High...149`
    ## • `Average Grade Performance` -> `Average Grade Performance...150`
    ## • `Average Grade Performance (Asian)` -> `Average Grade Performance
    ##   (Asian)...151`
    ## • `Average Grade Performance (Black)` -> `Average Grade Performance
    ##   (Black)...152`
    ## • `Average Grade Performance (Hispanic)` -> `Average Grade Performance
    ##   (Hispanic)...153`
    ## • `Average Grade Performance (White)` -> `Average Grade Performance
    ##   (White)...154`
    ## • `Average Grade Performance` -> `Average Grade Performance...155`
    ## • `Average Grade Performance (Asian)` -> `Average Grade Performance
    ##   (Asian)...156`
    ## • `Average Grade Performance (Black)` -> `Average Grade Performance
    ##   (Black)...157`
    ## • `Average Grade Performance (Hispanic)` -> `Average Grade Performance
    ##   (Hispanic)...158`
    ## • `Average Grade Performance (White)` -> `Average Grade Performance
    ##   (White)...159`
    ## • `95% CI - Low` -> `95% CI - Low...161`
    ## • `95% CI - High` -> `95% CI - High...162`
    ## • `95% CI - Low` -> `95% CI - Low...182`
    ## • `95% CI - High` -> `95% CI - High...183`
    ## • `# Deaths` -> `# Deaths...199`
    ## • `95% CI - Low` -> `95% CI - Low...201`
    ## • `95% CI - High` -> `95% CI - High...202`
    ## • `95% CI - Low` -> `95% CI - Low...221`
    ## • `95% CI - High` -> `95% CI - High...222`
    ## • `95% CI - Low` -> `95% CI - Low...245`
    ## • `95% CI - High` -> `95% CI - High...246`
    ## • `95% CI - Low` -> `95% CI - Low...249`
    ## • `95% CI - High` -> `95% CI - High...250`
    ## • `95% CI - Low` -> `95% CI - Low...253`
    ## • `95% CI - High` -> `95% CI - High...254`
    ## • `95% CI - Low` -> `95% CI - Low...272`
    ## • `95% CI - High` -> `95% CI - High...273`

``` r
total2021 = merge(x = hr2021, y = demo2021, by = "county", all.x = TRUE) %>%
  mutate(year = 2021)
```

### 2022 HR Data:

Health Ranking Data

``` r
hr2022 = read_excel('data/ny_hr22.xlsx', sheet = 'Outcomes & Factors SubRankings', skip = 1) %>% 
  janitor::clean_names() %>% 
  slice(-c(1)) %>% 
  select(-starts_with("z"), -fips, -state) %>% 
  rename(longevity_r = rank_5,
         qol_r = rank_7,
         health_beh_r = rank_9,
         clinical_care_r = rank_11,
         ses_r = rank_13,
         env_r = rank_15)

demo2022 = read_excel('data/ny_hr22.xlsx', sheet = 'Additional Measure Data', skip = 1) %>% 
  janitor::clean_names() %>% 
  slice(-c(1)) %>% 
    select('county', 
         'median_household_income', 
         'segregation_index',
         'percent_black',
         'percent_hispanic') %>% 
  rename(median_income = median_household_income, 
         segregation_score = segregation_index,
         p_black = percent_black,
         p_hispanic = percent_hispanic) %>% 
    mutate(segregation = case_when(segregation_score < 30 ~ 0,
                                 segregation_score >= 30 ~ 1),
           p_minority = p_hispanic + p_black)

total2022 = merge(x = hr2022, y = demo2022, by = "county", all.x = TRUE) %>% 
  mutate(year = 2022)
```

Combine 2019-2022

``` r
hr_all = rbind(total2019, total2020, total2021, total2022)
```

## Load 2020 to November 2022 NY COVID-19 Data

Decided to look at variables related to covid testing (i.e., \# positive
cases, \# tests performed), covid vaccination rate, and covid-related
fatalities  
1. Import 3 separate csv files (ny_covidtest, ny_coviddeaths,
ny_covidvax) 3. Select variables of interest 4. Merge 3 data frames into
one by `date` and `county` 5. Convert `date` data type from char to date
6. Separate date into day, month, & year for analysis

``` r
ny_test = read_csv('data/ny_covidtest.csv') %>% 
janitor::clean_names() %>% 
filter(geography == 'COUNTY') %>% 
select(-geography) %>% 
rename(date = test_date) %>% 
mutate(date = lubridate::mdy(date))  

ny_death = read_csv("data/ny_coviddeaths.csv") %>% 
janitor::clean_names() %>% 
rename(date = report_date, n_fatality = place_of_fatality) %>%
select(-deaths_by_county_of_residence) %>% 
mutate(date = lubridate::mdy(date))

ny_vax = read_csv("data/ny_covidvax.csv") %>% 
janitor::clean_names() %>% 
rename(date = report_as_of) %>% 
select(-region, -series_complete) %>% 
mutate(date = lubridate::mdy(date))

ny_covid_v1 = left_join(ny_test, ny_death, by = c("date", "county"))

covid_df_v1 = left_join(ny_covid_v1, ny_vax, by = c("date", "county")) %>% 
  separate(date, c("year", "month", "day"), sep = "-") %>% 
  mutate(year = as.numeric(year), month = as.numeric(month), day = as.numeric(day))

covid_df = covid_df_v1 %>% 
  group_by(county, year) %>% 
  summarise(n_cases = sum(new_positives),
            vax_dose1 = max(first_dose),
            n_deaths = max(n_fatality, na.rm = TRUE))
```

## Merge Data for Analysis

``` r
covid_hr = left_join(covid_df, hr_all, by = c("county", "year"))
comb = left_join(covid_hr, ny_df, by = 'county') %>% 
  mutate(tpop_50 = tpop/2,
         vax_maj = case_when(vax_dose1 >= .73*(tpop) ~ 1,
                             vax_dose1 < .73*(tpop) ~ 0))
```
