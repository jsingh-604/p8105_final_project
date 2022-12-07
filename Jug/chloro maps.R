setwd("")
require(data.table)
require(tidycensus)
require(ggplot2)
library(dplyr)
library(tidyverse)
census_api_key("aa512886c5449a582d837da8d3a07af66a043fe5")

census_data <- load_variables(2018, "acs5", cache=T)
fwrite(census_data, "census_variables.csv")

vars <- c(tpop = 'P001001',
          medage = 'P013001',
          wpop = 'P003002',
          bpop = 'P003003',
          apop = 'P003005',
          hpop = 'P004003')

NY_df <- get_decennial(state = "ny", 
                       geography = "county",
                       variables = vars,
                       geometry = T,
                       output = "wide")
View(NY_df)


NY_df = NY_df %>%
  mutate(
    county = gsub(" County, New York","",NAME))


NY_df  %>%
  ggplot(aes(fill = medage)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") 

ny_covid = read.csv('data/covid_testing.csv')  %>%
  janitor::clean_names()

###############################
ny_covid$test_date = lubridate::mdy(ny_covid$test_date)
##############################
total <- merge(ny_covid,NY_df,by="county")

sub_df = total[total$test_date >= "2020-12-25" & total$test_date <= "2021-12-28", ]

sub_df$Geometry = sub_df$geometry

group_df =  sub_df %>% group_by(county)  %>%
  summarise(total_covid = sum(new_positives))
##############################
df = dplyr :: left_join(group_df, NY_df, by = 'county')

df %>%
  ggplot() +
  geom_sf(aes(fill = total_covid, geometry = geometry)) + 
  scale_fill_viridis_c(option = "viridis") 

  
mon = seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "months")
df_list = list()
# loop version 2
for (i in 1:11) {
  sub_df = total[total$test_date >= mon[[i]] & total$test_date <= mon[[i+1]], ]
  group_df =  sub_df %>% group_by(county)  %>%
    summarise(total_covid = sum(new_positives))
  df = dplyr :: left_join(group_df, NY_df, by = 'county')
  df_list[[i]] = df
}

length(df_list)

library(gridExtra)
library(ggplot2)
p <- list()
for(i in 1:11){
  p[[i]] <- df_list[[i]] %>%
    ggplot() +
    geom_sf(aes(fill = total_covid, geometry = geometry)) + 
    scale_fill_viridis_c(option = "viridis") 
}
do.call(grid.arrange,p)

df_list[[4]] %>%
  ggplot() +
  geom_sf(aes(fill = total_covid, geometry = geometry)) + 
  scale_fill_viridis_c(option = "viridis") 

######################3
v = total %>%
  filter(
    test_date %in% mon[[1]]:mon[[2]]
  )

