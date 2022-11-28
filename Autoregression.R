library(dplyr)

ny_covid = read.csv('data/covid_testing.csv')  %>%
  janitor::clean_names()

###############################
ny_covid$test_date = lubridate::mdy(ny_covid$test_date)
sub_df = ny_covid[ny_covid$county == 'Albany', ]


###### ACF plot #######
library(forecast)
Acf(sub_df$new_positives)



### Auto regression ####
library(ggplot2)
reg1 = Arima(sub_df$new_positives, order = c(10,0,0))
summary(reg1)


