library(dplyr)

ny_covid = read.csv('data/covid_testing.csv')  %>%
  janitor::clean_names()

###############################
ny_covid$test_date = lubridate::mdy(ny_covid$test_date)
sub_df = ny_covid[ny_covid$county == 'New York City', ]
counties = levels(as.factor(ny_covid$county))
county_df_list = list()
# loop version 2
for (i in counties) {
  sub_df = ny_covid[ny_covid$county == i, ]
  county_df_list[[i]] = sub_df
}

net_df = matrix(nrow = 900)
for (i in county_df_list){
  net_df = cbind(net_df,i$new_positives[1:900])
}
net_df = net_df[,-c(1)]
net_df = data.frame(net_df)
colnames(net_df) = c(counties)

########  ACF plot #######
library(forecast)
conf.level <- 0.95
ciline <- qnorm((1 - conf.level)/2)/sqrt(length(sub_df$new_positives))
bacf <- acf(sub_df$new_positives, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))

library(ggplot2)
q <- ggplot(data=bacfdf, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity",fill="lightblue")  
q

### Auto regression ####
reg1 = Arima(sub_df$new_positives, order = c(10,0,0))
summary(reg1)

