getwd()
library(ggplot2)
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year) %>% 
  janitor::clean_names()



NY_df <- get_decennial(state = "ny", 
                       geography = "county",
                       variables = vars,
                       geometry = T,
                       output = "wide")
NY_df %>%
  janitor::clean_names()
NY_df = NY_df %>%
  mutate(
    county = gsub(" County, New York","",NAME))


# Most basic bubble plot
library(plotly)
data = read.csv("./data/bubble.csv") %>%
  mutate(County = as.factor(County),
         health = as.numeric(health),
         socio = as.numeric(socio)) %>%
  janitor::clean_names()


total <- merge(data,NY_df,by="county")

fig <- plot_ly(total, x = ~health, y = ~socio, text = ~county, type = 'scatter',
               mode = 'markers', size = ~tpop, color = ~county, colors = 'Paired',
               marker = list(opacity = 0.5, sizemode = 'diameter'))
fig <- fig %>% layout(title = 'health-socio',
                      xaxis = list(showgrid = FALSE),
                      yaxis = list(showgrid = FALSE),
                      showlegend = FALSE)

fig


p2 =  total %>%
  ggplot(aes(fill = health)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") 

p2
total$Geometry = total$geometry

p1 = NY_df  %>%
  ggplot(aes(fill = medage)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") 
p1

rsconnect::setAccountInfo(name='jagjit-singh', token='A9B0E64A15CDF1328B2A2D89AA0C8AAE', secret='6pNP/jt0TLjHOn+fuj0eqrDyDKm/qj1xyKQDgsCX')
