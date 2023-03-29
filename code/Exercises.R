library(tidyverse)

gapminder_data <- read_csv('Desktop/un-report/data/gapminder_data.csv')

ggplot(gapminder_data) +
  aes(x = year, y = pop) +
  geom_point() +
  facet_wrap(vars(continent))

View(gapminder_data)

gapminder_outliers <- gapminder_data %>%
  filter(continent == 'Asia' & year == 1952)

View(gapminder_outliers)

##Outliers are China and India!!!

gapminder_no_outliers <- gapminder_data %>% 
  filter(country != 'China', country != 'India')

View(gapminder_no_outliers)

ggplot(gapminder_no_outliers) +
  aes(x = year, y = pop) +
  geom_point() +
  facet_wrap(vars(continent)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(gapminder_no_outliers) +
  aes(x = year, y = pop, group = country, color = country) +
  geom_line() +
  geom_label(aes(label = country))

##according to graph, US is the highest, followed by Indonesia!

gapminder_data %>% 
  filter(year == 1982) %>% 
  summarise(highest_lifeExp = max(lifeExp))
  
