library(tidyverse)

gapminder_data <- read_csv('data/gapminder_data.csv')

#summarise() to summarise our data frame

summarise(gapminder_data, averageLifeExp = mean(lifeExp))

gapminder_data_summarized <- gapminder_data %>% summarise(averageLifeExp = mean(lifeExp)) ##does the same as above
gapminder_data_summarized

gapminder_data %>% summarise(recent_year = max(year))

#filter() to subset rows in a data frame

gapminder_data %>% filter(year == 2007) %>% summarise(averageLifeExp = mean(lifeExp)) ##this will give you mean life expectancy in 2007 only!

#What is the average GDP per capita for the first year in the data set?
#So first find what is the first year, then find the average gdpPercap

gapminder_data %>% filter(year == min(year)) %>% summarise(averagegdpPercap = mean(gdpPercap)) #1952 = 3725

#group_by() to group values from a column

gapminder_data %>% 
  group_by(year) %>% 
  summarise(average_lifeExp = mean(lifeExp))

#calculate average life exp by continent

gapminder_data %>% 
  group_by(continent) %>% 
  summarise(average_lifeExp = mean(lifeExp))

gapminder_data %>% 
  group_by(continent) %>% 
  summarise(average_lifeExp = mean(lifeExp), min_lifeExp = min(lifeExp))

#mutate() to add or change a variable/column in a data frame

gapminder_data %>% 
  mutate(gdp = pop * gdpPercap)

#make a new column called popInMillions

gapminder_data %>% 
  mutate(popInMillions = pop/1000000)

#select() to subset columns in a data frame

gapminder_data %>% 
  select(pop, year)

gapminder_data %>% 
  select(-continent)

#create a data frame with only country, continent, year and lifeExp

gapminder_data %>% 
  select(-pop, -gdpPercap)

#transforming long format data frame into wide data format using pivot_wider(), to do the opposite you can use pivot_longer()

gapminder_data %>% 
  select(-pop, -gdpPercap) %>% 
  pivot_wider(names_from = year, values_from = lifeExp) ##this creates new table with columns by year for each country and the values shown are lifeExp!

#subsetting data frame to year 2007 and drop the year and continent columns

gapminder_data %>% 
  filter(year == 2007) %>% 
  select(-year, -continent)

#data that is only from 2005 and only from the Americas

gapminder_data %>% 
  filter(year == 2007) %>% 
  filter(continent == 'Americas') %>% 
  select(-year, -continent)

gapminder_data_2007 <- gapminder_data %>% 
  filter(year == 2007 & continent == 'Americas') %>% 
  select(-year, -continent)

#are CO2 emissions related to GDP?

co2_emissions_dirty <- read_csv('data/co2-un-data.csv', skip = 2,
         col_names = c('region', 'country', 'year', 'series', 'value', 'footnotes', 'source')) ##data frame is messy, so we can tell it to skip rows, edit column names, etc.

co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 'Emissions (thousand metric tons of carbon dioxide)' = 'total_emissions', 'Emissions per capita (metric tons of carbon dioxide)' = 'per_capita_emissions')) ##recode() is useful if you want to edit contents of column cells to, for example, shorten content!

co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 'Emissions (thousand metric tons of carbon dioxide)' = 'total_emissions', 'Emissions per capita (metric tons of carbon dioxide)' = 'per_capita_emissions')) %>% 
  pivot_wider(names_from = series, values_from = value) ##makes data frame into wide format!

co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 'Emissions (thousand metric tons of carbon dioxide)' = 'total_emissions', 'Emissions per capita (metric tons of carbon dioxide)' = 'per_capita_emissions')) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  count(year) ##looks at number of data per year, to identify which year has the most data and is closest to 2007!

co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 'Emissions (thousand metric tons of carbon dioxide)' = 'total_emissions', 'Emissions per capita (metric tons of carbon dioxide)' = 'per_capita_emissions')) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year ==2005) %>% 
  select(-year) ##this makes data frame for ONLY 2005 emissions, which is why we also dropped the Year column since they will all be 2005!

View(co2_emissions)
View(gapminder_data_2007)

#need to now merge gapminder 2007 subset with 2005 emissions subset, using the country column! need to do an inner join, which means only elements in common between data frames are kept!

inner_join(gapminder_data_2007, co2_emissions)
anti_join(gapminder_data_2007, co2_emissions) ##shows you which ones were excluded, useful to fix in case important data points were excluded!

co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 'Emissions (thousand metric tons of carbon dioxide)' = 'total_emissions', 'Emissions per capita (metric tons of carbon dioxide)' = 'per_capita_emissions')) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year ==2005) %>% 
  select(-year) %>% 
  mutate(country = recode(country, 'Bolivia (Plurin. State of)' = 'Bolivia', 'United States of America' = 'United States', 'Venezuela (Boliv. Rep. of)' = 'Venezuela'))

View(co2_emissions)

anti_join(gapminder_data_2007, co2_emissions) ##so now Puerto Rico is the only one excluded, because it is a US territory, so it did not have an entry in co2 emissions on its own!

gapminder_data_2007 <- gapminder_data %>% 
  filter(year == 2007 & continent == 'Americas') %>% 
  select(-year, -continent) %>% 
  mutate(country = recode(country, 'Puerto Rico' = 'United States'))

View(gapminder_data_2007) ##so now see United States twice, so need to merge!

gapminder_data_2007 <- gapminder_data %>% 
  filter(year == 2007 & continent == 'Americas') %>% 
  select(-year, -continent) %>% 
  mutate(country = recode(country, 'Puerto Rico' = 'United States')) %>% 
  group_by(country) %>% 
  summarise(lifeExp = sum(lifeExp * pop) / sum(pop), gdpPercap = sum(gdpPercap * pop) / sum(pop), pop = sum(pop))

View(gapminder_data_2007)
anti_join(gapminder_data_2007, co2_emissions) ##now no problems!!!
gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions)

#so going back to our question, are CO2 emissions related to GDP?

ggplot(gapminder_co2, aes(x = gdpPercap, y = per_capita_emissions)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = "GDP (per capita)", y = 'CO2 Emitted (per capita)')

write_csv(gapminder_co2, 'data/gapminder_co2.csv')
