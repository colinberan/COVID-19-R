#Analysis of COVID-19 World Vaccination Progress (https://www.kaggle.com/gpreda/covid-world-vaccination-progress)
#Script by Colin Beran

library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(readr)

df = read.csv("E:/Users/Colin/Documents/Data Science/Data Sets/country_vaccinations.csv", stringsAsFactors=F)
head(df)

summary(df)

#Start by filtering data down to total vaccinations for each country.
#Also interested in "date" to check how recent this information is and "vaccines" to see what vaccines are being used.

df_total_vaccinations <- df %>%
  select(country, iso_code, date, total_vaccinations) %>%
  group_by(country) %>%
  filter(!is.na(total_vaccinations)) %>%
  filter(total_vaccinations == max(total_vaccinations))

head(df_total_vaccinations)

#We want the most recent date as well.
recent.date <- max(df_total_vaccinations$date)

#Plot of total vaccinations for top 20 most vaccinated countries as of most recent update.
df_total_vaccinations %>%
  arrange(desc(total_vaccinations)) %>%
  head(20) %>%
  ggplot(aes(total_vaccinations, reorder(country, total_vaccinations))) + theme_minimal() + 
  geom_bar(stat = 'identity', aes(fill=total_vaccinations)) + scale_x_continuous(labels = scales::comma) + 
  labs(title = paste('Total Vaccinations per Country as of', recent.date, sep = " "), y = '', x = 'Total Vaccinations') +
  theme(legend.position = 'none')

#We can see the US has vaccinated quite a lot of people, however it also has quite a few more people to vaccinate.
#Let's take a look at vaccinations per hundred people to see how the percentages look.

df_vacc_perc <- df %>%
  select(country, iso_code, date, people_vaccinated_per_hundred) %>%
  group_by(country) %>%
  filter(!is.na(people_vaccinated_per_hundred)) %>%
  filter(people_vaccinated_per_hundred == max(people_vaccinated_per_hundred)) %>%
  distinct(country, .keep_all = TRUE)

#Plot of total vaccinations per hundred people for top 20 most vaccinated countries as of most recent update.
df_vacc_perc %>%
  arrange(desc(people_vaccinated_per_hundred)) %>%
  head(20) %>%
  ggplot(aes(people_vaccinated_per_hundred, reorder(country, people_vaccinated_per_hundred))) + theme_minimal() + 
  geom_col(aes(fill=people_vaccinated_per_hundred)) + geom_label(aes(label = people_vaccinated_per_hundred)) +
  labs(title = paste('Total Vaccination Percentage per Country as of', recent.date, sep = " "), y = '', x = 'Percentage of Population') +
  theme(legend.position = 'none')

#Daily Vaccinations
df_vacc_daily <- df %>%
  select(country, daily_vaccinations) %>%
  group_by(country) %>%
  filter(!is.na(daily_vaccinations))

#Aggregate data by country name and average them.
df_vacc_daily <- aggregate(list(daily_vaccinations = df_vacc_daily$daily_vaccinations), list(country = df_vacc_daily$country), mean)

#Plot of average daily vaccinations for each country.
df_vacc_daily %>%
  arrange(desc(daily_vaccinations)) %>%
  head(20) %>%
  ggplot(aes(daily_vaccinations, reorder(country, daily_vaccinations))) + theme_minimal() + 
  geom_col(aes(fill=daily_vaccinations)) + geom_label(aes(label = round(daily_vaccinations))) +
  labs(title = paste('Average Daily Vaccinations per Country as of', recent.date, sep = " "), y = '', x = 'Daily Vaccinations') +
  theme(legend.position = 'none')
