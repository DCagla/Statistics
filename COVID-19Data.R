#This project focuses on using the R programming language to collect, 
#clean, analyze, and visualize COVID-19 data. It involves conducting time 
#series and demographic analyses on global COVID-19 case, recovery, and death 
#data, and presenting the findings through various charts.



#Install and load necessary packages
install.packages(c("tidyverse", "lubridate", "ggplot2", "plotly"))
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)

#Download COVID-19 data
covid_data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

#Preview the data
head(covid_data)

#Filter countries with valid data
valid_countries <- covid_data %>%
  group_by(location) %>%
  summarize(has_valid_cases = any(!is.na(total_cases)),
            has_valid_deaths = any(!is.na(total_deaths))) %>%
  filter(has_valid_cases & has_valid_deaths)

#Clean the data
covid_clean <- covid_data %>%
  inner_join(valid_countries, by = "location") %>%
  filter(!is.na(new_cases)) %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(location) %>% 
  summarize(total_cases = max(total_cases, na.rm = TRUE), 
            total_deaths = max(total_deaths, na.rm = TRUE),
            last_date = max(date, na.rm = TRUE))

#Identify top 10 countries by cases
top_countries <- covid_clean %>%
  arrange(desc(total_cases)) %>%
  slice_max(order_by = total_cases, n = 10)

#Visualize the data
ggplot(top_countries, aes(x=reorder(location, total_cases), y=total_cases)) +
  geom_bar(stat="identity", fill="blue") +
  theme_minimal() +
  labs(title="Top 10 Countries by Total COVID-19 Cases",
       x="Country", y="Total Cases") +
  coord_flip() 

#Display the chart
print(ggplot)
