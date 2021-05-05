library(tidyverse)
library(lubridate)
library(janitor)
library(here)

#load in covid dataset, clean

covid <- here("Data", "time_series_covid19_confirmed_US.csv") %>%
  read_csv() %>%
  clean_names() %>%
  select(-c(uid:fips, country_region:combined_key)) %>%
  rename(county = admin2, state = province_state) %>%
  pivot_longer(cols = -c(county, state), names_to = "date", values_to = "cases") %>%
  mutate(date = str_remove(date, "x"),
         date = mdy(date)) %>%
  group_by(state, date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup()

covid %>%
  slice_head(n=10)

#make a very basic plot with date on x axis and cases on y
covid %>%
  ggplot(mapping = aes(x=date, y=cases))

#create line graph of california cases
covid %>%
  filter(state== "California") %>%
  ggplot(mapping = aes(x=date, y=cases)) +
  geom_line()

#create bar graph of california cases
covid %>%
  filter(state == "California") %>%
  ggplot(mapping =aes(x=date, y=cases)) +
  geom_bar(stat = "identity")

#here are a few other styles for reference (bivariate)
#area under cases
covid %>%
  filter(state == "California") %>%
  ggplot(mapping = aes(x=date, y=cases)) +
  geom_area()
#point for each date
covid %>%
  filter(state == "California") %>%
  ggplot(mapping = aes(x=date, y=cases)) +
  geom_point()
#weird boxplot thing i don't really get
covid %>%
  mutate(time = month(date, label = TRUE)) %>%
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_boxplot() +
  ggtitle("covid-19 cases in the USA over time")
#ok i don't really understand any of the ones moving forward
covid %>%
  mutate(time = month(date, label = TRUE)) %>%
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_violin(scale = "width") +
  ggtitle("covid-19 cases in the USA over time")
#another
covid %>%
  mutate(time = month(date, label = TRUE)) %>%
  filter(time > "Jun") %>%
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 5, binwidth = 1000) +
  ggtitle("covid-19 cases in the USA over time")
#another !!
covid %>%
  mutate(time = month(date, label = TRUE)) %>%
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_point() +
  ggtitle("covid-19 cases in the USA over time")


#univariate distribution of data
covid %>%
  filter(date == as.Date("2020-09-24")) %>%
  ggplot(mapping =aes(x=cases)) +
  geom_histogram() +
  ggtitle("Distribution of covid19 cases in the US on 9/24/2020")

#same thing but new geometry!
covid %>%
  filter(date == as.Date("2020-09-24")) %>%
  ggplot(mapping = aes(x=cases)) +
  geom_density() +
  ggtitle("Distribution of covid-19 cases in the USA on 09/24/2020")


