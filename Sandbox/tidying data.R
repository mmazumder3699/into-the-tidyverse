library(tidyr)
library(dplyr)

#use dataset in tidyr

billboard %>%
  slice(1:10)

#make data longer instead of wider

billboard %>%
  pivot_longer(cols=starts_with("wk"), names_to = "week", values_to= "ranking") %>%
drop_na() %>%
  group_by(track) %>%
  slice(1:5) %>%
  ungroup() %>%
  slice(1:30)

#we took ot na data, here's what that looks like if u dont

billboard %>%
  filter(artist=="2Ge+her") %>%
  pivot_longer(cols = starts_with("wk"), names_to ="week", values_to = "ranking")

#look at another built in dataset and make it wider

us_rent_income %>%
  pivot_wider(names_from ="variable", values_from = c("estimate", "moe"))

#now it's easy to manipulate!

us_rent_income %>%
  pivot_wider(names_from ="variable", values_from = c("estimate", "moe")) %>%
  select(locale =NAME, estimate_income, estimate_rent) %>%
  group_by(locale) %>%
  summarise(p_income_spent_on_rent =12*estimate_rent/estimate_income) %>%
  arrange(p_income_spent_on_rent)

#do some tidying with jae's data
#load libraries/data

library(readr)
library(here)

conformity <-here("Data", "JustCon5_TPP_Order1.csv") %>%
  read_csv() %>%
  select(sub_id =mTurkCode, starts_with("assault"), starts_with("theft")) %>%
  slice(-1) %>%
  type_convert()

#make this data longer

conformity %>%
  pivot_longer(cols= -sub_id, names_to= "condition", values_to= "rating")

#make conditions parts separate columns

conformity %>%
  pivot_longer(cols = -sub_id, names_to= "condition", values_to= "rating") %>%
  separate(col = condition, into = c("crime_type", "crime_severity", "n_endorsing_punishment", "repetition_number", "qualitrics_junk")) %>%
  select(-qualitrics_junk)

#we might also want to smoosh columns together!

elections <- here("Data", "countypres_2000-2016.csv") %>%
  read_csv() %>%
  select(year, county, state, candidate, party, candidatevotes, totalvotes)

#combine county and state column

elections %>%
  unite(col="location", county, state)

#do the same thing but name it nicer

elections %>%
  unite(col="location", county, state, sep=",")

#keeping variable names nice and tidy

banks <- here("Data", "BankBranchesData.txt") %>%
  read_tsv()

#this dataset shows column names w CamelCase, can make it tidyverse compliant

library(janitor)

banks %>%
  clean_names()

#here's a super screwy dataset!

candy <- here("Data", "candyhierarchy2017.csv") %>%
  read_csv()

#it wont even show up lol, need to clean names

candy %>%
  clean_names()

#make readable candy variable

readable_candy <- candy %>%
  clean_names()

#make each row one rating of one candy
did_it_work <- readable_candy %>%
  pivot_longer(cols= starts_with("q6"), names_to="candy", values_to = "rating")

#(it did work)((i am so smart))

#tidy up covid data and make it longer, janitor the names

covid <- here("Data", "time_series_covid19_confirmed_US.csv") %>%
  read_csv()

covid %>%
  pivot_longer(cols= contains("/"), names_to= "date", values_to= "cases") %>%
  clean_names()
