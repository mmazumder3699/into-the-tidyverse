mean(2,6)
#doesn't give expected result bc 2,6 is not a single vector

mean(c(2,6))
#gives expected result bc c() combines arguments to create one vector

custom_mean <- function(this_vector) {sum(this_vector)/length(this_vector)}
#this creates the mean function!

#playing w custom function
custom_mean(c(2,6))
my_vector<- c(2,6)
custom_mean(my_vector)
custom_mean(this_vector=c(2,6))
custom_mean(this_vector=my_vector)

library(here)
library(readr)
library(dplyr)

#read in covid data
covid <- here ("data", "time_series_covid19_confirmed_US.csv") %>%
  read_csv

#take a look with dplyr
covid %>%
  slice(1:10)

#practice filtering
covid %>%
  filter(Province_State=="California") %>%
  select(fip=FIPS, county=Admin2, '9/18/20' :'9/24/20')
#province_state==california means that only variable matching california exactly will be picked (double equal sign)

#only interested in counties w at least 1000 cases on 9/24/20
covid %>%
  filter(Province_State=="California") %>%
  select(fip=FIPS, county=Admin2, '9/18/20' :'9/24/20') %>%
  filter('9/24/20'>=1000)

#look at data in just yolo county CA from 9/18 to 9/24
covid %>%
  filter(Province_State=="California") %>%
  filter(Admin2=="Yolo") %>%
  select(FIPS, Admin2, '9/18/20':'9/24/20')

#rename some of our variables here
covid %>%
  filter(Province_State=="California") %>%
  filter(Admin2=="Yolo") %>%
  select(fips=FIPS, county=Admin2, '9/18/20':'9/24/20')

#using select to get rid of a column
covid %>%
  filter(Province_State=="California") %>%
  filter(Admin2=="Yolo") %>%
  select(state=Province_State, fips=FIPS, county=Admin2, latest_cases='9/24/20') %>%
  select(-state)

#Practice mutating (transforming data, often to get to a normal distribution)
#Start off w original dataframe
covid %>%
  #keep only observations from CA
  filter(Province_State=="california") %>%
  #Only the first 20 counties using dplyr::slice()
  slice(1:20) %>%
  #Keep only a few columns of interest
  select(fips=FIPS, county=Admin2, latest_cases='9/24/20') %>%
  #Now apply a logarithmic transformtion using dplyr::mutate()
  mutate(latest_cases_log=log(latest_cases))
#overwrite latest cases with latest cases log
covid %>%
  filter(Province_State=="California") %>%
  slice(1:5) %>%
  select(fips=FIPS, county=Admin2, latest_cases= `9/24/20`) %>%
  mutate(latest_cases= log(latest_cases))

#mutate can create a new column
covid %>%
  filter(Province_State=="California") %>%
  slice(1:5) %>%
  select(fips=FIPS, county=Admin2, latest_cases= `9/24/20`) %>%
  mutate(state="California")

#use across function to mutate multiple columns (first argument specifies columns, second what to do w columns)
covid %>%
  select(state=Province_State, county=Admin2, `9/18/20`:`9/24/20`) %>%
  filter(state=="California") %>%
  slice(1:10) %>%
  mutate(across(.cols=`9/18/20`:`9/24/20`, .fns=~log(.x+1)))

#create a custom function (similar to lamda) to avoid log with infinite value
avoid_log_trap<- function(x) {log(x+1)}

covid %>%
  select(state=Province_State, county=Admin2, `9/18/20`:`9/24/20`) %>%
  filter(state=="California") %>%
  slice(1:10) %>%
  mutate(across(.cols=`9/18/20`:`9/24/20`, .fns=avoid_log_trap))

#Summarise

#Find total number of covid cases in one day
covid %>%
  filter(Province_State=="California") %>%
  select(fips=FIPS, county=Admin2, latest_cases=`9/24/20`) %>%
  summarise(total_cases=sum(latest_cases))

#Find daily sum of covid cases in california
covid %>%
  filter(Province_State=="California") %>%
  select(fips=FIPS, county=Admin2, `9/18/20`:`9/24/20`) %>%
  summarise(across(.cols=`9/18/20`:`9/24/20`, .fns=sum))

#Arrange
#reorder by number of covid cases
covid %>%
  filter(Province_State=="California") %>%
  slice(1:5) %>%
  select(fips=FIPS, county=Admin2, latest_cases=`9/24/20`) %>%
  arrange(latest_cases)

#by county name
covid %>%
  filter(Province_State=="California") %>%
  slice(1:5) %>%
  select(fips=FIPS, county=Admin2, latest_cases=`9/24/20`) %>%
  arrange(desc(county))

#Group_by (literally groups observations)

#summarise total covid cases at state level
covid %>%
  rename(state=Province_State, latest_cases= `9/24/20`) %>%
  group_by(state) %>%
  summarise(n_cases=sum(latest_cases)) %>%

#always ungroup!
ungroup() %>%
  arrange(desc(n_cases))

#Join

#read in second data set

urbanicity <- here("data", "NCHSURCodes2013.xlsx") %>%
  readxl::read_excel(na=c(".")) %>%
  janitor::clean_names() %>%
  select(fips_code, urbanicity=x2013_code, population=county_2012_pop)

urbanicity %>%
  slice(1:5)

#read in election data

elections <- here("github", "into-the-tidyverse", "data", "countypres_2000-2016.csv") %>%
  read_csv() %>%
  filter(year==2016) %>%
  filter(party %in% c("democrat", "republican"))
group_by(state, county, FIPS) %>%
  mutate(lean_republican=candidatevotes /first(candidatevotes)) %>%
  ungroup() %>%
  filter(party=="republican") %>%
  select(state, county, FIPS, lean_republican)

#start w covid data
covid %>%
  select(FIPS, county=Admin2, state=Province_State, latest_cases=`9/24/20`) %>%
  filter(state=="California") %>%
  slice(1:10) %>%
#Join w election data
  left_join(elections) %>%
#Join w population data
  left_join(urbanicity, by=c("FIPS"="fips_code"))

#start w covid data
covid %>%
  select(FIPS, county=Admin2, state=Province_State, latest_cases=`9/24/20`) %>%
  filter(state=="California") %>%
  slice(1:10) %>%
#Join w election data
  right_join(elections) %>%
#restrict to first 20 rows
  slice(1:20)

#Exercises

#read in incarceration
incarceration <- here("data", "incarceration_trends.csv") %>%
  read_csv()

#create ca_jail dataframe
ca_jail <- incarceration %>%
  filter(state=="CA")

#keep FIPS, total pop, jail pop
ca_jail1 <- ca_jail %>%
  select(fips, total_pop, total_jail_pop)

#create jail proportion
prop_jail <- ca_jail1 %>%
  mutate(prop_jail=total_jail_pop/total_pop)

#join with election data
elections_lean_republican %>%
rename(fips=FIPS) %>%
right_join(prop_jail) %>%
  arrange(desc(lean_republican))

elections_lean_republican %>%
  rename(fips=FIPS) %>%
  right_join(prop_jail) %>%
  arrange(lean_republican)
#make it a data frame for ease
election_prison <- more_trump1 %>%
  rename(fips=FIPS) %>%
  right_join(prop_jail)

#create new variable, more_trump
more_trump1 <- elections_lean_republican %>%
mutate(more_trump=if_else(lean_republican>1, "1", "0" ))

#summarize mean and SD of prop jail
election_prison %>%
  group_by(more_trump) %>%
  summarise(mean_prop_jail=mean(prop_jail,na.rm=TRUE))

election_prison %>%
  group_by(more_trump) %>%
  summarise(sd_jail_prop=sd(prop_jail, na.rm=TRUE))

#do the same thing at a national level

#create prop jail information at national level
prop_jail_us <- incarceration %>%
  select(fips, total_jail_pop, total_pop) %>%
  mutate(prop_jail_us=total_jail_pop/total_pop) 

#create election data with more trump info at national level
us_lean_rep <- elections %>%
  rename(fips=FIPS) %>%
  filter(year==2016) %>%
  filter(party %in% c("democrat", "republican")) %>%
group_by(state, county, fips) %>%
  mutate(lean_republican=candidatevotes /first(candidatevotes)) %>%
  ungroup() %>%
  filter(party=="republican") %>%
  select(state, county, fips, lean_republican) %>%
  mutate(more_trump=if_else(lean_republican>1, "1", "0" ))

#join datasets of election and incarceration at national level
us_trends <- us_lean_rep %>%
  right_join(prop_jail_us)

#summarize mean and SD of prop jail
us_trends %>%
  group_by(more_trump) %>%
  summarise(mean_jail=mean(prop_jail_us, na.rm=TRUE)) %>%
ungroup() %>%
  arrange(mean_jail)
