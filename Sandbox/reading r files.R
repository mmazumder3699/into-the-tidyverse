library(readr)
library(tibble)
library(here)
library(tictoc)

##Base R

here("Data", "PVD_2020_Property_Tax_Roll.csv") %>% 
  read.csv() %>%
  head()

base_read<- here("data","PVD_2020_Property_Tax_Roll.csv") %>%
  read.csv()

base_read %>%
  str()

#start timer
tic()

#read in the data using R's default function for reading CSVs
base_read<- here("data", "PVD_2020_Property_Tax_Roll.csv") %>%
  read.csv()

#stop timer
toc()

#Reading the tidyverse way!

#Start timer
tic()

#Read in the data using R's default function for reading CSVs
tidy_read <- here("data", "PVD_2020_Property_Tax_Roll.csv") %>%
  read_csv()

#stop timer
toc()

tidy_read %>%

#If u want to change variable type mayhaps
  tidy_read_mod <- here("data", "PVD_2020_Property_Tax_Roll.csv") %>%
  read_csv(col_types = cols(ZIP_POSTAL=col_character(), plat=col_character()))

#Exercises
#verifying that plat and zip are strings
tidy_read_mod <- here("data", "PVD_2020_Property_Tax_Roll.csv") %>%
  read_csv(col_types = cols(ZIP_POSTAL=col_character(), plat=col_character())) %>%
  str()
#Read covid-19 into R without downloading, assign covid_usa
covid_usa <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

#Read bank locations and name it bank_branches
bank_branches <- here("data","BankBranchesData.txt") %>%
  read_csv()