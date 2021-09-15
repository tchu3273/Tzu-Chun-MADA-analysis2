## -------------------------------------------------------------------
##
## Script name: processingscript.R
##
## Purpose of script: to load the raw data, processes and cleans it 
## and saves it as Rds file in the processed_data folder
##
## Date Created: 2021-09-15
## Last Update: 2021-09-15
## -------------------------------------------------------------------


#load needed packages. make sure they are installed.
library(here) #to set paths
library(tidyverse) #for data wrangling and plotting 

#you can find the data source and information here: https://data.cdc.gov/NCHS/Conditions-Contributing-to-COVID-19-Deaths-by-Stat/hk9y-quqm

#first, clear the environment and load the raw data 
rm(list=ls())
data_location <- here::here("data","raw_data","Conditions_Contributing_to_COVID-19_Deaths__by_State_and_Age__Provisional_2020-2021.csv")

#load data and check the size of the dataframe
rawdata <- read.csv(data_location)
dim(rawdata) #298080 14

#dataframe contains 298080 observations and 14 rows, it's too big to print it out, so I will just view the dataframe in another tab
View(rawdata)

#take a look at the data
dplyr::glimpse(rawdata)


#######################
### data processing ###
#######################
#I have some data processing to do...
#Dates were loaded as character, and needed to be converted to date variables
#I am interested in looking into which health conditions are most related to COVID-19 Deaths and whether the results vary by age group
#or state. I will also include time variables to explore the trend over time. To keep the data clean, only the variables needed for 
#analysis will be kept.
newdata1 <- rawdata %>% 
	mutate(Data.As.Of = as.Date(Data.As.Of, format = "%m/%d/%Y"),
	       Start.Date = as.Date(Start.Date, format = "%m/%d/%Y"),
	       End.Date   = as.Date(End.Date, format = "%m/%d/%Y"))

#I want to explore only the recent data in 2021. I will also keep only monthly data, so we can drop the "Group" variable. Similarly, I
#will use state-level data, and drop some rows. 
newdata2 <- newdata1 %>% 
	#filter out only year of 2021, monthly, and state-level data 
	filter(Year == "2021", Group == "By Month", State != "United States") %>% 
	#keep variables needed for analysis
	#To keep the analysis simpler and results more generalizable, we will use a broader category of medical conditions
	select(Start.Date, End.Date, Year, Month, State, Condition.Group, Age.Group, COVID.19.Deaths) %>% 
	#change month column to character and factor it
	mutate(Month = (factor(as.character(Month), levels = seq(1,9,1), ordered = TRUE)))



#Next, I will clean up the Age.Group column to get rid of the rows which were not actual numeric range and factor the age group,
#so I will end up with eight groups: 0-24 years, 25-34 years, 35-44 years, 45-54 years, 55-64 years, 65-74 years, 75-84 years
# add 85+.

age_group <- c("0-24",
							 "25-34",
							 "35-44",
							 "45-54",
							 "55-64",
							 "65-74",
							 "75-84",
							 "85+")

newdata3 <- newdata2 %>% 
	filter(Age.Group != "All Ages", Age.Group != "Not stated") %>% 
	mutate(Age.Group = factor(Age.Group, levels = age_group, ordered = TRUE))
	
	
#Lastly, I will edit column names to make it easier to understand and work with. I also just prefer working with lowercase.
processeddata <- newdata3 %>% 
	rename("start.date"       = "Start.Date",
				 "end.date"         = "End.Date",
				 "year"             = "Year",
				 "month"            = "Month",
				 "state"            = "State",
				 "health.condition" = "Condition.Group",
				 "age.group"        = "Age.Group",
				 "covid.19.deaths"  = "COVID.19.Deaths")

#take another look at the dataset to make sure it was cleaned properly 
dplyr::glimpse(processeddata)
View(processeddata)

# save data as RDS
# I suggest you save your processed and cleaned data as RDS or RDA/Rdata files. 
# This preserves coding like factors, characters, numeric, etc. 
# If you save as CSV, that information would get lost.
# See here for some suggestions on how to store your processed data:
# http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata

# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(processeddata, file = save_data_location)


