# Load packages

library(tidyverse)
library(purrr)
library(readxl)
library(lubridate)
library(readxl)
library(zoo)
library(dplyr)
library(tidyr)
library(stringr)
library(tidylog)
library(readxl)



setwd("input_data/Temperature/temp")

temp.files <- list.files(pattern = ".xlsx") ## list all files
View(temp.files) #name of all files folder
temp.files[c]

temp_datax <- NULL ## create empty dataframe to cumulate into

c
c=11
for(c in 1:length(temp.files)) { ## will loop through list of files
  
  temp_data <- read_excel(paste(temp.files[c])) ##upload each file one by one
  # head(temp_data)
  names(temp_data)[2:3] <- c("DateTime", "Temperature")
  # names(temp_data)
  
  ## format date and add filename
  temp_data <- temp_data %>%
    separate(DateTime, into  = c('Date', 'Time'), sep = ' ', remove = F) %>% # Split Date-Time (PST) column into date and time
    separate(Date, into = c("Year", "Month", "Day"), sep = '-', remove = T)# %>% ## split the date column into date, year, month, day
    # rename(Temperature = c(`Ch: 1 - Temperature   (°C)` | `Ch: 1 - Temperature   (Â°C)`)) %>%
    

  temp_data <- temp_data[1:7] ## remove columns from weird dfs
  
  temp_data <- temp_data %>%
    mutate(FileName = temp.files[c]) ## add filename

  temp_datax <- bind_rows(temp_datax, temp_data)  ## combine all - cumulative dataframe

  # View(temp_data)

  
}

save(temp_datax, file =  "T1_temp_data_LAR.Rdata")

head(temp_datax)



# Replicates ------------------------------------------------------------

dim(temp_datax)

## check replicates
temp_dat_red <- temp_datax %>%
  select(-FileName) %>%
  distinct()

### some data replicated in the different files - change filename and remove duplicates

unique(temp_datax$FileName)

str_split(unique(temp_datax$FileName), pattern = " ")

temp_names <- temp_datax %>%
  separate(FileName, into = c("SiteID", "Date_Name"), sep = c(" ", "_")) %>%
  separate(SiteID, into = c("SiteID", "test"), sep = c("_", " ")) %>%
  select(-test, -Date_Name) %>%
  mutate(SiteName = case_when(SiteID == 21150073 ~ "Benedict", 
                              SiteID == 21150064 ~ "Willow",
                              SiteID == 21150065 ~ "Riverfront", 
                              SiteID == 21150068 ~ "Burbank",
                              SiteID == 21150070 ~ "Compton", 
                              SiteID == 21150074 ~ "Steelhead",
                              SiteID == 21150075 ~ "Compton2", ## 2 x compton creek but not sure which is which
                              SiteID == 21150077 ~ "Rattlesnake")) 
         


temp_names <- temp_names %>% distinct()
dim(temp_names)


# Metrics -----------------------------------------------------------------

## look in AMR repo
## frist caluclate daily means etc
## then do metrics

df <- df %>% 
  select(-MinT, -MedT) %>% 
  mutate(max_07da = zoo::rollmean(MaxT, k=7, fill = NA)) %>% 
  mutate(mn_07da = zoo::rollmean(MeanT, k=7, fill = NA))

