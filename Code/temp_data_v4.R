# Load packages
library(tidyverse)
# library(dplyr)
library(readxl)
library(lubridate)
library(zoo)
library(tidyr)
library(stringr)
library(tidylog)




setwd("input_data/Temperature/temp")

temp.files <- list.files(pattern = ".xlsx") ## list all files
# View(temp.files) #name of all files folder
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

load( file =  "T1_temp_data_LAR.Rdata")

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
                              SiteID == 21150075 ~ "Compton2", ## 2 x compton creek - same site, logger was lost
                              SiteID == 21150077 ~ "Rattlesnake")) 
         


temp_names <- temp_names %>% distinct()
dim(temp_names)


# Metrics -----------------------------------------------------------------

## look in SMR repo
## frist caluclate daily means etc
## then do metrics

head(temp_names)

sum(is.na(temp_names))

df <- temp_names %>%
  group_by(SiteName, Year, Month, Day) %>%
  summarise(MeanTemp = mean(Temperature),
            MinTemp = min(Temperature),
            MaxTemp = max(Temperature))

head(df)

df <- df %>% 
  ungroup() %>%
  mutate(max_07da = zoo::rollmean(MaxTemp, k=7, fill = NA)) %>% ## rolling daily max
  mutate(mn_07da = zoo::rollmean(MeanTemp, k=7, fill = NA)) %>% ## rolling daily max
  mutate(DTR = MaxTemp - MinTemp) ## diurnal temp rate
  


# Figures -----------------------------------------------------------------
getwd()
## directory for figures
out.dir <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/SGR_Temp_Benthic/Figures/"


## map

## format date and make long

unique(df_date$SiteName)
unique(df_date$Metric)

df_date <- df %>%
  unite(col = "Date", c(Year, Month, Day), sep = "-") %>%
  mutate(Date = as.Date(Date)) %>%
  pivot_longer(MeanTemp:DTR, names_to = "Metric", values_to = "Temp") %>%
  mutate(Metric = factor(Metric, levels = c("max_07da", "mn_07da", "MeanTemp", "MinTemp", "MaxTemp", "DTR"))) %>%
  mutate(SiteName = factor(SiteName, levels = c("Benedict", "Burbank", "Compton", "Rattlesnake",
                                                "Riverfront", "Steelhead", "Compton2", "Willow")))
  
str(df_date)

## plot
T1 <- ggplot(df_date, aes(y = Temp, x = Date, group = Metric, color = Metric)) +
  geom_line() +
  facet_wrap(~SiteName, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_discrete(name = "Metric,", 
                       labels = c("7 Day Max", "7 Day Mean","Mean Daily Temp", "Min Daily Temp",
                                  "Max Daily Temp", "Diurnal Temp Rate")) 

T1

file.name1 <- paste0(out.dir, "Temp_by_Site.jpg")
ggsave(T1, filename=file.name1, dpi=300, height=5, width=6)


## metric labels
levels(df_date$Metric)
levels(df_date$Metric) <- c("7 Day Max", "7 Day Mean","Mean Daily Temp", "Min Daily Temp",
                            "Max Daily Temp", "Diurnal Temp Rate")

## plot
T2 <- ggplot(df_date, aes(y = Temp, x = Date, group = SiteName, color = SiteName)) +
  geom_line() +
  facet_wrap(~Metric, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  # scale_color_discrete(name = "Metric,", 
  #                      labels = c("7 Day Max", "7 Day Mean","Mean Daily Temp", "Min Daily Temp",
  #                                 "Max Daily Temp", "Diurnal Temp Rate")) 

T2

file.name1 <- paste0(out.dir, "Temp_by_Metric.jpg")
ggsave(T2, filename=file.name1, dpi=300, height=5, width=6)


# Map of sites ------------------------------------------------------------

setwd("input_data/Temperature")
getwd()

library(mapview)
library(sf)
library(leaflet)
library(leafem)

sites <- read.csv("Heal the Bay Temperature Logging Coordinates.csv")
sites <- sites %>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F)

sites
# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList, fgb = FALSE)

# leaflet(sites) %>%
#   addTiles() %>%
#   addLabelOnlyMarkers(label =  ~Site.Name, 
#                       labelOptions = labelOptions(noHide = T,
#                                                   direction = 'top',
#                                                   textOnly = T))

mapview(sites) %>%
  addStaticLabels(label = sites$Site.Name,
                  # noHide = TRUE,
                  direction = 'top',
                  # textOnly = TRUE,
                  textsize = "15px")

?addStaticLabels
# this map of all sites in same HUC 12
m1 <- mapview(sites, cex=6, col.regions="orange",
              layer.name="Temp Loggers") 
?mapview

m1
m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

