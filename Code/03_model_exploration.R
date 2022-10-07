## model exploration

library(tidylog)
library(tidyverse)
library(sf)


## workflow
## join csci with temp data, match years
## models with tolerant/senstivie taxa
## csci scores
## functional traits
## component metrics

## LA county temp data

load("/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/Projects/San_Gabriel_Temp/Data/AirTemp/Modeling/baseline_stream_temp.RData")
str(baseline_stream_temp)

## ungroup due to old r version, make all columns numbers
baseline_stream_temp <- baseline_stream_temp %>% ungroup() %>%
  mutate(Max_Wkl_Max_StreamT_grt_30_ = as.numeric(Max_Wkl_Max_StreamT_grt_30_))

## get comids
TempSites <- unique(baseline_stream_temp$COMID)


## bugs - sites only

bugSites <- st_read("output_data/01_bio_sites_surrounding_counties.shp")
head(bugSites)

## bugs data

csciScores <- read.csv("/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/git/Cannabis_Eflows/ignore/csci_ca_scores.csv")
head(csciScores)


# Filter sites to LA temp region --------------------------------------------------

## how many temp in bug sites
sum(TempSites %in% bugSites$COMID) ## 476

## filter bug sites to temp sites
bugTempSites <- bugSites %>%
  filter(COMID %in% TempSites)

## how many sites with scores?
sum(csciScores$stationcode %in% bugTempSites$masterid) ## 1192

## format scores
csciScores <- csciScores %>%
  select(sampleid, stationcode, sampledate, sampleyear, fieldreplicate, csci) %>%
  rename(masterid = stationcode)

head(csciScores)

## filter bug data using masterid ### remove reps - remove 2nd rep for now, change later!!!!
csciScoresLA <- csciScores %>%
  filter(masterid %in% bugTempSites$masterid, fieldreplicate == 1 ) 
  
length(unique(csciScoresLA$masterid)) ## 707 sites in LA region with temp

## join comids
scoresTempSites <- full_join(csciScoresLA, bugTempSites, by = "masterid")
head(scoresTempSites)

# Join bug sites to temp data ---------------------------------------------

## join by comid and year

scoresTempSites <- scoresTempSites %>%
  rename(year = sampleyear)
head(scoresTempSites)

head(baseline_stream_temp)

AllData <- left_join(scoresTempSites, baseline_stream_temp, by = c("COMID", "year")) %>% drop_na()
head(AllData)
dim(AllData) ## 734

## save out
write.csv(AllData, "output_data/03_bugs_temp_joined_by_year.csv")


# Figures -----------------------------------------------------------------

## format for figures, make temp vars long, remove Max_Wkl_Max_StreamT_grt_30_

AllDataLong <- AllData %>%
  pivot_longer(Max_Wkly_Mean_StreamT:Max_Wkl_Max_StreamT_grt_30_, names_to = "Variable", values_to = "Value") %>%
  filter(!Variable == "Max_Wkl_Max_StreamT_grt_30_")

head(AllDataLong)

ggplot(subset(AllDataLong, Variable == "Max_Wkl_Max_StreamT") , aes(y=csci, x=Value, group = Variable, color = Variable)) +
  geom_smooth(method = "gam") +
  geom_vline(xintercept = 30, linetype="dashed", 
             color = "red", size=0.5) +
  geom_vline(xintercept = 26.667, linetype="dashed",
             color = "blue", size=0.5) +
  geom_hline(yintercept = 0.79) +
  facet_wrap(~Variable) +
  scale_x_continuous(name="Water Temp (°C)") 



### issues - 
## only one temp value per year, need seasonal
## can we get other metrics?
