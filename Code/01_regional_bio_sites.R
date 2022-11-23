### bioassessment sites

##workflow
## 1 - upload csci and asci
## subset to region of interest - RB4?
## plot
## explore data in region
## format alternative response metrics
## - component metrics
## - traits and capture probability

library(tidylog)
library(tidyverse)
library(sf)
library(mapview)
library(nhdplusTools)
# install_github("SCCWRP/CSCI", force=T)
library(CSCI)     
library(lubridate)


# CSCI --------------------------------------------------------------------

load("ignore/SMC_bmi_cali.csv")
head(bug_tax_ca)
class(bug_tax_ca)

unique(bug_tax_ca$smcshed)

sgr_sites <- bug_tax_ca %>% filter(smcshed == "San Gabriel") 

head(sgr_sites)
length(unique(sgr_sites$masterid)) ## 113

## make spatial
# Create dataframe for looking up COMIDS (here use all stations)
bug_segs <- bug_tax_ca %>%
  dplyr::select(masterid, longitude, latitude, comid) %>%
  distinct(masterid, .keep_all = TRUE) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) %>%
  rename(COMID1 = comid)  %>%
  arrange(masterid)

# use nhdtools to get comids
bug_all_coms <- bug_segs %>%
  group_split(masterid) %>%
  set_names(., bug_segs$masterid) %>%
  map(~discover_nhdplus_id(.x$geometry))

bug_all_coms

# flatten into single dataframe instead of list
bug_segs_df <-bug_all_coms %>% flatten_dfc() %>% t() %>%
  as.data.frame() %>%
  rename("COMID"=V1) %>% rownames_to_column(var = "masterid")

bug_segs_df

bugs <- full_join(bug_segs, bug_segs_df, by = "masterid")
head(bugs)


## add county to filter
county <- bug_tax_ca %>%
  select(masterid, county) %>%
  distinct()

bugs2 <- left_join(bugs, county, by = "masterid")

class(bugs2)
dim(bugs2)

st_write(bugs2, "output_data/01_bio_sites_all.shp", append=F)

## subset to surrounding counties - update this!!!
sort(unique(bug_tax_ca$county))
names(bugs2)

## map of all sites in state
# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList, fgb = FALSE)



# this map of all sites in same HUC 12
m1 <- mapview(bugs2, cex=2, col.regions="orange",
              layer.name="Bugs Stations") 


m1
# m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

mapshot(m1, url = paste0(getwd(), "/output_data/01_bio_sites_all_counties_mapview.html"),
        file = paste0(getwd(), "/ignore/01_bio_sites_all_counties_mapview.png"))


## define counties to select
counties <- c("Los Angeles", "Ventura", "Orange", "Riverside", "San Bernardino")

## filter to selelted counties
bug_sp_sub <- bugs2 %>%
  filter(county %in% counties) %>%
  select(masterid, latitude, longitude, county, COMID)

dim(bug_sp_sub)

st_write(bug_sp_sub, "output_data/01_bio_sites_surrounding_counties.shp", append=F)

length(unique(bug_sp_sub$county)) ## 6
length(unique(bug_sp_sub$COMID)) ## 923
length(unique(bug_sp_sub$masterid)) ## 1310

### plot

## select only soatial columns

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList, fgb = FALSE)



# this map of all sites in same HUC 12
m1 <- mapview(bug_sp_sub, cex=2, col.regions="orange",
              layer.name="Bugs Stations") 
  

m1
# m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

mapshot(m1, url = paste0(getwd(), "/output_data/01_bio_sites_surrounding_counties_mapview.html"),
        file = paste0(getwd(), "/ignore/01_bio_sites_surrounding_counties_mapview.png"))
getwd()

mapview()


# CSCI component metrics --------------------------------------------------


# Capture probabilities ---------------------------------------------------

## capture probs
load(file="ignore/SMC_cap_prob_cali.csv")
head(oe_ca)
dim(oe_ca)
## bug data

bugs2 <- st_read("output_data/01_bio_sites_surrounding_counties.shp")
head(bugs2)
dim(bugs2)

## filter capture probability to only SGR region sites

capLAC <- oe_ca %>%
  # filter(masterid %in% bugs2$masterid) %>%
  rename(TAXON = otu) %>%
  drop_na(captureprob)
dim(capLAC)



# Traits ------------------------------------------------------------------

## uplaod traits EPA

traitsUSA <- read.csv("/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/git/SGR_Temp_Benthic/ignore/FreshwaterBioTraits_20100927.csv")
head(traitsUSA)
length(unique(traitsUSA$TAXON))
## filter to species in capture prob df
length(unique(capLAC$otu)) ## 335

TraitsCA <- traitsUSA %>%
  filter(TAXON %in% capLAC$TAXON )
length(unique(TraitsCA$TAXON)) ## 266

## get tolerance values to start with
names(TraitsCA)
unique(TraitsCA$TRAITS_NAME)

tols <- TraitsCA %>%
  filter(CATEGORY_NAME == "Tolerance") %>%
  dplyr::select(TAXON:TAXON_ORDER,STUDY_LOCATION_REGION, CATEGORY_NAME:VALUE_TEXT) %>%
  filter(TRAITS_NAME == "Thermal optima value") %>%
  group_by(TAXON) %>% 
  summarise(MeanValue = mean(VALUE_NUMBER)) ## quick fix, change later
  

length(unique(tols$TAXON)) ## 207 - find more!!!!

### join with capture probability

capTols <- inner_join(capLAC, tols, by = "TAXON") #%>%
  # dplyr::select(-c(sampleid)) %>% distinct()
names(capTols)

head(capTols)

sum(is.na(capTols$MeanValue))
sum(is.na(capTols$captureprob))

### weighted means per site - does capture probability change by date? no
## check the calculation, should the cap prob be summed?

# expTax <- capTols %>%
#   mutate(weightedValues = MeanValue*captureprob) %>%
#   group_by(masterid,latitude,longitude,county, huc) %>%
#   mutate(wgtValueEx = mean(weightedValues)/mean(captureprob)) %>%
#   ungroup() %>%
#   dplyr::select(masterid, wgtValueEx) %>%
#   distinct() 
  # summarise(wgtValueEx = weighted.mean(MeanValue,captureprob))

oe <- capTols %>% group_by(masterid,latitude,longitude,county, huc, sampleid) %>%
  summarise(wgtValueEx = weighted.mean(MeanValue, captureprob),
            wgtValueEx_obs = weighted.mean(MeanValue[meanobserved>0], captureprob[meanobserved>0])) %>%
  ungroup() %>%
  mutate(oeTemp = wgtValueEx_obs/wgtValueEx)

head(oe)

## temp alteration
## statewide


ggplot(oe, aes(x=longitude, y = latitude, color = oeTemp)) +
  geom_point() + 
  scale_color_viridis_c()


T1 <- ggplot(test, aes(y=oeTemp, x=Value, group = Variable, color = Variable)) +
  geom_smooth(method = "glm") +
  geom_vline(xintercept = 30, linetype="dashed", 
             color = "red", linewidth=0.5, show.legend = T) +
  geom_vline(xintercept = 26.667, linetype="dashed",
             color = "blue", linewidth=0.5, show.legend = T) +
  # geom_hline(yintercept = 0.79) +
  facet_wrap(~Variable, labeller =labeller(supp.labs),
             scales = "free_x") +
  scale_x_continuous(name="Water Temp (°C)") +
  scale_y_continuous(name = "Temp Preference (o/e)")

T1


## add 

head(expTax)
# Taxa occurrences and observed trait value--------------------------------------------------------

##  upload data - needs to be updated
Taxa <- read.csv("ignore/all_tax_data.csv")
head(Taxa)

## how many sites do we have in capture probability and in region
sum(unique(Taxa$stationcode) %in% unique(capTols$masterid)) ## 1184
names(Taxa)
## filter to regional sites ## get year by separating sampledate & remove replicates
?separate
taxaSub <- Taxa %>%
  filter(stationcode %in% capTols$masterid) %>%
  dplyr::select(stationcode, sampledate, replicate, finalid) %>%
  separate(sampledate, into= c("Date", "Time"), sep =c(" ")) %>%
  separate(Date, into = c("Month", "Day", "Year")) %>% dplyr::select(-Time, -Day) %>% ## get sample year
  filter(replicate == 1) %>% ## take first replicate, can change to random later
  rename(TAXON = finalid, masterid = stationcode)  %>% 
  inner_join(capTols, by = c("TAXON", "masterid")) ## join with trait and cap prob data

head(taxaSub)


## observed trait value

obsTax <- taxaSub %>%
  mutate(weightedValues = MeanValue*captureprob) %>%
  group_by(masterid,latitude,longitude,county, huc, Year) %>%
  mutate(wgtValueObs = mean(weightedValues)/mean(captureprob)) %>%
  ungroup() %>%
  dplyr::select(masterid, Year, wgtValueObs) %>%
  distinct() 
  

head(obsTax)
names(obsTax)


# Obs/Expected  -----------------------------------------------------------

### join obs and expected
## calculate o/e using (O-E)2/E - not sure about this calculation, check this!!!!

oe <- full_join(obsTax, expTax, by = "masterid") %>%
  ungroup() #%>%
  mutate(ObsExp = ((wgtValueObs-wgtValueEx)^2)/wgtValueEx)

head(oe)


# Format data for figures  -------------------------------------------------------------------

## upload la temp
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

## filter bug sites to temp sites
bugTempSites <- bugSites %>%
  filter(COMID %in% TempSites)

scoresTempSites <- full_join(oe, bugTempSites, by = "masterid") %>%
  rename(year = Year) %>% mutate(year = as.numeric(year))
head(scoresTempSites)

## join data
AllData <- left_join(scoresTempSites, baseline_stream_temp, by = c("COMID", "year")) %>% drop_na()
head(AllData)
dim(AllData) ## 734


# Figures -----------------------------------------------------------------


## format for figures, make temp vars long, remove Max_Wkl_Max_StreamT_grt_30_

AllDataLong <- AllData %>%
  pivot_longer(Max_Wkly_Mean_StreamT:Max_Wkl_Max_StreamT_grt_30_, names_to = "Variable", values_to = "Value") %>%
  filter(!Variable == "Max_Wkl_Max_StreamT_grt_30_") 

unique(AllDataLong$Variable)
supp.labs
supp.labs <- unique(AllDataLong$Variable)
names(supp.labs) <- c("Max Weekly Mean","Max Weekly Max", "Max Weekly Min", "Max Weekly Range", "Av Weekly Range")

head(AllDataLong)
# ?geom_vline

T1 <- ggplot(AllDataLong, aes(y=ObsExp, x=Value, group = Variable, color = Variable)) +
  geom_smooth(method = "glm") +
  geom_vline(xintercept = 30, linetype="dashed", 
             color = "red", linewidth=0.5, show.legend = T) +
  geom_vline(xintercept = 26.667, linetype="dashed",
             color = "blue", linewidth=0.5, show.legend = T) +
  # geom_hline(yintercept = 0.79) +
  facet_wrap(~Variable, labeller =labeller(supp.labs),
             scales = "free_x") +
  scale_x_continuous(name="Water Temp (°C)") +
  scale_y_continuous(name = "Temp Preference (o/e)")

T1

file.name1 <- paste0(out.dir, "03_temp_pref_temp_response_GAMs.jpg")
ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)




# check San Juan sites ----------------------------------------------------

soc_sites <- read.csv("input_data/CSCI_ASCI_SitesAll_for_SOC_FlowEcologyStudy.csv")

head(soc_sites)
dim(soc_sites)

soc_sp <- soc_sites %>%
  select(BugID, Lat, Long) %>%
  st_as_sf(coords=c("Long", "Lat"), crs=4326, remove=F)

soc_sp

# this map of all sites in same HUC 12
m1 <- mapview(soc_sp, cex=2, col.regions="green",
              layer.name="Bioassessment Stations") 


m1
m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")
