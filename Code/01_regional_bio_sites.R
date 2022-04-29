### bioassessment sites

##workflow
## 1 - upload csci and asci
## subset to region of interest - RB4?
## plot
## explore data in region

library(tidylog)
library(tidyverse)
library(sf)
library(mapview)
library(nhdplusTools)

# CSCI --------------------------------------------------------------------

load("ignore/SMC_bmi_cali.csv")
head(bug_tax_ca)
class(bug_tax_ca)

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

## subset to surrounding counties - update this!!!
sort(unique(bug_tax_ca$county))
names(bugs2)

## define counties to select
counties <- c("Los Angeles", "Ventura", "Orange", "Riverside", "San Bernardino")

## filter to selelted counties
bug_sp_sub <- bugs2 %>%
  filter(county %in% counties) %>%
  select(masterid, latitude, longitude, county, COMID)

dim(bug_sp_sub)

length(unique(bug_sp_sub$county)) ## 6
length(unique(bug_sp_sub$COMID)) ## 923
length(unique(bug_sp_sub$masterid)) ## 14

### plot

## select only soatial columns

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList, fgb = FALSE)x



# this map of all sites in same HUC 12
m1 <- mapview(bug_sp_sub, cex=2, col.regions="orange",
              layer.name="Bugs Stations") 
  

m1
m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

mapview()

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
