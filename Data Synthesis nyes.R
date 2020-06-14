##
# NY-enviroscreen 5/23/2020
## Data Synthesis 
##

setwd("C:/Users/Mike Petroni/Documents/GitHub/NYenviroScreen")

# R Libraries 

library(dplyr) 
library(readr)
library(readxl)
library(tigris)
library(tidycensus)
library(tidyr)
library(tidyverse)


#load API key
census_api_key("3b7f443116b03bdd7ce2f1ff3f2b117cfff19e69") 

##### Description
# NYES is a system like CalEnviroScreen, which combines indicators of population vulnerability 

##### Aquire data #############

##
# Flooding risk - tract level 
##
## Source: https://furmancenter.org/floodzonedata/map
## they get the data from FEMA and have already done the pre-prosessing 

# download and read into workspace
# download.file("https://furmancenter.org/files/floodzone_states/NY-FloodzoneData-Download.xlsx",
#               destfile = "data/NY-FloodzoneData-Download.xlsx", mode="wb")
flood_risk_ny <- read_excel("data/NY-FloodzoneData-Download.xlsx",
           sheet = 5)

##
# EPA EJScreen
##
## Source: https://www.epa.gov/ejscreen
# download and unzip - this is a large USA file 
# download.file("ftp://newftp.epa.gov/EJSCREEN/2019/EJSCREEN_2019_USPR.csv.zip",
#               destfile = "data/EJSCREEN_2019_USPR.csv.zip")
# unzip("data/EJSCREEN_2019_USPR.csv.zip", exdir="./data")
# grab EJscreen
ejScreen <- read_csv("data/EJSCREEN_2019_USPR.csv")
#names(ejScreen)
#subset to NY
ejScreenNY <- ejScreen %>% filter(ST_ABBREV == "NY")
rm(ejScreen)
##
# NYS Heat Vulnerability Index Data
##
## Source: https://www.health.ny.gov/environmental/weather/vulnerability_index/
# download.file("https://www.health.ny.gov/environmental/weather/vulnerability_index/docs/heat_vulnerability_index_data.zip",
#               destfile = "data/heat_vulnerability_index_data.zip")
# unzip("data/heat_vulnerability_index_data.zip", exdir="./data")
NY_heat_vul <- read_excel("data/NYS Heat Vulnerability Index Data_by Census Tract.xlsx")

##
# Heath indicators 
##
## Source: https://health.data.ny.gov/Health/Prevention-Agenda-2013-2018-Tracking-Indicators-Co/7j59-48xy
NY_health <- read_csv("https://health.data.ny.gov/api/views/7j59-48xy/rows.csv?accessType=DOWNLOAD")

#what indicators do we want?
NY_health_short <- NY_health %>% filter(Indicator %in% c(
  "Asthma emergency department visit rate per 10,000 population",  
  "Age-adjusted heart attack hospitalization rate per 10,000 population",
  "Percentage of preterm birth",
  "Percentage of premature deaths (before age 65 years)")) %>%
  group_by(`County Name`, Indicator) %>% 
  filter(`Data Years` == max(`Data Years`, na.rm = T)) %>% ungroup() %>% #take the most recent year
  mutate(Indicator = paste(Indicator, `Data Years`, sep = " - ")) %>%
  select(`County Name`, Indicator, `Percentage/Rate/Ratio`) %>% 
    group_by(`County Name`) %>%
  spread(Indicator, `Percentage/Rate/Ratio`) %>% ungroup()

  
#county fips 
#downlaod the fips coade for each county to use as a merging field 
acs2018_ny_county <- get_acs(geography = "county", 
                      state = "NY",
                      variables = c(population = "B01001_001"),
                      year = 2018)

acs2018_ny_county$`County Name` <- sapply(strsplit(acs2018_ny_county$NAME," County"), `[`, 1) 
#join in the fips with the county health data
countykey <- acs2018_ny_county %>% dplyr::select(`County Name`, GEOID)
NY_health_short <- left_join(NY_health_short, countykey)


#### cancer
# download.file("https://health.data.ny.gov/download/y4pv-ib8r/application%2Fzip",
#               destfile = "data/cancer11_15.zip", mode="wb")
# unzip("data/cancer11_15.zip", exdir = "./data")

# difference from expected total cancer incidence 2011-2015
can_inc <-  read_excel("data/NYSDOH_CancerMapping_Data_2011_2015.xlsx")
crosswalk <-  read_excel("data/NYSDOH_CancerMapping_Crosswalk_2011_2015.xlsx")
can_inc_sh <- can_inc %>%
  mutate(dif_from_expected_total = expected_Total - observed_Total) %>%
  mutate(StP_DifExpectedTotal = percent_rank(dif_from_expected_total)) %>%
    dplyr::select(dif_from_expected_total, StP_DifExpectedTotal, Dohregion)

#join with cross walk - we are assuming that bgs in the same dohregion have the same dif expected tot
can_inc_sh <- left_join(crosswalk, can_inc_sh, by = c("dohregion" = "Dohregion"))
can_inc_sh <- can_inc_sh %>% select(-dohregion) %>% distinct()
  

### https://studentwork.prattsi.org/infovis/visualization/food-desert-new-york-state-2010-census-demographics/

# USDA tract level food deserts 2015
# download.file("https://www.ers.usda.gov/webdocs/DataFiles/80591/DataDownload2015.xlsx?v=5315.6",
#                destfile = "data/DataDownload2015.xlsx", mode="wb")
Food_insecure <- read_excel("data/DataDownload2015.xlsx",
                            sheet = 3) 
Food_insecure_ny <- Food_insecure %>% filter(State == "New York") %>% 
  dplyr::select(CensusTract, LILATracts_1And10)

#this does not have all of our tracts 

# Drinking water
# https://www.health.ny.gov/statistics/environmental/public_health_tracking/about_pages/drinking_water/export
# see page 26 for method https://oehha.ca.gov/media/downloads/calenviroscreen/report/ces3dwmethodology.pdf
NY_drink <- read_csv("https://apps.health.ny.gov/statistics/environmental/public_health_tracking/tracker/files/water/drinking_water.csv")
#create a county average (blunt but all we can do right now)

NY_drink <- left_join(NY_drink, countykey, by = c("PRIN_CNTY" = "County Name"))

#take the most recent year... 2009? 
NY_drink_09 <- NY_drink %>% filter(YEAR == 2009)
unique(NY_drink_09$PRIN_CNTY)

#make the NA values 
NY_drink_09 <- NY_drink_09 %>%
  mutate(ASMAXCONC = ifelse(ASMAXCONC == -999, NA, ASMAXCONC),
         NITRATEMAXCONC  = ifelse(NITRATEMAXCONC  == -999, NA, NITRATEMAXCONC),
         TTHMMAXCONC = ifelse(TTHMMAXCONC == -999, NA, TTHMMAXCONC),
         HAA5MAXCONC = ifelse(HAA5MAXCONC == -999, NA, HAA5MAXCONC)) %>%
  mutate(ASMAXCONC_pop = ASMAXCONC*SYSTEM_POPULATION,
         NITRATEMAXCONC_pop  = NITRATEMAXCONC*SYSTEM_POPULATION,
         TTHMMAXCONC_pop = TTHMMAXCONC*SYSTEM_POPULATION,
         HAA5MAXCONC_pop = HAA5MAXCONC*SYSTEM_POPULATION)

#Population weighted averages
NY_drink_09 <- NY_drink_09 %>% group_by(PRIN_CNTY, GEOID) %>%
  summarise(county_pop = sum(SYSTEM_POPULATION),
            ASMAXCONC_w = sum(ASMAXCONC_pop, na.rm = T),
         NITRATEMAXCONC_w  = sum(NITRATEMAXCONC_pop, na.rm = T),
         TTHMMAXCONC_w = sum(TTHMMAXCONC_pop, na.rm = T),
         HAA5MAXCONC_w = sum(HAA5MAXCONC_pop, na.rm = T)) %>% ungroup()

NY_drink_09 <- NY_drink_09 %>% 
  transmute(GEOID = GEOID,
            ASMAXCONC_w = ASMAXCONC_w/county_pop,
            NITRATEMAXCONC_w  = NITRATEMAXCONC_w/county_pop,
            TTHMMAXCONC_w = TTHMMAXCONC_w/county_pop,
            HAA5MAXCONC_w = HAA5MAXCONC_w/county_pop)

#percentile each contaminant, then sum for score

NY_drink_09_P <- NY_drink_09 %>% 
  transmute(GEOID = GEOID,
            ASMAXCONC_P = percent_rank(ASMAXCONC_w)*100,
            NITRATEMAXCONC_P  = percent_rank(NITRATEMAXCONC_w)*100,
            TTHMMAXCONC_P = percent_rank(TTHMMAXCONC_w)*100,
            HAA5MAXCONC_P = percent_rank(HAA5MAXCONC_w)*100) %>%
  mutate(DrinkWaterScore = ASMAXCONC_P + NITRATEMAXCONC_P + TTHMMAXCONC_P + HAA5MAXCONC_P)

#pesticide application by county
# county level, need to scrape
#https://www.dec.ny.gov/docs/materials_minerals_pdf/prl2013.pdf

# Lots of county level environemtnal indictors from DOH
# https://www.health.ny.gov/environmental/public_health_tracking/

#additional indicators.... 
# air majors
# air minors
# hazardous waste sites
# hospital desert
# food desert 
# superfund sites
# public schools
# parks / recreational areas 
# tree canopy
# health/environmental advocacy groups
# Pathogenic and salutogenic infrastructure... 

#  proposed generating facilities 
# http://www3.dps.ny.gov/W/PSCWeb.nsf/All/763B187DD5A792DE8525847400667D6B?OpenDocument

##
# Census shapfiles
##
## Source: https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html 
nyblock_groups <- block_groups(state = "NY")
nyblock_groups_2000 <- block_groups(state = "NY", cb = T, year = 2000)

#grab the Urban Clusters and the Native American Blockgroups 
urb_zones <- urban_areas()
tribe_areas <- native_areas()

#convert to sf object
library(sf)


urb_zones_sf <- st_as_sf(urb_zones)
nyblock_groups_sf <- st_as_sf(nyblock_groups)
tribe_areas_sf <- st_as_sf(tribe_areas)
# ok lets do an intersection to get the percentage of urban area and native area for each ny bg
# Check the projection
# st_crs(urb_zones_sf)
# st_crs(nyblock_groups_sf)

int <- st_intersection(nyblock_groups_sf, urb_zones_sf)
int2 <- st_as_sf(int)
attArea <- int2 %>% 
  mutate(area = st_area(.) %>% as.numeric())
attArea$amount_in_Urban <- attArea$area/(as.numeric(attArea$ALAND) + as.numeric(attArea$AWATER))
#tribes interstion and area percentages
int_tribe <- st_intersection(nyblock_groups_sf, tribe_areas_sf)
int_tribe <- st_as_sf(int_tribe)

attArea_tribe <- int_tribe %>% 
  mutate(area = st_area(.) %>% as.numeric())
attArea_tribe$amount_in_Tribe <- attArea_tribe$area/(as.numeric(attArea_tribe$ALAND) + as.numeric(attArea_tribe$AWATER))

# make sure that the SF intersection process works 
# test <- attArea %>% filter(GEOID == 360039505001)
# testT <- tribe_areas_sf %>% filter(GEOID == "2535R")
# testBG <- nyblock_groups_sf %>% filter(GEOID == 360039505001)
# tstinter <- int_tribe %>% filter(GEOID == 360039505001)
#  library(leaflet)
# tst_BG_sp %>%
#   leaflet() %>%
#   addTiles() %>%
#   addPolygons(fill = "blue")

#now lets merge our indictors back into the dataset 

tribes <- as.data.frame(attArea_tribe)
tribes <- tribes %>% mutate(Tribal_Area = ifelse(amount_in_Tribe > 0, 1, 0),
                            Tribal_Area_or_Border = ifelse(!is.na(amount_in_Tribe), 1, 0)) %>%
  dplyr::select(GEOID, NAMELSAD.1, amount_in_Tribe, Tribal_Area, Tribal_Area_or_Border)

Urban <- as.data.frame(attArea)
Urban <- Urban %>% group_by(GEOID) %>%
  summarise(amount_in_Urban = sum(amount_in_Urban, na.rm = T)) %>% ungroup() %>%
  mutate(Urban_Area_50per = ifelse(amount_in_Urban > 0.5, 1, 0)) %>%
  dplyr::select(GEOID, amount_in_Urban, Urban_Area_50per)


#simplify boundaries for better app performance
library(rmapshaper)
# library(maptools)
# library(gpclib)
# library(lwgeom)
# library(sp)
# library(rgeos)
# library(raster)


nyblock_groups_simp <- ms_simplify(nyblock_groups)

##
# Census data from 2000 and 2018 for comparison with CP-29 PEJAs
##

# investigate the data you may need
v00 <- load_variables(2000, "sf3")
 v01 <- load_variables(2000, "sf1")
 v02 <- load_variables(2018, dataset = "acs5")
 
 rent <- v02 %>% filter(grepl("Income in the past 12 months below poverty level", label))
# mine <- v01 %>% filter(name == "P001001")
# 
# write.csv(v00, "census2000vars.csv")
# ?load_variables

#these will be used to create charts to quantify coverage,
#we want race breakout, income breakout

acs2018_ny <- get_acs(geography = "block group", 
        state = "NY",
        variables = c(medincome = "B19013_001",
                      rentasPercageofIncome = "B25071_001",
                      population = "B01003_001",
                      white_alone = "B02001_002",
                      black_alone = "B02001_003",
                      Native_American = "B02001_004",
                      Asian_alone = "B02001_005",
                      other_alone = "B02001_007",
                      two_or_more = "B02001_008",
                      Hispanic = "B03001_003",
                      #income
                      Households = "B19001_001",
                      inc_lessthan10k = "B19001_002",
                      inc_10to15 = "B19001_003",
                      inc_15to20 = "B19001_004",
                      inc_20to25 = "B19001_005",
                      inc_25to30 = "B19001_006",
                      inc_30to35 = "B19001_007",
                      inc_35to40 = "B19001_008",
                      inc_40to45 = "B19001_009",
                      inc_45to50 = "B19001_010",
                      inc_50to60 = "B19001_011",
                      inc_60to75 = "B19001_012",
                      inc_75to100 = "B19001_013",
                      inc_100to125 = "B19001_014",
                      inc_125to150 = "B19001_015",
                      inc_150to200 = "B19001_016",
                      inc_200plus = "B19001_017"),
        year = 2018) 

acs2018_ny1 <- acs2018_ny %>% dplyr::select(-moe) %>% spread(variable, estimate) %>% distinct()

#for income, need to use the tracts
acs2018_ny_tract <- get_acs(geography = "tract", 
                      state = "NY",
                      variables = c(population = "B01003_001",
                                    below_fed_poverty_level = "B17001_002"),
                      year = 2018) 

acs2018_ny_tract <- acs2018_ny_tract %>% dplyr::select(-moe) %>% spread(variable, estimate) %>% distinct()

#Census 2000
#https://api.census.gov/data/2000/sf3/variables.html
dc2000_ny <- get_decennial(geography = "block group", 
              state = "NY", year = 2000,
              variables = c(
                #White_total = "P012A001",
                white_alone = "P145A001",
                black_alone = "P145B001",
                Native_American = "P145C001",
                Asian_alone = "P145D001",
                other_alone = "P145F001",
                two_or_more = "P145G001",
                under_poverty = "P087002",
                total_pop = "P001001",
                Households = "P052001",
                inc_lessthan10k = "P052002",
                inc_10to15 = "P052003",
                inc_15to20 = "P052004",
                inc_20to25 = "P052005",
                inc_25to30 = "P052006",
                inc_30to35 = "P052007",
                inc_35to40 = "P052008",
                inc_40to45 = "P052009",
                inc_45to50 = "P052010",
                inc_50to60 = "P052011",
                inc_60to75 = "P052012",
                inc_75to100 = "P052013",
                inc_100to125 = "P052014",
                inc_125to150 = "P052015",
                inc_150to200 = "P052016",
                inc_200plus = "P052017")
              )
names(dc2000_ny)
dc2000_ny1 <- dc2000_ny %>% dplyr::select(-NAME) %>% group_by(GEOID) %>%
  spread(variable, value) %>% distinct() %>% ungroup()

# from the old script
################ 2000 ######################
#can we take this directly from the DEC?
#here is how this works, go to DEC website - https://www.dec.ny.gov/public/911.html
#download the kmz layer in google earth, open it in google earth, then save it as a kml
#get the layers
layers_peja <- st_layers("data/KML/Potential Environmental Justice Areas2.kml")
#read in each layer
read_kml <- function(layer1) {
  peja2k <- sf::st_read("data/KML/Potential Environmental Justice Areas2.kml",
                        layer = layers_peja$name[layer1]) 
  return(peja2k)
}

kmllist <- lapply(1:length(layers_peja$name), function (x)  read_kml(x))
kmllist1 <- kmllist[[1]]
for (i in 2:length(kmllist)) kmllist1 <- bind_rows(kmllist1, kmllist[[i]])


#merge with shapefiles
nyblock_groups_2000$GEOID <- paste0(nyblock_groups_2000$STATE, nyblock_groups_2000$COUNTY,
                                    nyblock_groups_2000$TRACT, nyblock_groups_2000$BLKGROUP)

nyblock_groups_2000$peja2k.1 <- ifelse(nyblock_groups_2000$GEOID %in% kmllist1$Name, 1, 0)

ej2k <- merge(nyblock_groups_2000, dc2000_ny1, by = "GEOID")
peja2k.2 <- subset(ej2k, ej2k$peja2k.1 == 1)

# did it work? - map it
# peja2k.2 %>%
#   leaflet() %>%
#   addTiles() %>%
#   addPolygons(fill = "blue")

## combine data sets 
##blocks

mydf <- ejScreenNY

#tractID
mydf$CensusTractID <- substr(mydf$ID,1,11)

#tracts 
head(NY_heat_vul$CensusTractID)
names(NY_heat_vul)
names(flood_risk_ny)
names(NY_health)

#heat
NY_heat_vul$CensusTractID <- as.character(NY_heat_vul$CensusTractID)
mydf2 <- left_join(mydf,NY_heat_vul)

#flood
head(flood_risk_ny$geo_id)
flood_risk_ny$CensusTractID <- flood_risk_ny$geo_id
mydf3 <- left_join(mydf2,flood_risk_ny)

#health
names(NY_health_short)
mydf3$GEOID <- substr(mydf$ID,1,5)
mydf4 <- left_join(mydf3,NY_health_short)

#drinking water contaiminants 
mydf5 <- left_join(mydf4,NY_drink_09_P)

#ACS evaluation variables 
acs2018_ny1$ID <- acs2018_ny1$GEOID
acs2018_ny1$GEOID <- NULL
mydf6 <- left_join(mydf5, acs2018_ny1)


#food access
names(Food_insecure_ny)
mydf7 <- left_join(mydf6, Food_insecure_ny, by = c("CensusTractID" = "CensusTract"))

#cancer incidence
names(can_inc_sh)
can_inc_sh$geoid10 <- as.character(can_inc_sh$geoid10)
mydf8 <- left_join(mydf7, can_inc_sh, by = c("ID" = "geoid10"))

#urban zones
mydf9 <- left_join(mydf8, Urban, by = c("ID" = "GEOID"))

#tribe areas
mydf10 <- left_join(mydf9, tribes, by = c("ID" = "GEOID"))

#ACS Poverty
acs2018_ny_tract <- acs2018_ny_tract %>% group_by(GEOID) %>%
summarise(per_poverty = below_fed_poverty_level/population) %>% ungroup()
mydf11 <- left_join(mydf10, acs2018_ny_tract, by = c("CensusTractID" = "GEOID"))


#holy shit 500 variables!!

ny_enviro_screen_data <- mydf11







