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
names(ejScreen)
#subset to NY
ejScreenNY <- ejScreen %>% filter(ST_ABBREV == "NY")

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
  filter(`Data Years` == max(`Data Years`, na.rm = T)) %>% ungroup() %>%
  mutate(Indicator = paste(Indicator, `Data Years`, sep = " - ")) %>%
  select(`County Name`, Indicator, `Percentage/Rate/Ratio`) %>% 
    group_by(`County Name`) %>%
  spread(Indicator, `Percentage/Rate/Ratio`) %>% ungroup()
  
#county fips 

acs2018_ny_county <- get_acs(geography = "county", 
                      state = "NY",
                      variables = c(population = "B01001_001"),
                      year = 2018)

acs2018_ny_county$`County Name` <- sapply(strsplit(acs2018_ny_county$NAME," County"), `[`, 1) 

countykey <- acs2018_ny_county %>% dplyr::select(`County Name`, GEOID)
NY_health_short <- left_join(NY_health_short, countykey)

### https://studentwork.prattsi.org/infovis/visualization/food-desert-new-york-state-2010-census-demographics/

# USDA tract level food deserts 2015
# download.file("https://www.ers.usda.gov/webdocs/DataFiles/80591/DataDownload2015.xlsx?v=5315.6",
#                destfile = "data/DataDownload2015.xlsx", mode="wb")
Food_insecure <- read_excel("data/DataDownload2015.xlsx",
                            sheet = 3) 
Food_insecure_ny <- Food_insecure %>% filter(State == "New York")
#this does not have all of our tracts 

# Drinking water
# https://www.health.ny.gov/statistics/environmental/public_health_tracking/about_pages/drinking_water/export
# see page 26 for method https://oehha.ca.gov/media/downloads/calenviroscreen/report/ces3dwmethodology.pdf
NY_drink <- read_csv("https://apps.health.ny.gov/statistics/environmental/public_health_tracking/tracker/files/water/drinking_water.csv")
#create a county average (blunt but all we can do right now)

NY_drink <- left_join(NY_Drink, countykey, by = c("PRIN_CNTY" = "County Name"))

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
nyblock_groups <- block_groups(state = "NY", cb = T)
nyblock_groups_2000 <- block_groups(state = "NY", cb = T, year = 2000)

##
# Census data from 2000 and 2018 for comparison with CP-29 PEJAs
##
census_api_key("3b7f443116b03bdd7ce2f1ff3f2b117cfff19e69") 
# investigate the data you may need
# v00 <- load_variables(2000, "sf3")
# v01 <- load_variables(2000, "sf1")
# mine <- v01 %>% filter(name == "P001001")
# 
# write.csv(v00, "census2000vars.csv")
# ?load_variables

#these will be used to create charts to quantify coverage,
#we want race breakout, income breakout

acs2018_ny <- get_acs(geography = "block group", 
        state = "NY",
        variables = c(medincome = "B19013_001",
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

#these poverty variables do not work.... 

dc2000_ny <- get_decennial(geography = "block group", 
              state = "NY", year = 2000,
              variables = c(
                #White_total = "P012A001",
                white_alone_sf3 = "P145A001",
                under_poverty = "P087002",
                total_pop = "P001001"
              ))

# from the old script
################ 2000 ######################
# Poverty
pov2000 <- read.csv("data/DEC_00_SF3_P087_with_annincome.csv", stringsAsFactors = TRUE, header = TRUE)
medinc2000 <- read.csv("data/DEC_00_SF3_P053_with_ann_medain_income.csv", stringsAsFactors = TRUE, header = TRUE)
# Race
race2000 <- read.csv("data/redistricting race 2000.csv", stringsAsFactors = TRUE, header = TRUE)
race2000 <- race2000[complete.cases(race2000), ]
race2000$totpop <- ave(race2000$P0010001, race2000$GEOID,  FUN = function(x) sum(x, na.rm = T))
race2000$totwhite <- ave(race2000$P0020005, race2000$GEOID,  FUN = function(x) sum(x, na.rm = T))
my.cols <- c("GEOID", "totpop", "totwhite")
race2000 <- race2000[my.cols]
race2000 <- unique(race2000)
race2000$raceper2k <- ((race2000$totpop - race2000$totwhite)/race2000$totpop)
#Urb/rur
urb.rur2000 <- read.csv("data/2000urb_rur.csv", stringsAsFactors = TRUE, header = TRUE)
#2000 poverty percentage per block group
pov2000$povper2k <- pov2000$VD02/pov2000$VD01
#2000 urb/rural (MY!!!!) decsion rule for urban, over 50% urban 
urb.rur2000$perUrb2k <- (urb.rur2000$VD02/urb.rur2000$VD01)
urb.rur2000$urb2k <- ifelse(urb.rur2000$perUrb2k > .5, 1, 0)
urb.rur2000$GEO.id <- as.character(urb.rur2000$GEO.id)
urb.rur2000$geo.id3 <- sapply(strsplit(urb.rur2000$GEO.id, split = 'S', fixed = TRUE), function(x) (x[2]))
urb.rur2000$geo.id3 <- as.numeric(urb.rur2000$geo.id3)

eja <- merge(pov2000, urb.rur2000, by = "GEO.id")
ejb <- merge(eja, medinc2000, by = "GEO.id")
ejb$GEO.id <- as.character(ejb$GEO.id)
ejb$geo.id3 <- sapply(strsplit(ejb$GEO.id, split = 'S', fixed = TRUE), function(x) (x[2]))
ejb <- merge(ejb, race2000, by.x = "geo.id3", by.y = "GEOID")
#merge with shapefiles
nyblock_groups_2000$GEOID <- paste0(nyblock_groups_2000$STATE, nyblock_groups_2000$COUNTY,
                                    nyblock_groups_2000$TRACT, nyblock_groups_2000$BLKGROUP)

ej2k <- merge(nyblock_groups_2000, ejb, by.x = "GEOID", by.y = "geo.id3", all.x = TRUE)
ej2k$peja2k.1 <- ifelse(ej2k$urb2k == 0 & ej2k$raceper2k > .296 | ej2k$urb2k == 1 & ej2k$raceper2k > .466 | ej2k$povper2k >= .2310, 1, 0)
peja2k.2 <- subset(ej2k, ej2k$peja2k.1 == 1)

#did it work?

peja2k.2 %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(fill = "blue")

#sorta - need to see why we are getting more bgs than the DEC


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


#holy shit 500 variables!!

ny_enviro_screen_data <- mydf6







