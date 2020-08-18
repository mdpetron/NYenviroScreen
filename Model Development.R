# NYenviroScreen Model 

# following the CalEPA CalEnviroScreen model
# reference https://oehha.ca.gov/calenviroscreen/scoring-model 

# data selection
names(ny_enviro_screen_data)

#remove bgs with no population 

ny_enviro_screen_data_wpop <- ny_enviro_screen_data %>% filter(ACSTOTPOP > 0)

df <- ny_enviro_screen_data_wpop %>%
  dplyr::select(######sociodemo######## 
         MINORPCT, #minority
         LOWINCPCT, #lowincome
         LESSHSPCT, #eductional attainment 
         LINGISOPCT, #linguistic isolation
         PercentUnemployed, #unemployment
         #housing burden/unemployment?
         
         #### Sensative Population #####
         UNDER5PCT,  #youth 
         OVER64PCT, #elderly
         `Age-adjusted heart attack hospitalization rate per 10,000 population - 2014`, #hearts
         `Asthma emergency department visit rate per 10,000 population - 2014`, #ashma
         `Percentage of premature deaths (before age 65 years) - 2016`, #deaths
         `Percentage of preterm birth - 2016`, # premies
         PercentDisability, #disabled
         StP_DifExpectedTotal, #cancer incidence 
         #food insecurity?
         
         #### Exposures  #####
         OZONE, #ozone
         PM25, #pm25
         RESP, #HAPS RESP
         PTRAF, # traffic prox/volumne
         DSLPM, #
         CANCER,
         DrinkWaterScore,
         #pesticides, Deisel, Drinking water
         
         #### Environmental Affect  #####
         PWDIS, #prox to water discharges
         PNPL, #prox to superfund sites
         PRMP, #prox to facilities with HAZ chems onsite
         PTSDF, #prox to haz waste disposal facilities 
         PRE1960PCT, #lead paint indicator 
         
         #### Physical vulnerability/characteristics  #####
         
         HVIScore, #heat vulnerability index
         LILATracts_1And10, #low income, low access census tract 
         Tribal_Area_or_Border, #tribal area or bordering one
         Urban_Area_50per, #50% or more within an urban cluster
         shr_hu_fp_any, #share of housing units in the 100 or 500 years Flood plain
         
         #### Evaluation Metrics ####
         ID,
         CensusTractID,
         amount_in_Urban,
         medincome,
         rentasPercageofIncome,
         per_poverty,
         population,
         white_alone,
         black_alone,
         Native_American,
         Asian_alone,
         other_alone,
         two_or_more,
         Hispanic,
         #income
         Households,
         AREALAND,
         inc_lessthan10k,
         inc_10to15,
         inc_15to20,
         inc_20to25,
         inc_25to30,
         inc_30to35,
         inc_35to40,
         inc_40to45,
         inc_45to50,
         inc_50to60,
         inc_60to75,
         inc_75to100,
         inc_100to125,
         inc_125to150,
         inc_150to200,
         inc_200plus)

#get state rank for the sociodemogrpahic indicators 
df <- df %>% 
  mutate(StP_MINORPCT = as.numeric(percent_rank(MINORPCT)),
         StP_LOWINCPCT = as.numeric(percent_rank(LOWINCPCT)),
         StP_LESSHSPCT = as.numeric(percent_rank(LESSHSPCT)),
         StP_PRE1960PCT = as.numeric(percent_rank(PRE1960PCT)),
         StP_LINGISOPCT = as.numeric(percent_rank(LINGISOPCT)),
         StP_UNDER5PCT = as.numeric(percent_rank(UNDER5PCT)),
         StP_OVER64PCT = as.numeric(percent_rank(OVER64PCT)),
         StP_RentBurden = as.numeric(percent_rank(rentasPercageofIncome)),
         StP_FoodAccess = as.numeric(percent_rank(LILATracts_1And10)),
         StP_PercentDisability = as.numeric(percent_rank(PercentDisability)),
         StP_PercentUnemployed = as.numeric(percent_rank(PercentUnemployed)),
         StP_heart_atack = as.numeric(percent_rank(`Age-adjusted heart attack hospitalization rate per 10,000 population - 2014`)),
         StP_asthma = as.numeric(percent_rank(`Asthma emergency department visit rate per 10,000 population - 2014`)),
         StP_premature_death = as.numeric(percent_rank(`Percentage of premature deaths (before age 65 years) - 2016`)),
         StP_preterm_birth = as.numeric(percent_rank(`Percentage of preterm birth - 2016`)),
         StP_OZONE = as.numeric(percent_rank(OZONE)),
         StP_PM25 = as.numeric(percent_rank(PM25)),
         StP_RESP = as.numeric(percent_rank(RESP)),
         StP_PTRAF = as.numeric(percent_rank(PTRAF)),
         StP_CANCER = as.numeric(percent_rank(CANCER)),
         StP_DSLPM = as.numeric(percent_rank(DSLPM)),
         StP_DrinkWaterScore = as.numeric(percent_rank(DrinkWaterScore)),
         StP_PWDIS = as.numeric(percent_rank(PWDIS)),
         StP_PNPL = as.numeric(percent_rank(PNPL)),
         StP_PRMP = as.numeric(percent_rank(PRMP)),
         StP_PTSDF = as.numeric(percent_rank(PTSDF)),
         StP_PRE1960PCT = as.numeric(percent_rank(PRE1960PCT)),
         StP_HVIScore = as.numeric(percent_rank(HVIScore)),
         StP_shr_hu_fp_any = as.numeric(percent_rank(shr_hu_fp_any)))
#look at it
str(df)
#Setup the default model groupings

df <- df %>%
  mutate(exposure = rowMeans(dplyr::select(., StP_PTRAF,
                                    StP_OZONE,
                                    StP_PRE1960PCT,
                                    StP_RESP,
                                    StP_CANCER,
                                    StP_PM25,
                                    StP_DSLPM,
                                    StP_DrinkWaterScore),
                             na.rm = T)*100,
         effects = rowMeans(dplyr::select(., StP_PWDIS,
                                          StP_PNPL,
                                          StP_PTSDF,
                                          StP_PRMP),
                             na.rm = T)*100,
         Health = rowMeans(dplyr::select(.,StP_heart_atack,
                                         StP_asthma,
                                         StP_DifExpectedTotal,
                                         StP_premature_death,
                                         StP_PercentDisability,
                                         StP_preterm_birth),
                           na.rm = T)*100,
         Sensitivepops = rowMeans(dplyr::select(., StP_UNDER5PCT,
                                                StP_OVER64PCT,
                                                StP_FoodAccess,
                                                StP_HVIScore,
                                                StP_shr_hu_fp_any), 
                            na.rm = T)*100,
         SocioEcon = rowMeans(dplyr::select(.,StP_LOWINCPCT,
                                     StP_LESSHSPCT,
                                     StP_LINGISOPCT,
                                     StP_RentBurden,
                                     StP_PercentUnemployed,
                                     StP_MINORPCT),
                                     na.rm = T)*100) %>% ungroup()

#this is a simmilar way that Cal Enviro Does it, but we add a thir catagory for population characteristics - health
df$PopChar <- ((df$SocioEcon + df$Sensitivepops + df$Health)/3)
df$PolBur  <- ((df$exposure + (df$effects/2))/1.5)
df$PolBurSc <- (df$PolBur/max(df$PolBur, na.rm = T))*10
df$PopCharSc <- (df$PopChar/max(df$PopChar, na.rm = T))*10

df$nyeScore <- df$PolBurSc*df$PopCharSc

df$StP_nyeScore <- round(as.numeric(percent_rank(df$nyeScore)*100),2)


#Use minority and income to create a 2108 version of the 2000 method
library(classInt)

##############
#Natural break for poverty percentage
################

#need to make sure we classify all of the rural areas

df <- df %>% mutate(Urban_Area_50per = ifelse(is.na(Urban_Area_50per), 0, Urban_Area_50per))

poverty_2018 <- df %>% dplyr::select(ID, per_poverty) %>% distinct()

p2kperblk <- poverty_2018$per_poverty
p2kperblk <- sort(p2kperblk, decreasing = TRUE)
p2kperblk <- p2kperblk[complete.cases(p2kperblk)]
jenks.ints.2k <- classIntervals(p2kperblk, 2, style = "jenks") #.214 
jenks.ints.2k$brks[2]

Urb_race <- df %>% filter(Urban_Area_50per == 1)
p2kperblk <- Urb_race$MINORPCT
p2kperblk <- sort(p2kperblk, decreasing = TRUE)
p2kperblk <- p2kperblk[complete.cases(p2kperblk)]
jenks.ints.race.urb <- classIntervals(p2kperblk, 2, style = "jenks") 
 jenks.ints.race.urb$brks #.524

rur_race <- df %>% filter(Urban_Area_50per == 0)
p2kperblk <- rur_race$MINORPCT
p2kperblk <- sort(p2kperblk, decreasing = TRUE)
p2kperblk <- p2kperblk[complete.cases(p2kperblk)]
jenks.ints.race.rur <- classIntervals(p2kperblk, 2, style = "jenks") 
jenks.ints.race.rur$brks #.237

#2018 Peja on Natural Breaks (Jenks)

df <- df %>%
  mutate(peja_18_breaks = ifelse(per_poverty > jenks.ints.2k$brks[2] |
                                   Urban_Area_50per == 1 &
                                   MINORPCT > jenks.ints.race.urb$brks[2] |
                                   Urban_Area_50per == 0 &
                                   MINORPCT > jenks.ints.race.rur$brks[2], 1, 0),
         peja_18_cal = ifelse(StP_nyeScore >= 75, 1, 0)) %>%
  mutate(peja_18_hybrid = ifelse(peja_18_cal == 1 | peja_18_breaks == 1, 1, 0))

#incorporate any Native American BG

df <- df %>% mutate(peja_18_hybrid = ifelse(!is.na(Tribal_Area_or_Border), 1, peja_18_hybrid))



tst <- df %>% dplyr::select(per_poverty, Urban_Area_50per,
                            MINORPCT, peja_18_breaks)


#### maps and figures 

#wrangle
# make a dataset to compare percent coverage by each eval metric
names(df)
#consolidate the income brackets 
df <- df %>% mutate(
  "Less than 10k-25k" = rowSums(dplyr::select(.,inc_lessthan10k,
                                               inc_10to15,
                                               inc_15to20,
                                               inc_20to25),
                       na.rm = T),
  "25k-45k" = rowSums(dplyr::select(.,inc_25to30,
                                              inc_30to35,
                                              inc_35to40,
                                              inc_40to45),
                                na.rm = T),
  "45k-100k" = rowSums(dplyr::select(.,inc_45to50,
                                    inc_50to60,
                                    inc_60to75,
                                    inc_75to100),
                      na.rm = T),
  "100k and more" = rowSums(dplyr::select(.,inc_100to125,
                                          inc_125to150,
                                          inc_150to200,
                                          inc_200plus),
                       na.rm = T)
  ) 
#same for 2000
df2k <- as.data.frame(ej2k)
df2k <- df2k %>% mutate(
  "Less than 10k-25k" = rowSums(dplyr::select(.,inc_lessthan10k,
                                              inc_10to15,
                                              inc_15to20,
                                              inc_20to25),
                                na.rm = T),
  "25k-45k" = rowSums(dplyr::select(.,inc_25to30,
                                    inc_30to35,
                                    inc_35to40,
                                    inc_40to45),
                      na.rm = T),
  "45k-100k" = rowSums(dplyr::select(.,inc_45to50,
                                     inc_50to60,
                                     inc_60to75,
                                     inc_75to100),
                       na.rm = T),
  "100k and more" = rowSums(dplyr::select(.,inc_100to125,
                                          inc_125to150,
                                          inc_150to200,
                                          inc_200plus),
                            na.rm = T)
) 

names(df)

dfs <- df %>% dplyr::select(37:46, 105:108) %>%
  summarise_all(sum) %>% gather("Income Bracket",
                                "Total NY Households/Population 2018")
dfsb <- df %>% filter(peja_18_cal == 1) %>% dplyr::select(37:46, 105:108) %>%
  summarise_all(sum) %>% gather("Income Bracket",
                                "NYeviroScreen - CalEPA Style Coverage")
dfsc <- df %>% filter(peja_18_hybrid == 1) %>% dplyr::select(37:46, 105:108) %>%
  summarise_all(sum) %>% gather("Income Bracket",
                                "NYenviroScreen - Hybrid Coverage")
dfsd <- df %>% filter(peja_18_breaks == 1) %>% dplyr::select(37:46, 105:108) %>%
  summarise_all(sum) %>% gather("Income Bracket",
                                "NYenviroScreen - Natural Breaks Coverage")
dfsd$`Income Bracket` 
# make a 2000 dataset to compare percent coverage by each eval metric

df2k$AREALAND <- df2k$AREA
dfsc1 <- df2k %>% mutate(population = total_pop) %>%
  dplyr::select(14:16, 33:38, 40:46) %>%
  summarise_all(sum, na.rm =T) %>% gather("Income Bracket",
                                          "Total Households or Population 2000")
dfsc2 <- df2k %>% filter(peja2k.1 == 1) %>% mutate(population = total_pop) %>%
  dplyr::select(14:16, 33:38, 40:46) %>%
  summarise_all(sum) %>% gather("Income Bracket",
                                "2000 PEJA Coverage")

#combine into one dataset
comp_plot_df <- left_join(dfs, dfsb)
comp_plot_df <- left_join(comp_plot_df, dfsc)
comp_plot_df <- left_join(comp_plot_df, dfsd)
comp_plot_df <- left_join(comp_plot_df, dfsc1)
comp_plot_df <- left_join(comp_plot_df, dfsc2)

#add in percentage of BGs covered 
                x1 <- "Block Groups"
                x2 <- nrow(df)
                x3 <- nrow(df[df$peja_18_cal == 1, ])
                x4 <- nrow(df[df$peja_18_hybrid == 1, ])
                x5 <- nrow(df[df$peja_18_breaks == 1, ])
                x6 <- nrow(df2k)
                x7 <- nrow(df2k[df2k$peja2k.1 == 1, ])

totbg_adder <- data.frame(x1,
                             x2,
                             x3,
                             x4,
                             x5,
                             x6,
                             x7)
names(totbg_adder) <- names(comp_plot_df)
comp_plot_df <- bind_rows(comp_plot_df, totbg_adder)
#add in percentage of RUR/URB bgs covered
x1 <- "Urban Block Groups"
x2 <- nrow(df[df$Urban_Area_50per == 1, ])
x3 <- nrow(df[df$peja_18_cal == 1 & df$Urban_Area_50per == 1, ])
x4 <- nrow(df[df$peja_18_hybrid == 1 & df$Urban_Area_50per == 1, ])
x5 <- nrow(df[df$peja_18_breaks == 1 & df$Urban_Area_50per == 1, ])
x6 <- nrow(df2k[df2k$Urban_Area_50per == 0, ])
x7 <- nrow(df2k[df2k$peja2k.1 == 1 & df2k$Urban_Area_50per == 0, ])

urbbg_adder <- data.frame(x1,
                          x2,
                          x3,
                          x4,
                          x5,
                          x6,
                          x7)
names(urbbg_adder) <- names(comp_plot_df)
comp_plot_df <- bind_rows(comp_plot_df, urbbg_adder)
#add in percentage of RUR/URB bgs covered
x1 <- "Rural Block Groups"
x2 <- nrow(df[df$Urban_Area_50per != 1, ])
x3 <- nrow(df[df$peja_18_cal == 1 & df$Urban_Area_50per != 1, ])
x4 <- nrow(df[df$peja_18_hybrid == 1 & df$Urban_Area_50per != 1, ])
x5 <- nrow(df[df$peja_18_breaks == 1 & df$Urban_Area_50per != 1, ])
x6 <- nrow(df2k[df2k$Urban_Area_50per != 0, ])
x7 <- nrow(df2k[df2k$peja2k.1 == 1 & df2k$Urban_Area_50per != 0, ])

rurbg_adder <- data.frame(x1,
                          x2,
                          x3,
                          x4,
                          x5,
                          x6,
                          x7)
names(rurbg_adder) <- names(comp_plot_df)
comp_plot_df <- bind_rows(comp_plot_df, rurbg_adder)




comp_plot_df$`Income Bracket` <- c("Population ",      "White ",    
                                  "Black ",     "Native American ",
                                  "Asian ",     "Other ",    
                                  "Two or more races ",     "Hispanic",       
                                  "Households ", "Land Area ",     "Less than 10k-25k ",
                                  "25k-45k ",            "45k-100k ",
                                  "Block Groups ",      "100k and more ",
                                  "Urban Block Groups ", "Rural Block Groups ")



########### major issue with 2000 urban rural here. must skip for now###########################################################################################


#merge with simplified shapefiles 
ejshp <- merge(nyblock_groups_simp, df, by.x = "GEOID", by.y = "ID", all.x = TRUE)

ejshp <- ejshp[ejshp$GEOID %in% df$ID, ]



# #ehhhh
# register_google("AIzaSyB-Hnhq04eitbDv697lEtRPbJKBoN106GQ")
# library(ggmap)
# 
# lon <- c(-74,-75)
# lat <- c(40.5,41)
# map <- get_map(location = c(lon = -74.1502, lat = 40.5795),
#                maptype = "roadmap", source = "google", zoom = 12)
# 
# p <- ggmap(map, extent = "normal", maprange = FALSE) +
#   geom_polygon(data = fortify(Rich),
#                aes(long, lat, group = group),
#                fill = "orange", colour = "red", alpha = 0.2)
# 
# print(p)

#sweet
write_rds(ejshp, "C:/Users/Mike Petroni/Documents/GitHub/NYenviroScreen/NYenviroScreen-app/www/ejshp_061520.rds")
write_rds(ej2k, "C:/Users/Mike Petroni/Documents/GitHub/NYenviroScreen/NYenviroScreen-app/www/peja2k_061520.rds")

ej2k <- readRDS("C:/Users/Mike Petroni/Documents/GitHub/NYenviroScreen/NYenviroScreen-app/www/peja2k_061520.rds")
ejshp <- readRDS("C:/Users/Mike Petroni/Documents/GitHub/NYenviroScreen/NYenviroScreen-app/www/ejshp_061520.rds")

  
df1 <- df %>% select(blockG, 194:226)

#reload old BlockG
blockG$nyeScore_SP <- df1$nyeScore_SP
blockG$nyeScore <- df1$nyeScore
#blockG$PopCharSc <- df1$PopCharSc
blockG$PolBurSc <- df1$PolBurSc
blockG$PolBur <- df1$PolBur

PEJA2017 <- blockG %>% filter(nyeScore_SP >= 7.5)

write_rds(PEJA2017, "C:/Users/Mike Petroni/Documents/TRI Local/PEJA2017_6_8_18.rds")###









