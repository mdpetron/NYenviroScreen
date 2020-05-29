# NYenviroScreen Model 

# following the CalEPA CalEnviroScreen model
# reference https://oehha.ca.gov/calenviroscreen/scoring-model 

# data selection
names(ny_enviro_screen_data)

df <- ny_enviro_screen_data %>%
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
         
         #### Physical Vulnerability  #####
         HVIScore, #heat vulnerability index
         shr_hu_fp_any, #share of housing units in the 100 or 500 years Flood plain
         
         #### Evaluation Metrics ####
         ID,
         medincome,
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
  mutate(StP_MINORPCT = percent_rank(MINORPCT),
         StP_LOWINCPCT = percent_rank(LOWINCPCT),
         StP_LESSHSPCT = percent_rank(LESSHSPCT),
         StP_PRE1960PCT = percent_rank(PRE1960PCT),
         StP_LINGISOPCT = percent_rank(LINGISOPCT),
         StP_UNDER5PCT = percent_rank(UNDER5PCT),
         StP_OVER64PCT = percent_rank(OVER64PCT),
         StP_PercentDisability = percent_rank(PercentDisability),
         StP_PercentUnemployed = percent_rank(PercentUnemployed),
         StP_heart_atack = percent_rank(`Age-adjusted heart attack hospitalization rate per 10,000 population - 2014`),
         StP_asthma = percent_rank(`Asthma emergency department visit rate per 10,000 population - 2014`),
         StP_premature_death = percent_rank(`Percentage of premature deaths (before age 65 years) - 2016`),
         StP_preterm_birth = percent_rank(`Percentage of preterm birth - 2016`),
         StP_OZONE = percent_rank(OZONE),
         StP_PM25 = percent_rank(PM25),
         StP_RESP = percent_rank(RESP),
         StP_PTRAF = percent_rank(PTRAF),
         StP_CANCER = percent_rank(CANCER),
         StP_DrinkWaterScore = percent_rank(DrinkWaterScore),
         StP_PWDIS = percent_rank(PWDIS),
         StP_PNPL = percent_rank(PNPL),
         StP_PRMP = percent_rank(PRMP),
         StP_PTSDF = percent_rank(PTSDF),
         StP_PRE1960PCT = percent_rank(PRE1960PCT),
         StP_HVIScore = percent_rank(HVIScore),
         StP_shr_hu_fp_any = percent_rank(shr_hu_fp_any))


#Setup the default model groupings

df <- df %>% group_by(ID) %>% 
  mutate(exposure = mean(as.numeric(StP_PTRAF),
                         as.numeric(StP_OZONE),
                         as.numeric(StP_PRE1960PCT), 
                         as.numeric(StP_RESP),
                         as.numeric(StP_CANCER),
                         as.numeric(StP_PM25),
                         as.numeric(StP_DSLPM),
                         as.numeric(StP_DrinkWaterScore),
                         SP_TC, na.rm = T)*100,
         
         effects = mean(as.numeric(StP_PWDIS),
                        as.numeric(StP_PNPL),
                        as.numeric(StP_PTSDF),
                        as.numeric(StP_PRMP),
                        na.rm = T)*100,
         
         Sensitivepops = mean(as.numeric(StP_UNDER5PCT),
                              as.numeric(StP_OVER64PCT),
                              as.numeric(StP_heart_atack),
                              as.numeric(StP_asthma),
                              as.numeric(StP_premature_death),
                              as.numeric(StP_PercentDisability),
                              as.numeric(StP_preterm_birth),
                              as.numeric(StP_HVIScore),
                              as.numeric(StP_shr_hu_fp_any),
                              na.rm = T)*100,
         
         SocioEcon = mean(as.numeric(StP_LOWINCPCT),
                          as.numeric(StP_LESSHSPCT),
                          as.numeric(StP_LINGISOPCT),
                          as.numeric(StP_PercentUnemployed),
                          as.numeric(StP_MINORPCT),
                          na.rm = T)*100) %>% ungroup()



df$PopChar <- ((df$SocioEcon + df$Sensitivepops)/2)
df$PolBur  <- ((df$exposure + (df$effects/2))/1.5)
df$PolBurSc <- (df$PolBur/max(df$PolBur, na.rm = T))*10
df$PopCharSc <- (df$PopChar/max(df$PopChar, na.rm = T))*10

df$nyeScore <- df$PolBurSc*df$PopCharSc

df$nyeScore_SP <- percent_rank(df$nyeScore)*10

df1 <- df %>% select(blockG, 194:226)

#reload old BlockG
blockG$nyeScore_SP <- df1$nyeScore_SP
blockG$nyeScore <- df1$nyeScore
#blockG$PopCharSc <- df1$PopCharSc
blockG$PolBurSc <- df1$PolBurSc
blockG$PolBur <- df1$PolBur

PEJA2017 <- blockG %>% filter(nyeScore_SP >= 7.5)

write_rds(PEJA2017, "C:/Users/Mike Petroni/Documents/TRI Local/PEJA2017_6_8_18.rds")###
