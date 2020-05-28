# NYenviroScreen Model 

# following the CalEPA CalEnviroScreen model
# reference https://oehha.ca.gov/calenviroscreen/scoring-model 

# data selection
names(ny_enviro_screen_data)

df <- ny_enviro_screen_data %>%
  select(######sociodemo######## 
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
         
         
         
         PRE1960PCT, #housing)

#get state rank for the sociodemogrpahic indicators 
df <- df %>% mutate(SP_MINORPCT = percent_rank(MINORPCT),
                    SP_LOWINCPCT = percent_rank(LOWINCPCT),
                    SP_LESSHSPCT = percent_rank(LESSHSPCT),
                    SP_PRE1960PCT = percent_rank(PRE1960PCT),
                    SP_LINGISOPCT = percent_rank(LINGISOPCT),
                    SP_UNDER5PCT = percent_rank(UNDER5PCT),
                    SP_OVER64PCT = percent_rank(OVER64PCT))



  # combine multiple indicators, create percentiles 

TractsR1 <- TractsR %>% mutate(SP_ARTHRITIS_CrudePrev = percent_rank(ARTHRITIS_CrudePrev),
                               SP_CASTHMA_CrudePrev = percent_rank(CASTHMA_CrudePrev),
                               SP_BPHIGH_CrudePrev = percent_rank(BPHIGH_CrudePrev),
                               SP_CANCER_CrudePrev = percent_rank(CANCER_CrudePrev),
                               SP_DIABETES_CrudePrev = percent_rank(DIABETES_CrudePrev),
                               SP_KIDNEY_CrudePrev = percent_rank(KIDNEY_CrudePrev),
                               SP_STROKE_CrudePrev = percent_rank(STROKE_CrudePrev),
                               SP_TEETHLOST_CrudePrev = percent_rank(TEETHLOST_CrudePrev),
                               SP_PHLTH_CrudePrev = percent_rank(PHLTH_CrudePrev),
                               SP_MHLTH_CrudePrev = percent_rank(MHLTH_CrudePrev),
                               SP_CHD_CrudePrev = percent_rank(CHD_CrudePrev),
                               SP_HIGHCHOL_CrudePrev = percent_rank(HIGHCHOL_CrudePrev))

df <- left_join(blockG, TractsR1, by = c("GEOIDTR" = "tract"))

#get state rank for the sociodemogrpahic indicators 
df <- df %>% mutate(SP_MINORPCT = percent_rank(MINORPCT),
                    SP_LOWINCPCT = percent_rank(LOWINCPCT),
                    SP_LESSHSPCT = percent_rank(LESSHSPCT),
                    SP_PRE1960PCT = percent_rank(PRE1960PCT),
                    SP_LINGISOPCT = percent_rank(LINGISOPCT),
                    SP_UNDER5PCT = percent_rank(UNDER5PCT),
                    SP_OVER64PCT = percent_rank(OVER64PCT),
                    blockTC1 = blockTC.x/(ALAND10 + AWATER10),
                    SP_TC = percent_rank((blockTC1)),
                    SP_SCORE = percent_rank((blockScore.x)))

#get state rank for the health indicators 

df <- df %>% group_by(blockG) %>% mutate(exposure = mean(as.numeric(SP_PTRAF), as.numeric(SP_OZONE), as.numeric(SP_PRE1960PCT), 
                                                         as.numeric(SP_RESP), as.numeric(SP_CANCER), as.numeric(SP_PM25),
                                                         as.numeric(SP_DSLPM), SP_TC, na.rm = T)*10,
                                         
                                         effects = mean(as.numeric(SP_PWDIS), as.numeric(SP_PNPL), as.numeric(SP_PTSDF), as.numeric(SP_PRMP), na.rm = T)*10,
                                         
                                         Sensitivepops = mean(SP_UNDER5PCT, SP_OVER64PCT, SP_MINORPCT,SP_ARTHRITIS_CrudePrev,SP_CASTHMA_CrudePrev,
                                                              SP_BPHIGH_CrudePrev,SP_CANCER_CrudePrev,SP_DIABETES_CrudePrev,SP_KIDNEY_CrudePrev, SP_STROKE_CrudePrev,
                                                              SP_TEETHLOST_CrudePrev,SP_PHLTH_CrudePrev,SP_MHLTH_CrudePrev,SP_CHD_CrudePrev,SP_HIGHCHOL_CrudePre, na.rm = T)*10,
                                         
                                         SocioEcon = mean(SP_LOWINCPCT, SP_LESSHSPCT, SP_LINGISOPCT, na.rm = T)*10) 

df <- df %>% ungroup()

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
