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

#Natural break fro poverty percentage

#need to make sure we classify all of the rural areas

df <- df %>% mutate(Urban_Area_50per = ifelse(is.na(Urban_Area_50per), 0, Urban_Area_50per))

poverty_2018 <- df %>% dplyr::select(ID, per_poverty) %>% distinct()

p2kperblk <- poverty_2018$per_poverty
p2kperblk <- sort(p2kperblk, decreasing = TRUE)
p2kperblk <- p2kperblk[complete.cases(p2kperblk)]
jenks.ints.2k <- classIntervals(p2kperblk, 2, style = "jenks") #.213 
jenks.ints.2k$brks[2]

Urb_race <- df %>% filter(Urban_Area_50per == 1)
p2kperblk <- Urb_race$MINORPCT
p2kperblk <- sort(p2kperblk, decreasing = TRUE)
p2kperblk <- p2kperblk[complete.cases(p2kperblk)]
jenks.ints.race.urb <- classIntervals(p2kperblk, 2, style = "jenks") 
jenks.ints.race.urb$brks #.530 

rur_race <- df %>% filter(Urban_Area_50per == 0)
p2kperblk <- rur_race$MINORPCT
p2kperblk <- sort(p2kperblk, decreasing = TRUE)
p2kperblk <- p2kperblk[complete.cases(p2kperblk)]
jenks.ints.race.rur <- classIntervals(p2kperblk, 2, style = "jenks") 
jenks.ints.race.rur$brks #.242

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


dfs <- df %>% dplyr::select(36:44, 103:106) %>%
  summarise_all(sum) %>% gather("Income Bracket",
                                "Total NY Households/Population 2018")
dfsb <- df %>% filter(peja_18_cal == 1) %>% dplyr::select(36:44, 103:106) %>%
  summarise_all(sum) %>% gather("Income Bracket",
                                "NYeviroScreen - CalEPA Style Coverage")
dfsc <- df %>% filter(peja_18_hybrid == 1) %>% dplyr::select(36:44, 103:106) %>%
  summarise_all(sum) %>% gather("Income Bracket",
                                "NYenviroScreen - Hybrid Coverage")
dfsd <- df %>% filter(peja_18_breaks == 1) %>% dplyr::select(36:44, 103:106) %>%
  summarise_all(sum) %>% gather("Income Bracket",
                                "NYenviroScreen - Natural Breaks Coverage")
 
# make a 2000 dataset to compare percent coverage by each eval metric

names(df2k)
dfsc1 <- df2k %>% mutate(population = totpop) %>%
  dplyr::select(51:53, 70, 71, 73:80) %>%
  summarise_all(sum, na.rm =T) %>% gather("Income Bracket",
                                          "Total Households or Population 2000")
dfsc2 <- df2k %>% filter(peja2k.1 == 1) %>% mutate(population = totpop) %>%
  dplyr::select(51:53, 70, 71, 73:80) %>%
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
x6 <- nrow(df2k[df2k$urb2k == 1, ])
x7 <- nrow(df2k[df2k$peja2k.1 == 1 & df2k$urb2k == 1, ])

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
x6 <- nrow(df2k[df2k$urb2k != 1, ])
x7 <- nrow(df2k[df2k$peja2k.1 == 1 & df2k$urb2k != 1, ])

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
                                  "Households ",      "Less than 10k-25k ",
                                  "25k-45k ",            "45k-100k ",
                                  "Block Groups ",      "100k and more ",
                                  "Urban Block Groups ", "Rural Block Groups ")



#now we can plot them, maybe
library(plotly)
plot_ly(comp_plot_df, type = "bar",
        x = ~`Income Bracket`,
        y = ~(`NYeviroScreen - CalEPA Style Coverage`/`Total NY Households/Population 2018`)*100,
        name = "NYeviroScreen - CalEPA Style Coverage") %>%
  add_trace(y = ~(`2000 PEJA Coverage`/`Total Households or Population 2000`)*100,
            name = "2000 PEJA Coverage") %>%
  add_trace(y = ~(`NYenviroScreen - Natural Breaks Coverage`/`Total NY Households/Population 2018`)*100,
            name = "NYenviroScreen - Natural Breaks Coverage") %>%
  add_trace(y = ~(`NYenviroScreen - Hybrid Coverage`/`Total NY Households/Population 2018`)*100,
            name = "NYenviroScreen - Hybrid Coverage") %>%
  layout(xaxis = list(title = 'Percent of House Holds or Population'), barmode = 'group',
         yaxis = list(title = "",
                      categoryorder = "array",
                      categoryarray = ~`Income Bracket`))

#time change 2000 vs 2018
comp_plot_df_time_gen <- comp_plot_df %>%
  filter(`Income Bracket` %in% c("Population ",
                                 "Urban Block Groups ",
                                 "Rural Block Groups "))

Noax <- list(
  title = "",
  zeroline = T,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

fig1 <- plot_ly(comp_plot_df_time_gen,
        type = "bar",
        orientation = 'h', marker = list(color = '404788FF'),
        y = ~`Income Bracket`,
        x = ~(`2000 PEJA Coverage`/`Total Households or Population 2000`)*100,
            name = "2003 PEJA Coverage", 
        text = ~paste0(round(`2000 PEJA Coverage`/`Total Households or Population 2000`,2)*100, "%"),
        textposition = "auto") %>%
  add_trace(marker = list(color = '238A8DFF'),
            x = ~round(`NYenviroScreen - Natural Breaks Coverage`/`Total NY Households/Population 2018`, 2)*100,
            name = "2020 - Natural Breaks",
            text = ~paste0(round(`NYenviroScreen - Natural Breaks Coverage`/`Total NY Households/Population 2018`, 2)*100, "%"),
            textposition = "auto") %>%
  add_trace(marker = list(color = '55C667FF'),
            x = ~round(`NYenviroScreen - Hybrid Coverage`/`Total NY Households/Population 2018`, 2)*100,
            name = "2020 - Hybrid Model",
            text = ~paste0(round(`NYenviroScreen - Hybrid Coverage`/`Total NY Households/Population 2018`, 2)*100, "%"),
            textposition = "auto") %>%
  add_trace(marker = list(color = 'FDE735FF'),
            x = ~round(`NYeviroScreen - CalEPA Style Coverage`/`Total NY Households/Population 2018`, 2)*100,
            name = "2020 - CalEPA Model",
            text = ~paste0(round(`NYeviroScreen - CalEPA Style Coverage`/`Total NY Households/Population 2018`, 2)*100, "%"),
            textposition = "auto") %>%
  layout(title = "Comparing PEJA Designation Methods: Urban vs Rural <br>",
         yaxis = list(title = '',
                      categoryorder = "array",
                      categoryarray = ~`Income Bracket`),
         barmode = 'group',
         orientation = 'h',
         xaxis = Noax) %>% layout(legend = list(orientation = 'h'))

fig1

#Race Change 
comp_plot_df_race <- comp_plot_df %>%
  filter(`Income Bracket` %in% c( "White ",    
                                  "Black ",     "Native American ",
                                  "Asian ",     "Other ",    
                                  "Two or more races "))

fig2 <- plot_ly(comp_plot_df_race,
                type = "bar",
                orientation = 'h', marker = list(color = '404788FF'),
                y = ~`Income Bracket`,
                x = ~(`2000 PEJA Coverage`/`Total Households or Population 2000`)*100,
                name = "2003 PEJA Coverage", 
                text = ~paste0(round(`2000 PEJA Coverage`/`Total Households or Population 2000`,2)*100, "%"),
                textposition = "auto") %>%
  add_trace(marker = list(color = '238A8DFF'),
            x = ~round(`NYenviroScreen - Natural Breaks Coverage`/`Total NY Households/Population 2018`, 2)*100,
            name = "2020 - Natural Breaks",
            text = ~paste0(round(`NYenviroScreen - Natural Breaks Coverage`/`Total NY Households/Population 2018`, 2)*100, "%"),
            textposition = "auto") %>%
  add_trace(marker = list(color = '55C667FF'),
            x = ~round(`NYenviroScreen - Hybrid Coverage`/`Total NY Households/Population 2018`, 2)*100,
            name = "2020 - Hybrid Model",
            text = ~paste0(round(`NYenviroScreen - Hybrid Coverage`/`Total NY Households/Population 2018`, 2)*100, "%"),
            textposition = "auto") %>%
  add_trace(marker = list(color = 'FDE735FF'),
            x = ~round(`NYeviroScreen - CalEPA Style Coverage`/`Total NY Households/Population 2018`, 2)*100,
            name = "2020 - CalEPA Model",
            text = ~paste0(round(`NYeviroScreen - CalEPA Style Coverage`/`Total NY Households/Population 2018`, 2)*100, "%"),
            textposition = "auto") %>%
  layout(title = "Comparing Methods: Race <br>",
         yaxis = list(title = '',
                      categoryorder = "array",
                      categoryarray = ~`Income Bracket`),
         barmode = 'group',
         orientation = 'h',
         xaxis = Noax) %>% layout(legend = list(orientation = 'h'))

fig2

#Income Change 
comp_plot_df_inc <- comp_plot_df %>%
  filter(`Income Bracket` %in% c( "Less than 10k-25k ",
                                  "25k-45k ",            "45k-100k ",
                                  "100k and more "))

fig3 <- plot_ly(comp_plot_df_inc,
                type = "bar",
                orientation = 'h', marker = list(color = '404788FF'),
                y = ~`Income Bracket`,
                x = ~(`2000 PEJA Coverage`/`Total Households or Population 2000`)*100,
                name = "2003 PEJA Coverage", 
                text = ~paste0(round(`2000 PEJA Coverage`/`Total Households or Population 2000`,2)*100, "%"),
                textposition = "auto") %>%
  add_trace(marker = list(color = '238A8DFF'),
            x = ~round(`NYenviroScreen - Natural Breaks Coverage`/`Total NY Households/Population 2018`, 2)*100,
            name = "2020 - Natural Breaks",
            text = ~paste0(round(`NYenviroScreen - Natural Breaks Coverage`/`Total NY Households/Population 2018`, 2)*100, "%"),
            textposition = "auto") %>%
  add_trace(marker = list(color = '55C667FF'),
            x = ~round(`NYenviroScreen - Hybrid Coverage`/`Total NY Households/Population 2018`, 2)*100,
            name = "2020 - Hybrid Model",
            text = ~paste0(round(`NYenviroScreen - Hybrid Coverage`/`Total NY Households/Population 2018`, 2)*100, "%"),
            textposition = "auto") %>%
  add_trace(marker = list(color = 'FDE735FF'),
            x = ~round(`NYeviroScreen - CalEPA Style Coverage`/`Total NY Households/Population 2018`, 2)*100,
            name = "2020 - CalEPA Model",
            text = ~paste0(round(`NYeviroScreen - CalEPA Style Coverage`/`Total NY Households/Population 2018`, 2)*100, "%"),
            textposition = "auto") %>%
  layout(title = "Comparing Methods: Income <br>",
         yaxis = list(title = '',
                      categoryorder = "array",
                      categoryarray = ~`Income Bracket`),
         barmode = 'group',
         orientation = 'h',
         xaxis = Noax) %>% layout(legend = list(orientation = 'h'))

fig3


## lets find our case studies 

highScore1 <- df %>% filter(nyeScore == max(nyeScore))

names(highScore)

highScore <- highScore1 %>% dplyr::select(contains("StP_")) %>%
  dplyr::select(-StP_nyeScore) %>% gather("Indicator",
                                "NYS Percentile") %>% filter(!is.na(`NYS Percentile`)) 

fig4 <- plot_ly(
  type = 'scatterpolar',
  r = highScore$`NYS Percentile`,
  theta = highScore$Indicator,
  fill = 'toself'
) 

fig4

#rural
highScore_rur <- df %>% filter(Urban_Area_50per == 0) %>%  filter(nyeScore == max(nyeScore))

names(highScore)

highScore <- highScore_rur %>% dplyr::select(contains("StP_")) %>%
  dplyr::select(-StP_nyeScore) %>% gather("Indicator",
                                          "NYS Percentile") %>% filter(!is.na(`NYS Percentile`)) 

fig4 <- plot_ly(
  type = 'scatterpolar',
  r = highScore$`NYS Percentile`,
  theta = highScore$Indicator,
  fill = 'toself'
) 

fig4

#can we do some type of lorenz curve.... 
#anotation
#gini coeficeint of inquality 
library(ineq)
mygini <- Gini(df$nyeScore)


a <- list(
  x = 50,
  y = 50,
  text = paste0("GINI Coeficent: ", round(mygini, 2)),
  xref = "x",
  yref = "y",
  showarrow = F,
  arrowhead = 7,
  ax = 20,
  ay = -40
)

fig5 <- plot_ly(df, 
  type = 'scatter', y = ~nyeScore, x = ~StP_nyeScore
) %>%
  layout(yaxis = list(title = 'NYenviroScreen Score',
                      zeroline = T,
                      showline = FALSE,
                      showticklabels = T,
                      showgrid = FALSE),
         xaxis = list(
           title = "Percentile Rank",
           zeroline = T,
           showline = FALSE,
           showticklabels = T,
           showgrid = FALSE
         ),
         annotations = a)

fig5

plot(df$nyeScore ~ df$StP_nyeScore)

install.packages("ineq")


plot(Lc(df$nyeScore),col="darkred",lwd=2)

top20 <- df %>% filter(StP_nyeScore >= 80)
sum(top20$nyeScore)/sum(df$nyeScore)

#now lets map this shit

# some good mapping tools
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
# https://www.r-bloggers.com/inset-maps-with-ggplot2/
 
names(nyblock_groups)
ejshp <- merge(nyblock_groups, df, by.x = "GEOID", by.y = "ID", all.x = TRUE)


leaflet(ejshp) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = .2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("viridis",  -StP_nyeScore)(-StP_nyeScore),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste(StP_nyeScore))

#sweet
ejshp$PEJA2000 <- ifelse(ejshp$GEOID %in% ejb$geo.id3, 1, 0)
write_rds(ejshp, "C:/Users/Mike Petroni/Documents/GitHub/NYenviroScreen/NYenviroScreen-app/www/ejshp_053020.rds")
write_rds(ej2k, "C:/Users/Mike Petroni/Documents/GitHub/NYenviroScreen/NYenviroScreen-app/www/peja2k_053020.rds")


?percent_rank
df1 <- df %>% select(blockG, 194:226)

#reload old BlockG
blockG$nyeScore_SP <- df1$nyeScore_SP
blockG$nyeScore <- df1$nyeScore
#blockG$PopCharSc <- df1$PopCharSc
blockG$PolBurSc <- df1$PolBurSc
blockG$PolBur <- df1$PolBur

PEJA2017 <- blockG %>% filter(nyeScore_SP >= 7.5)

write_rds(PEJA2017, "C:/Users/Mike Petroni/Documents/TRI Local/PEJA2017_6_8_18.rds")###









