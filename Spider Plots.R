
#spider plots

#Here we will do a full page figure with five plots

#first the CDF of 


a <- list(
  x = highScore1$StP_nyeScore/100,
  y = highScore1$nyeScore,
  text = paste0("(b) ",details1$`County Name`,
                " County - Blockgroup: ", details1$ID),
  xref = "x",
  yref = "y",
  showarrow = T,
  arrowhead = 1,
  xanchor = "right",
  ax = -50,
  ay = -15
)

b <- list(
  x = highScore_rur$StP_nyeScore/100,
  y = highScore_rur$nyeScore,
  text = paste0("(c) ",details_rur$`County Name`,
                " County - Blockgroup: ", details_rur$ID),
  xref = "x",
  yref = "y",
  showarrow = T,
  arrowhead = 1,
  xanchor = "right",
  ax = -50,
  ay = -15
)

c <- list(
  x = highScore_min$StP_nyeScore/100,
  y = highScore_min$nyeScore,
  text = paste0("(e) ",details_min$`County Name`,
                " County - Blockgroup: ", details_min$ID),
  xref = "x",
  yref = "y",
  showarrow = T,
  arrowhead = 1,
  xanchor = "left",
  ax = 50,
  ay = -15
)

d <- list(
  x = highScore_med$StP_nyeScore/100,
  y = highScore_med$nyeScore,
  text = paste0("(d) ",details_med$`County Name`,
                " County - Blockgroup: ", details_med$ID),
  xref = "x",
  yref = "y",
  showarrow = T,
  arrowhead = 1,
  xanchor = "right",
  ax = -50,
  ay = -15
)


fig2.1 <- plot_ly(df, 
                  type = 'scatter', y = ~nyeScore, x = ~percent_rank(nyeScore), marker = list(size = 5)
) %>%
  layout(yaxis = list(title = 'NYenviroScreen Score',
                      zeroline = T,
                      showline = FALSE, 
                      showticklabels = T,
                      showgrid = FALSE),  
         xaxis = list(
           title = "Percentile Rank",
           tickformat = "%",
           zeroline = T,
           showline = FALSE,
           showticklabels = T,
           showgrid = FALSE
         ),
         annotations = list(a,b,c,d))

fig2.1 

### now lets grab our 4 cases 

#High urban

## lets find our case studies 

highScore1 <- df %>% filter(nyeScore == max(nyeScore))

details1 <- ny_enviro_screen_data %>% filter(ID == highScore1$ID)
details_urb <- attArea %>% filter(GEOID == highScore1$ID)
details1$`County Name`

highScore <- highScore1 %>% dplyr::select(contains("StP_")) %>%
  dplyr::select(-StP_nyeScore) %>% gather("Indicator",
                                          "NYS Percentile") %>% filter(!is.na(`NYS Percentile`)) 

highScore$Indicator2 <- c("Excess Cancers",
                          "Minority",
                          "Low Income",
                          "Low Education",
                          "Old Dwellings",
                          "Liguistic Iso",
                          "Young Pct",
                          "Old Pct",
                          "Rent Burden",
                          "Food Access",
                          "Heart Attacks",
                          "Asthma",
                          "Premature Death",
                          "Preterm Birth",
                          "Ozone",
                          "PM2.5",
                          "Respiratory Risk",
                          "Traffic Prox",
                          "Cancer Risk",
                          "Diesel PM",
                          "Water Discharges",
                          "Superfund Prox",
                          "RMP Prox",
                          "Landfill Prox",
                          "Flood Zone Pct")

fig2.2 <- plot_ly(
  type = 'scatterpolar',
  r = highScore$`NYS Percentile`,
  theta = highScore$Indicator2,
  marker = list(color = "black"),
  fill = 'toself',
  fillcolor = "404788FF",
  opacity = .6
) %>% layout(polar = list(radialaxis = list(tickformat = "%")))

fig2.2

#rural
highScore_rur <- df %>% filter(Urban_Area_50per == 0) %>%  filter(nyeScore == max(nyeScore))

details_rur <- ny_enviro_screen_data %>% filter(ID == highScore_rur$ID)
details_rur$`County Name`

highScore <- highScore_rur %>% dplyr::select(contains("StP_")) %>%
  dplyr::select(-StP_nyeScore) %>% gather("Indicator",
                                          "NYS Percentile") %>% filter(!is.na(`NYS Percentile`)) 

highScore$Indicator2 <- c("Excess Cancers",
                          "Minority",
                          "Low Income",
                          "Low Education",
                          "Old Dwellings",
                          "Liguistic Iso",
                          "Young Pct",
                          "Old Pct",
                          "Rent Burden",
                          "Food Access",
                          "Heart Attacks",
                          "Asthma",
                          "Premature Death",
                          "Preterm Birth",
                          "Ozone",
                          "PM2.5",
                          "Respiratory Risk",
                          "Traffic Prox",
                          "Cancer Risk",
                          "Diesel PM",
                          "Drinking Water",
                          "Water Discharges",
                          "Superfund Prox",
                          "RMP Prox",
                          "Landfill Prox")

fig2.3 <- plot_ly(
  type = 'scatterpolar',
  r = highScore$`NYS Percentile`,
  theta = highScore$Indicator2,
  fillcolor = "238A8DFF",
  marker = list(color = "black"),
  opacity = .6,
  fill = 'toself'
) %>% layout(polar = list(radialaxis = list(tickformat = "%",
                                            range = c(0, 1))))

fig2.3

#lowest
highScore_min <- df %>% filter(nyeScore == min(nyeScore))


details_min <- ny_enviro_screen_data %>% filter(ID == highScore_min$ID)
details_urb <- attArea %>% filter(GEOID == highScore_min$ID)
details_min$`County Name`
details_urb$NAME10

highScore <- highScore_min %>% dplyr::select(contains("StP_")) %>%
  dplyr::select(-StP_nyeScore) %>% gather("Indicator",
                                          "NYS Percentile") %>% filter(!is.na(`NYS Percentile`)) 

highScore$Indicator2 <- c("Excess Cancers",
                          "Minority",
                          "Low Income",
                          "Low Education",
                          "Old Dwellings",
                          "Liguistic Iso",
                          "Young Pct",
                          "Old Pct",
                          "Food Access",
                          "Disabilities",
                          "Unemployment Pct",
                          "Heart Attacks",
                          "Asthma",
                          "Premature Death",
                          "Ozone",
                          "PM2.5",
                          "Respiratory Risk",
                          "Traffic Prox",
                          "Cancer Risk",
                          "Diesel PM",
                          "Drinking Water",
                          "Water Discharges",
                          "Superfund Prox",
                          "RMP Prox",
                          "Landfill Prox",
                          "Heat Risk")

 

#middle
highScore_med <- df %>% filter(round(nyeScore,2) == 31.34)

highScore_med <- highScore_med[1,]

details_med <- ny_enviro_screen_data %>% filter(ID == highScore_med$ID)
details_urb <- attArea %>% filter(GEOID == highScore_med$ID)
details$`County Name`
details_urb$NAME10

highScore <- highScore_med %>% dplyr::select(contains("StP_")) %>%
  dplyr::select(-StP_nyeScore) %>% gather("Indicator",
                                          "NYS Percentile") %>% filter(!is.na(`NYS Percentile`)) 

highScore$Indicator2 <- c("Excess Cancers",
                          "Minority",
                          "Low Income",
                          "Low Education",
                          "Old Dwellings",
                          "Liguistic Iso",
                          "Young Pct",
                          "Old Pct",
                          "Food Access",
                          "Disabilities",
                          "Unemployment Pct",
                          "Heart Attacks",
                          "Asthma",
                          "Premature Death",
                          "Ozone",
                          "PM2.5",
                          "Respiratory Risk",
                          "Traffic Prox",
                          "Cancer Risk",
                          "Diesel PM",
                          "Drinking Water",
                          "Water Discharges",
                          "Superfund Prox",
                          "RMP Prox",
                          "Landfill Prox",
                          "Heat Risk")


fig2.4 <- plot_ly(
  type = 'scatterpolar',
  r = highScore$`NYS Percentile`,
  theta = highScore$Indicator2,
  fillcolor = "55C667FF",
  marker = list(color = "black"),
  opacity = .6,
  fill = 'toself'
) %>% layout(polar = list(radialaxis = list(tickformat = "%")))

fig2.4




