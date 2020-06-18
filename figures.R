#figures


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
  layout(title = "Comparing Methods: Coverage by Race <br>",
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
  layout(title = "Comparing Methods: Coverage by Household Income <br>",
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

#or using ggplot
p = ggplot(highScore, aes(x=Indicator, y=`NYS Percentile`, fill=`NYS Percentile`)) +
  geom_histogram(binwidth=1, stat='identity') + theme_light() +
  scale_fill_gradient(low="red", high="white", limits=c(0,100)) +
  theme(axis.title.y=element_text(angle=0))
p + theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))

p + coord_polar()

#rural
highScore_rur <- df %>% filter(Urban_Area_50per == 0) %>%  filter(nyeScore == max(nyeScore))

names(highScore)

highScore <- highScore_rur %>% dplyr::select(contains("StP_")) %>%
  dplyr::select(-StP_nyeScore) %>% gather("Indicator",
                                          "NYS Percentile") %>% filter(!is.na(`NYS Percentile`)) 

fig4.rur <- plot_ly(
  type = 'scatterpolar',
  r = highScore$`NYS Percentile`,
  theta = highScore$Indicator,
  fill = 'toself'
) 

fig4.rur

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

#lets map our natural breaks (Jenks Clusters)
# poverty_2018 
# Urb_race
# rur_race
b <- list(
  x = jenks.ints.2k$brks[2],
  y = 250,
  text = paste0("Jenks Poverty <br> Cluster Threshold: ",
                round(jenks.ints.2k$brks[2]*100, 2), "%"),
  xref = "x",
  yref = "y",
  showarrow = T,
  arrowhead = 7,
  ax = 85,
  ay = -40
)
poverty_2018$cluster <- ifelse(poverty_2018$per_poverty >= jenks.ints.2k$brks[2],
                               "PEJA","Not PEJA")
fig6 <- plot_ly(poverty_2018, x = ~per_poverty,
                type = "histogram", color = ~cluster) %>%
  add_segments(inherit = F, x=jenks.ints.2k$brks[2],
               y=0, xend=jenks.ints.2k$brks[2],
               yend=489, line=list(color="black", width = 2),
               name = "Threshold", showlegend = F) %>%
  layout(yaxis = list(title = 'Tract Count',
                      zeroline = T,
                      showline = FALSE,
                      showticklabels = T,
                      showgrid = FALSE),
         xaxis = list(
           title = "Percent Living Under Federal Poverty Line 2018",
           zeroline = T,
           showline = FALSE,
           showticklabels = T,
           tickformat = "%",
           showgrid = FALSE
         ), legend = list(x = 0.6, y = 0.2),
         annotations = b, showlegend = T)
fig6

# Urb_race
b <- list(
  x = jenks.ints.race.urb$brks[2],
  y = 500,
  text = paste0("Jenks Urban Race <br> Cluster Threshold: ",
                round(jenks.ints.race.urb$brks[2]*100, 2), "%"),
  xref = "x",
  yref = "y",
  showarrow = T,
  arrowhead = 7,
  ax = -95,
  ay = -20
)
Urb_race$cluster <- ifelse(Urb_race$MINORPCT >= jenks.ints.race.urb$brks[2],
                           "PEJA","Not PEJA")
fig7 <- plot_ly(Urb_race, x = ~MINORPCT,
                type = "histogram", color = ~cluster) %>%
  add_segments(inherit = F, x=jenks.ints.race.urb$brks[2],
               y=0, xend=jenks.ints.race.urb$brks[2],
               yend=969, line=list(color="black", width = 2),
               name = "Threshold", showlegend = F) %>%
  layout(yaxis = list(title = 'Block Group Count',
                      zeroline = T,
                      showline = FALSE,
                      showticklabels = T,
                      showgrid = FALSE),
         xaxis = list(
           title = "Urban Block Groups - Percent Minority 2018",
           zeroline = T,
           showline = FALSE,
           showticklabels = T,
           tickformat = "%",
           showgrid = FALSE
         ), legend = list(x = 0.6, y = 0.5),
         annotations = b, showlegend = T)
fig7

# rur_race
b <- list(
  x = jenks.ints.race.rur$brks[2],
  y = 250,
  text = paste0("Jenks Rural Race <br> Cluster Threshold: ",
                round(jenks.ints.race.rur$brks[2]*100, 2), "%"),
  xref = "x",
  yref = "y",
  showarrow = T,
  arrowhead = 7,
  ax = 95,
  ay = -20
)
rur_race$cluster <- ifelse(rur_race$MINORPCT >= jenks.ints.race.rur$brks[2],
                           "PEJA","Not PEJA")
fig8 <- plot_ly(rur_race, x = ~MINORPCT,
                type = "histogram", color = ~cluster) %>%
  add_segments(inherit = F, x=jenks.ints.race.rur$brks[2],
               y=0, xend=jenks.ints.race.rur$brks[2],
               yend=495, line=list(color="black", width = 2),
               name = "Threshold", showlegend = F) %>%
  layout(yaxis = list(title = 'Block Group Count',
                      zeroline = T,
                      showline = FALSE,
                      showticklabels = T,
                      showgrid = FALSE),
         xaxis = list(
           title = "Rural Block Groups - Percent Minority 2018",
           zeroline = T,
           showline = FALSE,
           showticklabels = T,
           tickformat = "%",
           showgrid = FALSE
         ), legend = list(x = 0.7, y = 0.2),
         annotations = b, showlegend = T)
fig8
