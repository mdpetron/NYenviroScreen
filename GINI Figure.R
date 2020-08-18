#Results figure 1 

#GINI comparison

#packages 
library(ineq)
library(plotly)
library(dplyr)

#make a urban/rural catagorical variable 

df <- df %>%
  mutate("Blockgroup Type" = ifelse(Urban_Area_50per == 1, "Urban", "Rural"))

################# nyes ############## 
#get gigi coef for variable 
mygini <- Gini(df$nyeScore)

a <- list(
  x = .7,
  y = 30000,
  text = paste0("<b>GINI Coeficent: ", round(mygini, 2), "</b>"),
  xref = "x",
  yref = "y",
  showarrow = F,
  arrowhead = 7,
  ax = 20,
  ay = -40
)

dfplot <- df %>% arrange(desc(-nyeScore)) %>% mutate(cum_score = cumsum(nyeScore))

linerank <- c(0, 1)
linecum <- c(0, max(dfplot$cum_score))

equal <- data.frame(linerank, linecum)



fig1.1 <- plot_ly(dfplot, 
                  type = 'scatter', y = ~cum_score, x = ~percent_rank(nyeScore),
                  name = "Cumulative Burden Equality", mode = "lines"
) %>% add_trace(inherit = F, data = equal, y = ~linecum, x = ~linerank, mode = "lines",
                name = "Perfect Equality") %>%
  layout(yaxis = list(title = 'Cumulative NYenviroScreen Score',
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
         legend = list(x = 0.1, y = 0.9,
                       bgcolor = "#E2E2E2",
                       bordercolor = "#FFFFFF",
                       borderwidth = 2),
         annotations = a)

fig1.1 



############# other CDF plots not used 





################# health ############## 

mygini <- Gini(df$Health)

a <- list(
  x = .25,
  y = 80,
  text = paste0("Health Score<br>GINI Coeficent: ", round(mygini, 2)),
  xref = "x",
  yref = "y",
  showarrow = F,
  arrowhead = 7,
  ax = 20,
  ay = -40
)

fig1.2 <- plot_ly(df, 
                  type = 'scatter', y = ~Health, x = ~percent_rank(Health), marker = list(size = 5)
) %>%
  layout(yaxis = list(title = 'Health Score',
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
         annotations = a)

fig1.2 


################# Sensitivepops ############## 

mygini <- Gini(df$Sensitivepops)

a <- list(
  x = .25,
  y = 80,
  text = paste0("Sensative Populations Score<br>GINI Coeficent: ", round(mygini, 2)),
  xref = "x",
  yref = "y",
  showarrow = F,
  arrowhead = 7,
  ax = 20,
  ay = -40
)

fig1.3 <- plot_ly(df, 
                  type = 'scatter', y = ~Sensitivepops, x = ~percent_rank(Sensitivepops), marker = list(size = 5)
) %>%
  layout(yaxis = list(title = 'Sensative Populations Score',
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
         annotations = a)

fig1.3 

################# SocioEcon ############## 

mygini <- Gini(df$SocioEcon)

a <- list(
  x = .25,
  y = 80,
  text = paste0("Socioeconomic Vulnerability Score<br>GINI Coeficent: ", round(mygini, 2)),
  xref = "x",
  yref = "y",
  showarrow = F,
  arrowhead = 7,
  ax = 20,
  ay = -40
)

fig1.4 <- plot_ly(df, 
                  type = 'scatter', y = ~SocioEcon, x = ~percent_rank(SocioEcon), marker = list(size = 5)
) %>%
  layout(yaxis = list(title = 'Socioeconomic Vulnerability Score',
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
         annotations = a)

fig1.4

################# exposures ############## 

mygini <- Gini(df$exposure)

a <- list(
  x = .25,
  y = 80,
  text = paste0("Exposures Score<br>GINI Coeficent: ", round(mygini, 2)),
  xref = "x",
  yref = "y",
  showarrow = F,
  arrowhead = 7,
  ax = 20,
  ay = -40
)

fig1.5 <- plot_ly(df, 
                  type = 'scatter', y = ~exposure, x = ~percent_rank(exposure), marker = list(size = 5)
) %>%
  layout(yaxis = list(title = 'Exposures Score',
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
         annotations = a)

fig1.5

################# Hazards ############## 

mygini <- Gini(df$effects)

a <- list(
  x = .25,
  y = 80,
  text = paste0("Hazards Score<br>GINI Coeficent: ", round(mygini, 2)),
  xref = "x",
  yref = "y",
  showarrow = F,
  arrowhead = 7,
  ax = 20,
  ay = -40
)

fig1.6 <- plot_ly(df, 
                  type = 'scatter', y = ~effects, x = ~percent_rank(effects), marker = list(size = 5)
) %>%
  layout(yaxis = list(title = 'Hazards Score',
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
         annotations = a)

fig1.6

# can we combines these charts? 

fig <- subplot(fig1.1, fig1.2,
               fig1.3, fig1.4,
               fig1.5, fig1.6, nrows = 3)

fig






