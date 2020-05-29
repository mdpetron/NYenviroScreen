#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(dplyr)


county_df <- readRDS("www/covid04152020.rds")

county_df$County <- ifelse(is.na(county_df$County), "Unidentified", county_df$County)
county_df$County <- as.character(county_df$County)
names(county_df)

y <- c("NATA Respiratory Hazard" = "nataRespHaz",
       "PM2.5" = "pm25_12_20",
       "RSEI ToxConc" = "ToxConc_Co_mean",
       "NATA Naphthalene" = "NAPHTHALENE",
       "NATA Acrolein" = "ACROLEIN",
       "NATA Formaldehyde" = "FORMALDEHYDE",
       "NATA Chloroprene" = "CHLOROPRENE",
       "NATA Point Source" = "PT-StationaryPoint Respiratory (hazard quotient)",
       "NATA Secondary" = "SECONDARY Respiratory (hazard quotient)",
       "Percent Without Active Leisure Time" = "active",
       "Adult Obesity Rate" = "measurename_mean_12to20_Adult obesity",
       "Poverty Rate" = "Poverty_Per_mean_12to18",
       "Median Income" = "median_income_mean_12to18",
       "Percent African American" = "Black_Per_mean_12to18") 

y2 <- c("Covid Deaths" = "cov_deaths",
        "Percent Without Active Leisure Time" = "active",
        "Adult Obesity Rate" = "measurename_mean_12to20_Adult obesity",
        "Poverty Rate" = "Poverty_Per_mean_12to18",
        "Median Income" = "median_income_mean_12to18",
        "Percent African American" = "Black_Per_mean_12to18") 

x <- unique(county_df$State)

# Define UI for application that draws a histogram
ui <- bootstrapPage(
    
    # navbarPage("Power Pollution Costs", id="nav",
    
    # tabPanel("Interactive map",
    #div(class="outer",
    
    
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(id = "controls", 
                  fixed = TRUE, top = 0, left = "auto", right = 0, bottom = 0,
                  width = 700, height = "auto", style = "opacity: 0.82; overflow-y:scroll", 
                  wellPanel( id = "controls",
                             h2("Covid-19 Mortality and Air Pollution"),
                             p(class = "text-muted",
                               paste("Mike Petroni, mdpetron@syr.edu, Center for Environmental Medicine and Informatics (CEMI)",
                                     "at the SUNY College Of Environmental Science and Forestry - TEST APP"
                               ), 
                               
                               # sliderInput("year", "Year", min = 1997, max = 2017, value = 2015, animate = TRUE, sep = "", ticks = FALSE),
                               selectizeInput("state", "States", x, selected = unique(county_df$State), multiple = TRUE),
                               # checkboxGroupInput("area", "Region", xx, selected = "Northeast", inline = TRUE),
                               # sliderInput("thres", "Super Polluter Threshold", min = 75, max = 99, value = 95, ticks = TRUE),
                               selectInput("var", "Color map points by (plot Y)", y, selected = "nataRespHaz"), 
                               selectInput("var2", "Size map points by (plot X)", y2, selected = "cov_deaths"), 
                               checkboxInput("log", "Log x axis?", value = FALSE),
                               # h3("In-View Graphs"),
                               # paste("These graphs will ajust to map bounds showing only the data in-view on the map."),
                               # HTML("<br><br><br>"),
                               # paste("This graph allows for comparion of in-view plants. The points are sized by total yearly MWh produced."),
                               # HTML("<br><br>"),
                               # plotlyOutput("localChange2", height = 300),
                               # HTML("<br>"),
                               # paste("This graph examines emissions costs from each Electic Generating Unit (EGU) as thier proportion of total costs."),
                               # HTML("<br><br>"),
                               plotlyOutput("localChange3", height = 400)
                               #,
                               #          dataTableOutput("table"),
                               #          
                               #          h3("Static Graphs"),
                               #          
                               #          plotlyOutput("localChange", height = 400),
                               #          HTML("<br>"),
                               #          plotlyOutput("localChange4", height = 400),
                               #          HTML("<br>"),
                               #          plotlyOutput("localChange5", height = 400),
                               #          HTML("<br>"),
                               #          h3("Focus: New York State"),
                               #          paste("This section focuses in on NYS to examine in-state regional trends, averaged costs per MWh, and future emmssions forcasts. In 2017 NYS
                               # implemented the Clean Energy Standard (CES). The graphs below show interactive estimates of health costs saving tied ot NOx and SO2 reductions
                               # in three NYS electric regions"),
                               #          HTML("<br><br>"),
                               #          #img(src = "Figure-4-NERC-Interconnections-and-Regions.png", height = 72, width = 72),
                               #          #HTML("<br>"),
                               #          sliderInput("int", "Intrest Rate", min = 1, max = 15, value = 5, ticks = TRUE),
                               #          sliderInput("up", "Up State RE Displacment Percentage", min = 1, max = 50, value = 30, ticks = TRUE),
                               #          sliderInput("li", "Long Island RE Displacment Percentage", min = 1, max = 50, value = 30, ticks = TRUE),
                               #          sliderInput("cw", "NYCW RE Displacment Percentage", min = 1, max = 50, value = 30, ticks = TRUE),
                               #          plotlyOutput("fortot1", height = 400),
                               #          HTML("<br>"),
                               #          plotlyOutput("fortot", height = 400),
                               #          HTML("<br>"),
                               #          plotlyOutput("forGraphNOx", height = 400),
                               #          HTML("<br>"),
                               #          plotlyOutput("forGraphSO2", height = 400),
                               #          HTML("<br>"),
                               #          plotlyOutput("localChange6", height = 400),
                               #          HTML("<br>"),
                               #          plotlyOutput("localChange7", height = 400),
                               #          HTML("<br>"),
                               #          plotlyOutput("localChange8", height = 400),
                               #          HTML("<br>"),
                               #          plotlyOutput("localChange9", height = 300),
                               #          HTML("<br>"),
                               #          plotlyOutput("localChange10", height = 300),
                               #          HTML("<br>"),
                               #          plotlyOutput("localChange11", height = 300),
                               #          HTML("<br>"),
                               #          plotlyOutput("localChange12", height = 300),
                               #          #img(src = "egrid2014_egrid_subregions_0.jpg", height = 72, width = 72)
                               #          paste("This viewer uses several data sources. Emissions data comes from
                               #   the EPA AMPD (Air Markets Program Dataset). Marginal emission costs per ton
                               #   comes from the EASIUR model (Estimating Air pollution Social Impact Using Regression).
                               #   More information about the AMPD can be found by pasting this link in your browser:
                               #   https://ampd.epa.gov/ampd/ and more information about EASIUR can be found here: 
                               #   http://barney.ce.cmu.edu/~jinhyok/easiur/"
                               #          ) 
                               
                               
                             ))))

server <- function(input, output, session) {
    
    
    
    filteredtype <- reactive({
        print(input$var2)
        dat <- county_df %>% filter(State %in% input$state) %>% select(LAT, LON, County, State,`covid_cases_ X4.15.20`, input$var, input$var2) 
        print(head(dat))
        dat <- dat %>% mutate(element = dat[,6],
                              element2 = dat[,7])
        print(head(dat))
        return(dat)
    })
    
    
    output$map <- renderLeaflet({ 
        leaflet() %>% setView(lng = -74.1445, lat = 40.174, zoom = 4)  %>% 
            addTiles()
    })
    
    InBounds <- reactive({
        if (is.null(input$map_bounds))
            return(us.e.dat[FALSE,])
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(filteredtype(),
               Facility.Latitude >= latRng[1] & Facility.Latitude <= latRng[2] &
                   Facility.Longitude >= lngRng[1] & Facility.Longitude <= lngRng[2])
    })
    
    # table <- reactive({ renderDataTable(arrange(filteredtype(), desc(filteredtype()$so2Noxcost)), options = NULL, searchDelay = 500,
    #                                     callback = "function(oTable) {}", escape = TRUE, env = parent.frame(),
    #                                     quoted = FALSE, outputArgs = list())
    # })
    # 
    
    
    output$localChange3 <- renderPlotly({
        
        if(input$log == T){
            plot_ly(filteredtype(),
                    type = "scatter", mode = "markers", x = ~log(element2), y = ~element, color = ~State) %>%
                layout( yaxis = list(title = paste(input$var),
                                     gridcolor = 'rgb(255, 255, 255)',
                                     zerolinewidth = 1,
                                     ticklen = 5,
                                     gridwidth = 2),
                        xaxis = list(title =  paste0("Log - ",input$var2),
                                     gridcolor = 'rgb(255, 255, 255)',
                                     zerolinewidth = 1,
                                     ticklen = 5,
                                     gridwith = 2),
                        paper_bgcolor = 'rgb(243, 243, 243)',
                        plot_bgcolor = 'rgb(243, 243, 243)')
        } else {
            plot_ly(filteredtype(),
                    type = "scatter", mode = "markers", x = ~element2, y = ~element, color = ~State) %>%
                layout( yaxis = list(title = paste(input$var),
                                     gridcolor = 'rgb(255, 255, 255)',
                                     zerolinewidth = 1,
                                     ticklen = 5,
                                     gridwidth = 2),
                        xaxis = list(title =  paste(input$var2),
                                     gridcolor = 'rgb(255, 255, 255)',
                                     zerolinewidth = 1,
                                     ticklen = 5,
                                     gridwith = 2),
                        paper_bgcolor = 'rgb(243, 243, 243)',
                        plot_bgcolor = 'rgb(243, 243, 243)')  
        }
        
        
        
        
    })
    
    
    
    observe({
        
        value <- input$var
        pal <- colorBin("YlOrBr", filteredtype()$element, bins =9)
        if(max(filteredtype()$element2, na.rm =T) > 2) {
            
            #### size by percent of total? ### might be a solution to the sizing problem...... looks great so far tho! 
            leafletProxy("map", data = filteredtype()) %>%
                clearShapes() %>%
                clearMarkers() %>%
                removeControl(layerId = "legend") %>%
                addCircleMarkers(lng = ~LON, lat = ~LAT, color = "#444444",
                                 opacity = .1, fillOpacity = 0.4, radius = ~(log(element2)*input$map_zoom)/2,
                                 fillColor = ~colorBin("YlOrBr", element, bins =9)(element), popup = ~paste0(County, ", ", State, "</br> 4/15 Covid-19 Cases: ",
                                                                                                             `covid_cases_ X4.15.20`,
                                                                                                             "</br> Deaths: ",
                                                                                                             element2,
                                                                                                             "</br> Pollution: ",
                                                                                                             element)) %>%
                addLegend("bottomleft", pal = pal, values = filteredtype()$element,
                          title = paste(value), opacity = 1, layerId = "legend")
        } else {
            leafletProxy("map", data = filteredtype()) %>%
                clearShapes() %>%
                clearMarkers() %>%
                removeControl(layerId = "legend") %>%
                addCircleMarkers(lng = ~LON, lat = ~LAT, color = "#444444",
                                 opacity = .1, fillOpacity = 0.4, radius = ~((element2*5)*input$map_zoom),
                                 fillColor = ~colorBin("YlOrBr", element, bins =9)(element), popup = ~paste0(County, ", ", State, "</br> 4/15 Covid-19 Cases: ",
                                                                                                             `covid_cases_ X4.15.20`,
                                                                                                             "</br> Deaths: ",
                                                                                                             element2,
                                                                                                             "</br> Pollution: ",
                                                                                                             element)) %>%
                addLegend("bottomleft", pal = pal, values = filteredtype()$element,
                          title = paste(value), opacity = 1, layerId = "legend")  
        }
        
    })
    
}


shinyApp(ui, server)
