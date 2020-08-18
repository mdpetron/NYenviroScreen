#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# setwd("C:/Users/Mike Petroni/Documents/GitHub/NYenviroScreen/NYenviroScreen-app")

library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(sp)

ejshp <- readRDS("www/ejshp_061520.rds")
ej2k <- readRDS("www/peja2k_061520.rds")
peja2k.2 <- subset(ej2k, ej2k$peja2k.1 == 1)
cumburd <- subset(ejshp, ejshp$peja_18_cal == 1)
hybrid <- subset(ejshp, ejshp$peja_18_hybrid == 1)

df2k <- as.data.frame(ej2k)
df <- as.data.frame(ejshp)
df$ALAND <- as.numeric(df$ALAND)

#for the evaluation

minority2k <- round((sum(peja2k.2$total_pop)-sum(peja2k.2$white_alone))/(sum(df2k$total_pop)-sum(df2k$white_alone))*100,2)
population2k <- round(sum(peja2k.2$total_pop)/sum(df2k$total_pop)*100,2)
poverty2k <- round(sum(peja2k.2$under_poverty)/(sum(df2k$under_poverty))*100,2)

df2 <- df %>% filter(peja_18_hybrid == 1)

minority22k <- round((sum(df2$population)-sum(df2$white_alone))/(sum(df$population)-sum(df$white_alone))*100,2)
population22k <- round(sum(df2$population)/sum(df$population)*100,2)
poverty22k <- round(sum(df2$per_poverty*df2$population, na.rm = T)/sum(df$per_poverty*df$population, na.rm = T)*100,2)

df3 <- df %>% filter(peja_18_cal == 1)

minority23k <- round((sum(df3$population)-sum(df3$white_alone))/(sum(df$population)-sum(df$white_alone))*100,2)
population23k <- round(sum(df3$population)/sum(df$population)*100,2)
poverty23k <- round(sum(df3$per_poverty*df3$population, na.rm = T)/sum(df$per_poverty*df$population, na.rm = T)*100,2)


Allmets <- c("2003 Potential EJ Areas (CP-29)" = "PEJA2000",
             "2020 Cumulative Burden Model" = "CumBurd",
             "2020 Hybrid Model" = "Hybrid",
             "Minority (%)" = "StP_MINORPCT",
            "Low Income (%)" = "StP_LOWINCPCT",
            "Educational Attainment (%)" = "StP_LESSHSPCT",
            "Linguistic Isolation (%)" = "StP_LINGISOPCT",
            "Unemployment (%)" = "StP_PercentUnemployed",
            "Low Income - Food Desert"  = "StP_FoodAccess",
            "High Rent Burden" = "StP_RentBurden",
    "Over 64 (%)" = "StP_OVER64PCT",
    "Under 5 (%)" = "StP_UNDER5PCT",
    "Heart Attack*" = "StP_heart_atack",
    "Asthma*" = "StP_asthma",
    "Premature Death*" = "StP_premature_death",
    "Disability (%)" = "StP_PercentDisability",
    "Preterm Birth*" = "StP_preterm_birth",
    "Heat Vulnerability*" = "StP_HVIScore",
    "Increased Cancer Incidence" = "StP_DifExpectedTotal",
    "Homes in Floodplain (%)" = "StP_shr_hu_fp_any",
    "Traffic Proximity*" = "StP_PTRAF",
              "Ozone*" = "StP_OZONE",
              "Lead Paint*" = "StP_PRE1960PCT",
              "Respiratory Hazard*" = "StP_RESP",
              "Cancer Hazard*" = "StP_CANCER",
              "Particulate Matter*" = "StP_PM25",
              "Diesel Exaust*" = "StP_DSLPM",
              "Drinking Water Contaminants*" = "StP_DrinkWaterScore",
    "Waste Water Discharge Proximity*" = "StP_PWDIS",
             "Superfund Site Proximity*" = "StP_PNPL",
             "Hazardous Waste Landfill Proximity*" = "StP_PTSDF",
             "Risk Management Plan Facility Proximity*" = "StP_PRMP") 


# Define UI for application 
ui <- bootstrapPage(

    #some css styles
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    #our backgrounds map
    leafletOutput("map", width = "70%", height = "100%"),
    
    #right side panel 
    absolutePanel(id = "controls", 
                  fixed = TRUE, top = 0, left = "auto", right = 0, bottom = 0,
                  width = "30%", height = "auto", style = "opacity: 0.92; overflow-y:scroll", 
                  wellPanel( id = "controls",
                             h1("NYenviroScreen"),
                             h2("A Data Driven Method for Designating NYS Potential Environmental Justice Areas"),
                             p(class = "text-muted",
                               paste("This application was designed by the Center for Environmental Medicine and Informatics (CEMI)",
                                     "at the SUNY College Of Environmental Science and Forestry."
                               ),
                               a("Learn more about this project and the following metrics.",
                                 href = "https://docs.google.com/document/d/1aITVs6CvRC5pc1eEulXxn3m2cvu7Qw_2YVw027SEk1I/edit?usp=sharing",
                                 target="_blank"),
                               selectizeInput("Allmets", "Map Environmental Justice Metrics",
                                              Allmets, selected = "2000 Potential EJ Areas", multiple = FALSE),
                               h4(strong("Potential Environmental Justice Area Coverage")),fluidRow(
                                   #column(1,h3("Evaluation Metrics")),
                                   column(12,h5(strong("2003 CP-29 Model")),
                                          h6(paste0("Population: ", population2k, "%")),
                                          h6(paste0("Minority Population: ", minority2k, "%")),
                                          h6(paste0("Low Income Population: ", poverty2k, "%")),
                                   h5(strong("Cumulative Burden Model")),
                                   h6(paste0("Population: ", population23k, "%")),
                                   h6(paste0("Minority Population: ", minority23k, "%")),
                                   h6(paste0("Low Income Population: ", poverty23k, "%")),
                                   h5(strong("NYenviroScreen - Hybrid Model")),
                                   h6(paste0("Population: ", population22k, "%")),
                                   h6(paste0("Minority Population: ", minority22k, "%")),
                                   h6(paste0("Low Income Population: ", poverty22k, "%"))  )))))

                             )

server <- function(input, output, session) {
    
    #we want to do a few things, 
    #maps the areas, with outlines that indicate if the meet the threshold or not 
    #make a few charts for income and minority coverage 
    
    output$map <- renderLeaflet({ 
        leaflet() %>% setView(lng = -75.5445, lat = 42.874, zoom = 7)  %>% 
            addTiles()
    })
    
    observeEvent(input$Allmets,{
        print(input$Allmets)
        if(input$Allmets == "PEJA2000"){
            leafletProxy("map", data = peja2k.2) %>%
                clearShapes() %>%
                clearMarkers() %>%
                removeControl(layerId = "legend") %>%
                addPolygons(color = "#444444", weight = .2, smoothFactor = 0.5,
                            opacity = 1.0, fillOpacity = 0.5,
                            fillColor = "purple",
                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                bringToFront = TRUE),
                            popup = ~paste0("BlockGroupID: ", GEOID)) %>%                                                                            
                addLegend("bottomleft", colors = "purple", labels = "2003 PEJA",
                          title = "2003 Potential Environmental Justice Areas", opacity = 1, layerId = "legend")
            
        } else if(input$Allmets == "CumBurd"){
            leafletProxy("map", data = cumburd) %>%
                clearShapes() %>%
                clearMarkers() %>%
                removeControl(layerId = "legend") %>%
                addPolygons(color = "#444444", weight = .2, smoothFactor = 0.5,
                            opacity = 1.0, fillOpacity = 0.5,
                            fillColor = "gree",
                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                bringToFront = TRUE),
                            popup = ~paste0("BlockGroupID: ", GEOID)) %>%                                                                            
                addLegend("bottomleft", colors = "green", labels = "2020 Cummulative Burden Model",
                          title = "2000 Potential Environmental Justice Areas", opacity = 1, layerId = "legend")
            
        } else if(input$Allmets == "Hybrid"){
            leafletProxy("map", data = hybrid) %>%
                clearShapes() %>%
                clearMarkers() %>%
                removeControl(layerId = "legend") %>%
                addPolygons(color = "#444444", weight = .2, smoothFactor = 0.5,
                            opacity = 1.0, fillOpacity = 0.5,
                            fillColor = "blue",
                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                bringToFront = TRUE),
                            popup = ~paste0("BlockGroupID: ", GEOID)) %>%                                                                            
                addLegend("bottomleft", colors = "blue", labels = "2020 Hybrid Model",
                          title = "2000 Potential Environmental Justice Areas", opacity = 1, layerId = "legend")
            
        } else {
        
        df2 <- df %>% dplyr::select(., GEOID, input$Allmets)
        names(df2) <- c("GEOID", "myval")
        ejshp <- sp::merge(ejshp, df2, by = "GEOID", all.x = TRUE)
        
        quantileNum <- 4
        
        probs <- seq(0, 1, length.out = quantileNum + 1)
        bins <- quantile(ejshp$myval, probs, na.rm = TRUE, names = FALSE)
        
        while (length(unique(bins)) != length(bins)) {
            quantileNum <- quantileNum - 1
            probs <- seq(0, 1, length.out = quantileNum + 1)
            bins <- quantile(ejshp$myval, probs, na.rm = TRUE, names = FALSE)
        }
        
        pal <- colorBin("viridis", bins = bins)

        # pal <-  colorQuantile("viridis",  ejshp$myval)
        
        #### size by percent of total? ### might be a solution to the sizing problem...... looks great so far tho! 
        leafletProxy("map", data = ejshp) %>%
            clearShapes() %>%
            clearMarkers() %>%
            removeControl(layerId = "legend") %>%
            addPolygons(color = "#444444", weight = .2, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5,
                        fillColor = ~pal(myval),
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE),
                        popup = ~paste0("BlockGroupID: ", GEOID, "<br>", input$Allmets, ": ", round(myval, 2))) %>%                                                                            
            addLegend("bottomleft", pal = pal, values = ejshp$myval,
                      title = "NYenviroScreen Percentile", opacity = 1, layerId = "legend")
        }
        
    })
    
    
}


shinyApp(ui, server)


