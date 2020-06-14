#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
 setwd("C:/Users/Mike Petroni/Documents/GitHub/NYenviroScreen/NYenviroScreen-app")

library(shiny)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(matrixStats)

ejshp <- readRDS("www/ejshp_053020.rds")
ej2k <- readRDS("www/peja2k_053020.rds")
peja2k.2 <- subset(ej2k, ej2k$peja2k.1 == 1)

df2k <- as.data.frame(ej2k)

df <- as.data.frame(ejshp)
df$ALAND <- as.numeric(df$ALAND)

#afunction to use

rowMaxs <- function(df, na.rm=TRUE) {
    
    if (is.matrix(df)) {df <- data.frame(df, stringsAsFactors=FALSE, drop = FALSE)}
    
    valid.cols <- sapply(df, function(x) { is.numeric(x) || is.logical(x) || is.character(x)})
    stopifnot(any(valid.cols))
    # or could just return NA?:
    # if (!any(valid.cols)) {return(NA)}
    if (any(!valid.cols) ) {warning('using only numeric (double or integer) or logical or character columns -- ignoring other columns ')}
    
    result <- do.call(pmax, c(df[ , valid.cols, drop = FALSE], na.rm=na.rm))
    
    result[nononmissing <- rowSums(!is.na(df[ , valid.cols, drop = FALSE]))==0] <- -Inf
    if (any(nononmissing)) {warning('where no non-missing arguments, returning -Inf')}
    return(result)
    
    # df = data.frame of numeric values, i.e. a list of vectors passed to pmax
    # Value returned is vector, each element is max of a row of df
}


# here is the baseline NYenviroScreen based on Cal-EnviroScreen
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
           Sensitivepops = rowMeans(dplyr::select(., StP_UNDER5PCT,
                                                  StP_OVER64PCT,
                                                  StP_heart_atack,
                                                  StP_asthma,
                                                  StP_premature_death,
                                                  StP_PercentDisability,
                                                  StP_preterm_birth,
                                                  StP_HVIScore,
                                                  StP_shr_hu_fp_any),
                                    na.rm = T)*100,
           SocioEcon = rowMeans(dplyr::select(.,StP_LOWINCPCT,
                                              StP_LESSHSPCT,
                                              StP_LINGISOPCT,
                                              StP_PercentUnemployed,
                                              StP_MINORPCT),
                                na.rm = T)*100) %>% ungroup()
#Here is the evaluation

dfs <- df %>% dplyr::select(38:44, 46:62) %>%
    summarise_all(sum) %>% gather("Income Bracket",
                                  "Total NY House Holds 2018")
dfsb <- df %>% filter(StP_nyeScore >= 75) %>% dplyr::select(38:44, 46:62) %>%
    summarise_all(sum) %>% gather("Income Bracket",
                                  "NYeviroScreen Coverage")
dfsc1 <- df2k %>% mutate(population = totpop) %>%
    dplyr::select(51:71, 73,75,76) %>%
    summarise_all(sum, na.rm =T) %>% gather("Income Bracket",
                                  "Total Households or Population 2000")
dfsc <- df2k %>% filter(peja2k.1 == 1) %>% mutate(population = totpop) %>%
    dplyr::select(51:71, 73,75,76) %>%
    summarise_all(sum) %>% gather("Income Bracket",
                                  "2000 PEJA Coverage")


dfs <- left_join(dfs, dfsb)
dfs <- left_join(dfs, dfsc)
dfs <- left_join(dfs, dfsc1)

ComboMeth <- c("Mean", "Sum", "Maximum")

#list for the selection 
SocDem <- c("Minority (%)" = "StP_MINORPCT",
       "Low Income (%)" = "StP_LOWINCPCT",
       "Educational Attainment (%)" = "StP_LESSHSPCT",
       "Linguistic Isolation (%)" = "StP_LINGISOPCT",
       "Unemployment (%)" = "StP_PercentUnemployed")
       
Sensa <- c(
       "Over 64 (%)" = "StP_OVER64PCT",
       "Under 5 (%)" = "StP_UNDER5PCT",
       "Heart Attack*" = "StP_heart_atack",
       "Asthma*" = "StP_asthma",
       "Premature Death*" = "StP_premature_death",
       "Disability (%)" = "StP_PercentDisability",
       "Preterm Birth*" = "StP_preterm_birth",
       "Heat Vulnerability*" = "StP_HVIScore",
       "Homes in Floodplain (%)" = "StP_shr_hu_fp_any") 

exposure <- c("Traffic Proximity*" = "StP_PTRAF",
        "Ozone*" = "StP_OZONE",
        "Lead Paint*" = "StP_PRE1960PCT",
        "Respiratory Hazard*" = "StP_RESP",
        "Cancer Hazard*" = "StP_CANCER",
        "Particulate Matter*" = "StP_PM25",
        "Diesel Exaust*" = "StP_DSLPM",
        "Drinking Water Contaminents*" = "StP_DrinkWaterScore") 

effects <- c("Waste Water Discharge Proximity*" = "StP_PWDIS",
              "Superfund Site Proximity*" = "StP_PNPL",
              "Hazardous Waste Landfill Proximity*" = "StP_PTSDF",
              "Risk Managment Plan Facility Proximity*" = "StP_PRMP") 

Allmets <- c("2000 Potential EJ Areas" = "PEJA2000",
             "Minority (%)" = "StP_MINORPCT",
            "Low Income (%)" = "StP_LOWINCPCT",
            "Educational Attainment (%)" = "StP_LESSHSPCT",
            "Linguistic Isolation (%)" = "StP_LINGISOPCT",
            "Unemployment (%)" = "StP_PercentUnemployed",
    "Over 64 (%)" = "StP_OVER64PCT",
    "Under 5 (%)" = "StP_UNDER5PCT",
    "Heart Attack*" = "StP_heart_atack",
    "Asthma*" = "StP_asthma",
    "Premature Death*" = "StP_premature_death",
    "Disability (%)" = "StP_PercentDisability",
    "Preterm Birth*" = "StP_preterm_birth",
    "Heat Vulnerability*" = "StP_HVIScore",
    "Homes in Floodplain (%)" = "StP_shr_hu_fp_any",
    "Traffic Proximity*" = "StP_PTRAF",
              "Ozone*" = "StP_OZONE",
              "Lead Paint*" = "StP_PRE1960PCT",
              "Respiratory Hazard*" = "StP_RESP",
              "Cancer Hazard*" = "StP_CANCER",
              "Particulate Matter*" = "StP_PM25",
              "Diesel Exaust*" = "StP_DSLPM",
              "Drinking Water Contaminents*" = "StP_DrinkWaterScore",
    "Waste Water Discharge Proximity*" = "StP_PWDIS",
             "Superfund Site Proximity*" = "StP_PNPL",
             "Hazardous Waste Landfill Proximity*" = "StP_PTSDF",
             "Risk Managment Plan Facility Proximity*" = "StP_PRMP") 


# Define UI for application 
ui <- bootstrapPage(

    #some css styles
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    #our backgrounds map
    leafletOutput("map", width = "60%", height = "60%"),
    #bottom panel 
    absolutePanel(id = "eval", 
                  fixed = TRUE, top = "auto", left = 0, right = "auto", bottom = 0,
                  width = "60%", height = "40%", style = "opacity: 0.92; overflow-y:scroll", 
                  fluidPage(fluidRow(
                      #column(1,h3("Evaluation Metrics")),
                      column(12,plotlyOutput("localChange3", height = 300, width = 900))))),
    
    #right side panel 
    absolutePanel(id = "controls", 
                  fixed = TRUE, top = 0, left = "auto", right = 0, bottom = 0,
                  width = "40%", height = "auto", style = "opacity: 0.92; overflow-y:scroll", 
                  wellPanel( id = "controls",
                             h1("NYenviroScreen"),
                             h2("A Data Driven Method for Designating NYS Potential Environmental Justice Areas"),
                             p(class = "text-muted",
                               paste("A test application designed by the Center for Environmental Medicine and Informatics (CEMI)",
                                     "at the SUNY College Of Environmental Science and Forestry"
                               ), 
                               selectizeInput("Allmets", "Map Environmental Justice Metrics",
                                              Allmets, selected = "2000 Potential EJ Areas", multiple = FALSE),
                               # Add links to indicators descriptions, github page, and documentation
                               h3("Build Your Own Metric"),
                               #SOCIO
                               fluidPage(fluidRow(
                                   column(8,selectizeInput("SocDem", "Sociodemographic Factors",
                                                           SocDem, selected = unique(SocDem), multiple = TRUE)),
                                   column(2,selectInput("SocDem_type", "Combination Method", ComboMeth, selected = "Mean", multiple = F)),
                                   column(2,numericInput("SocDem_Weight", "Weight", 100, min = 1, max = 100, step = 1)))),
                               fluidPage(fluidRow(
                                   column(8,selectizeInput("Sensa", "Sensative Population Indicators",
                                                           Sensa, selected = unique(Sensa), multiple = TRUE)),
                                   column(2,selectInput("Sensa_type", "Combination Method", ComboMeth, selected = "Mean", multiple = F)),
                                   column(2,numericInput("Sensa_Weight", "Weight", 100, min = 1, max = 100, step = 1)))),
                               fluidPage(fluidRow(
                                   column(8,selectizeInput("exposure", "Environmental Exposures",
                                                           exposure, selected = unique(exposure), multiple = TRUE)),
                                   column(2,selectInput("exposure_type", "Combination Method", ComboMeth, selected = "Mean", multiple = F)),
                                   column(2,numericInput("exposure_Weight", "Weight", 100, min = 1, max = 100, step = 1)))),
                               fluidPage(fluidRow(
                                   column(8,selectizeInput("effects", "Environemtnal Effects",
                                                           effects, selected = unique(effects), multiple = TRUE)),
                                   column(2,selectInput("effects_type", "Combination Method", ComboMeth, selected = "Mean", multiple = F)),
                                   column(2,numericInput("effects_Weight", "Weight", 100, min = 1, max = 100, step = 1)))),
                               fluidPage(fluidRow(
                                   column(4,numericInput("thres", "Percentile Threshold", 75, min = 1, max = 99, step = 1, width = 100)),
                                   column(4,checkboxInput("onlyPEJA", "Only Map Areas Over Threshold", FALSE)),
                                   column(4,actionButton("button_click", "Map Custom Metric"))))

                             ))))

server <- function(input, output, session) {
    
    #we want to do a few things, 
    #maps the areas, with outlines that indicate if the meet the threshold or not 
    #make a few charts for income and minority coverage 
    
    
    Myval <- eventReactive(input$button_click,{
        #exposure
        if(input$exposure_type == "Mean") {
            df <- df %>% 
                mutate(exposure1 = rowMeans(dplyr::select(., input$exposure),
                                            na.rm = T)*100)}
        if(input$exposure_type == "Sum") {
            df <- df %>%
                mutate(exposure1 = rowSums(dplyr::select(., input$exposure),
                                            na.rm = T)*100)}
        if(input$exposure_type == "Product") {
            df <- df %>%
                mutate(exposure1 = rowProds(dplyr::select(., input$exposure),
                                           na.rm = T)*100)}
        if(input$exposure_type == "Maximum") {
            df <- df %>%
                mutate(exposure1 = rowMaxs(dplyr::select(., input$exposure),
                                            na.rm = T)*100)}
        #SocDem
        if(input$SocDem_type == "Mean") {
            df <- df %>% 
                mutate(SocioEcon1 = rowMeans(dplyr::select(., input$SocDem),
                                            na.rm = T)*100)}
        if(input$SocDem_type == "Sum") {
            df <- df %>%
                mutate(SocioEcon1 = rowSums(dplyr::select(., input$SocDem),
                                           na.rm = T)*100)}
        if(input$SocDem_type == "Product") {
            df <- df %>%
                mutate(SocioEcon1 = rowProds(dplyr::select(., input$SocDem),
                                            na.rm = T)*100)}
        if(input$SocDem_type == "Maximum") {
            df <- df %>%
                mutate(SocioEcon1 = rowMaxs(dplyr::select(., input$SocDem),
                                           na.rm = T)*100)}
        #Sensitivepops
        if(input$Sensa_type == "Mean") {
            df <- df %>% 
                mutate(Sensitivepops1 = rowMeans(dplyr::select(., input$Sensa),
                                             na.rm = T)*100)}
        if(input$Sensa_type == "Sum") {
            df <- df %>%
                mutate(Sensitivepops1 = rowSums(dplyr::select(., input$Sensa),
                                            na.rm = T)*100)}
        if(input$Sensa_type == "Product") {
            df <- df %>%
                mutate(Sensitivepops1 = rowProds(dplyr::select(., input$Sensa),
                                             na.rm = T)*100)}
        if(input$Sensa_type == "Maximum") {
            df <- df %>%
                mutate(Sensitivepops1 = rowMaxs(dplyr::select(., input$Sensa),
                                            na.rm = T)*100)}
        #SocDem
        if(input$effects_type == "Mean") {
            df <- df %>% 
                mutate(effects1 = rowMeans(dplyr::select(., input$effects),
                                             na.rm = T)*100)}
        if(input$effects_type == "Sum") {
            df <- df %>%
                mutate(effects1 = rowSums(dplyr::select(., input$effects),
                                            na.rm = T)*100)}
        if(input$effects_type == "Product") {
            df <- df %>%
                mutate(effects1 = rowProds(dplyr::select(., input$effects),
                                             na.rm = T)*100)}
        if(input$effects_type == "Maximum") {
            df <- df %>%
                mutate(effects1 = rowMaxs(dplyr::select(., input$effects),
                                            na.rm = T)*100)}

        
        df$PopChar1 <- ((df$SocioEcon1*(input$SocDem_Weight/100) + df$Sensitivepops1*(input$Sensa_Weight/100))/((input$SocDem_Weight + input$Sensa_Weight)/100))
        df$PolBur1  <- ((df$exposure1*(input$exposure_Weight/100) + (df$effects1*(input$effects_Weight/100)))/((input$exposure_Weight + input$effects_Weight)/100))
        df$PolBurSc1 <- (df$PolBur1/max(df$PolBur1, na.rm = T))*10
        df$PopCharSc1 <- (df$PopChar1/max(df$PopChar1, na.rm = T))*10
        
        print(paste("POPchar: ", df$PopChar1[df$GEOID == 360679400001]))
        print(paste("POPchar_o: ", df$PopChar[df$GEOID == 360679400001]))
        print(paste("PolBur: ", df$PolBur1[df$GEOID == 360679400001]))
        print(paste("PolBur_o: ", df$PolBur[df$GEOID == 360679400001]))
        print(paste("POPchar_score: ", df$PopCharSc1[df$GEOID == 360679400001]))
        print(paste("POPchar_score_o: ", df$PopCharSc[df$GEOID == 360679400001]))
        print(paste("PolBur_score: ", df$PolBurSc1[df$GEOID == 360679400001]))
        print(paste("PolBur_score_o: ", df$PolBurSc[df$GEOID == 360679400001]))
        
        df$nyeScore1 <- df$PolBurSc1*df$PopCharSc1
        df$StP_nyeScore1 <- round(as.numeric(percent_rank(df$nyeScore1)*100),2)
        
        return(df)
        
    })
    
    Myval2 <- eventReactive(input$button_click,{
        #exposure
        if(input$exposure_type == "Mean") {
            df <- df %>% 
                mutate(exposure1 = rowMeans(dplyr::select(., input$exposure),
                                            na.rm = T)*100)}
        if(input$exposure_type == "Sum") {
            df <- df %>%
                mutate(exposure1 = rowSums(dplyr::select(., input$exposure),
                                           na.rm = T)*100)}
        if(input$exposure_type == "Product") {
            df <- df %>%
                mutate(exposure1 = rowProds(dplyr::select(., input$exposure),
                                            na.rm = T)*100)}
        if(input$exposure_type == "Maximum") {
            df <- df %>%
                mutate(exposure1 = rowMaxs(dplyr::select(., input$exposure),
                                           na.rm = T)*100)}
        #SocDem
        if(input$SocDem_type == "Mean") {
            df <- df %>% 
                mutate(SocioEcon1 = rowMeans(dplyr::select(., input$SocDem),
                                             na.rm = T)*100)}
        if(input$SocDem_type == "Sum") {
            df <- df %>%
                mutate(SocioEcon1 = rowSums(dplyr::select(., input$SocDem),
                                            na.rm = T)*100)}
        if(input$SocDem_type == "Product") {
            df <- df %>%
                mutate(SocioEcon1 = rowProds(dplyr::select(., input$SocDem),
                                             na.rm = T)*100)}
        if(input$SocDem_type == "Maximum") {
            df <- df %>%
                mutate(SocioEcon1 = rowMaxs(dplyr::select(., input$SocDem),
                                            na.rm = T)*100)}
        #Sensitivepops
        if(input$Sensa_type == "Mean") {
            df <- df %>% 
                mutate(Sensitivepops1 = rowMeans(dplyr::select(., input$Sensa),
                                                 na.rm = T)*100)}
        if(input$Sensa_type == "Sum") {
            df <- df %>%
                mutate(Sensitivepops1 = rowSums(dplyr::select(., input$Sensa),
                                                na.rm = T)*100)}
        if(input$Sensa_type == "Product") {
            df <- df %>%
                mutate(Sensitivepops1 = rowProds(dplyr::select(., input$Sensa),
                                                 na.rm = T)*100)}
        if(input$Sensa_type == "Maximum") {
            df <- df %>%
                mutate(Sensitivepops1 = rowMaxs(dplyr::select(., input$Sensa),
                                                na.rm = T)*100)}
        #SocDem
        if(input$effects_type == "Mean") {
            df <- df %>% 
                mutate(effects1 = rowMeans(dplyr::select(., input$effects),
                                           na.rm = T)*100)}
        if(input$effects_type == "Sum") {
            df <- df %>%
                mutate(effects1 = rowSums(dplyr::select(., input$effects),
                                          na.rm = T)*100)}
        if(input$effects_type == "Product") {
            df <- df %>%
                mutate(effects1 = rowProds(dplyr::select(., input$effects),
                                           na.rm = T)*100)}
        if(input$effects_type == "Maximum") {
            df <- df %>%
                mutate(effects1 = rowMaxs(dplyr::select(., input$effects),
                                          na.rm = T)*100)}
        
        
        df$PopChar1 <- ((df$SocioEcon1*(input$SocDem_Weight/100) + df$Sensitivepops1*(input$Sensa_Weight/100))/((input$SocDem_Weight + input$Sensa_Weight)/100))
        df$PolBur1  <- ((df$exposure1*(input$exposure_Weight/100) + (df$effects1*(input$effects_Weight/100)))/((input$exposure_Weight + input$effects_Weight)/100))
        df$PolBurSc1 <- (df$PolBur1/max(df$PolBur1, na.rm = T))*10
        df$PopCharSc1 <- (df$PopChar1/max(df$PopChar1, na.rm = T))*10
        
        df$nyeScore1 <- df$PolBurSc1*df$PopCharSc1
        df$StP_nyeScore1 <- round(as.numeric(percent_rank(df$nyeScore1)*100),2)
        dfs <- df %>% dplyr::select(9,38:62) %>%
            summarise_all(sum) %>% gather("Income Bracket",
                                          "Total NY House Holds 2018")
        dfsb <- df %>% filter(StP_nyeScore >= 75) %>% dplyr::select(9,38:62) %>%
            summarise_all(sum) %>% gather("Income Bracket",
                                          "NYeviroScreen Coverage")
        dfsc <- df %>% filter(StP_nyeScore1 >= input$thres) %>% dplyr::select(9,38:62) %>%
            summarise_all(sum) %>% gather("Income Bracket",
                                          "Custom Metric Coverage")
        dfsc1 <- df2k %>% mutate(population = totpop) %>%
            dplyr::select(51:71, 73,75,76) %>%
            summarise_all(sum, na.rm =T) %>% gather("Income Bracket",
                                                    "Total Households or Population 2000")
        dfsc2 <- df2k %>% filter(peja2k.1 == 1) %>% mutate(population = totpop) %>%
            dplyr::select(51:71, 73,75,76) %>%
            summarise_all(sum) %>% gather("Income Bracket",
                                          "2000 PEJA Coverage")
        
        dfs <- left_join(dfs, dfsb)
        dfs <- left_join(dfs, dfsc)
        dfs <- left_join(dfs, dfsc1)
        dfs <- left_join(dfs, dfsc2)
        
        
        return(dfs)
        
    })
    
    # Myval3 <- eventReactive(input$button_click,{
    #     
    #     df <- df %>%
    #         mutate(exposure1 = rowMeans(dplyr::select(., input$exposure),
    #                                     na.rm = T)*100,
    #                effects1 = rowMeans(dplyr::select(., input$effects),
    #                                    na.rm = T)*100,
    #                Sensitivepops1 = rowMeans(dplyr::select(., input$Sensa),
    #                                          na.rm = T)*100,
    #                SocioEcon1 = rowMeans(dplyr::select(.,input$SocDem),
    #                                      na.rm = T)*100) %>% ungroup()
    #     
    #     df$PopChar1 <- ((df$SocioEcon1 + df$Sensitivepops1)/2)
    #     df$PolBur1  <- ((df$exposure1 + (df$effects1/2))/1.5)
    #     df$PolBurSc1 <- (df$PolBur1/max(df$PolBur1, na.rm = T))*10
    #     df$PopCharSc1 <- (df$PopChar1/max(df$PopChar1, na.rm = T))*10
    #     
    #     df$nyeScore1 <- df$PolBurSc1*df$PopCharSc1
    #     
    #     df$StP_nyeScore1 <- round(as.numeric(percent_rank(df$nyeScore1)*100),2)
    #     print(names(df))
    #     dfs <- df %>% dplyr::select(39:45) %>%
    #         summarise_all(sum) %>% gather("Income Bracket",
    #                                       "Total NY House Holds 2018")
    #     dfsb <- df %>% filter(StP_nyeScore >= 75) %>% dplyr::select(39:45) %>%
    #         summarise_all(sum) %>% gather("Income Bracket",
    #                                       "NYeviroScreen Coverage")
    #     dfsc <- df %>% filter(StP_nyeScore1 >= input$thres) %>% dplyr::select(39:45) %>%
    #         summarise_all(sum) %>% gather("Income Bracket",
    #                                       "Custom Metric Coverage")
    #     
    #     dfs <- left_join(dfs, dfsb)
    #     dfs <- left_join(dfs, dfsc)
    #     
    #     
    #     return(dfs)
    #     
    # })
    
    
    output$map <- renderLeaflet({ 
        leaflet() %>% setView(lng = -75.5445, lat = 42.874, zoom = 7)  %>% 
            addTiles()
    })
    
    # InBounds <- reactive({
    #     if (is.null(input$map_bounds))
    #         return(us.e.dat[FALSE,])
    #     bounds <- input$map_bounds
    #     latRng <- range(bounds$north, bounds$south)
    #     lngRng <- range(bounds$east, bounds$west)
    #     
    #     subset(filteredtype(),
    #            Facility.Latitude >= latRng[1] & Facility.Latitude <= latRng[2] &
    #                Facility.Longitude >= lngRng[1] & Facility.Longitude <= lngRng[2])
    # })
    
    # table <- reactive({ renderDataTable(arrange(filteredtype(), desc(filteredtype()$so2Noxcost)), options = NULL, searchDelay = 500,
    #                                     callback = "function(oTable) {}", escape = TRUE, env = parent.frame(),
    #                                     quoted = FALSE, outputArgs = list())
    # })
    # 
    
    observe({

        if(input$button_click == F) {
            #if the custom metric has NOT been intiated    
        output$localChange3 <- renderPlotly({
            
            plot_ly(dfs, type = "bar",
                    x = ~`Income Bracket`,
                    y = ~(`NYeviroScreen Coverage`/`Total NY House Holds 2018`)*100,
                    name = "NYeviroScreen") %>%
                add_trace(y = ~(`2000 PEJA Coverage`/`Total Households or Population 2000`)*100,
                          name = "PEJA 2000") %>%
                layout(xaxis = list(title = 'Percent of Households or Population'), barmode = 'group',
                       yaxis = list(title = "",
                                    categoryorder = "array",
                                    categoryarray = ~`Income Bracket`))
            
            
            
            
        })   } else{
        #if the custom metric has been intiated 
        output$localChange3 <- renderPlotly({
            
            plot_ly(Myval2(), type = "bar",
                    x = ~`Income Bracket`,
                    y = ~(`NYeviroScreen Coverage`/`Total NY House Holds 2018`)*100,
                    name = "NYeviroScreen") %>%
                add_trace(y = ~(`2000 PEJA Coverage`/`Total Households or Population 2000`)*100,
                          name = "PEJA 2000") %>%
                add_trace(y = ~(`Custom Metric Coverage`/`Total NY House Holds 2018`)*100,
                          name = "Custom Metric") %>%
                layout(xaxis = list(title = 'Percent of House Holds or Population'), barmode = 'group',
                       yaxis = list(title = "",
                                    categoryorder = "array",
                                    categoryarray = ~`Income Bracket`))
            
            
            
            
        })   
        }
    })


    # output$localChange4 <- renderPlotly({
    #     
    #     plot_ly(Myval3(), type = "bar",
    #             y = ~`Income Bracket`,
    #             x = ~(`NYeviroScreen Coverage`/`Total NY House Holds 2018`)*100,
    #             name = "NYeviroScreen") %>%
    #         add_trace(x = ~(`Custom Metric Coverage`/`Total NY House Holds 2018`)*100,
    #                   name = "Custom Metric") %>%
    #         layout(xaxis = list(title = 'Percent of Population'), barmode = 'group',
    #                yaxis = list(title = "",
    #                             categoryorder = "array",
    #                             categoryarray = ~`Income Bracket`))
    #     
    #     
    #     
    #     
    # })
    
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
                addLegend("bottomleft", colors = "purple", labels = "2000 PEJA",
                          title = "2000 Potential Environmental Justice Areas", opacity = 1, layerId = "legend")
            
        } else {
        
        df2 <- df %>% dplyr::select(., GEOID, input$Allmets)
        names(df2) <- c("GEOID", "myval")
        ejshp <- merge(ejshp, df2, by = "GEOID", all.x = TRUE)

        pal <-  colorQuantile("viridis",  ejshp$myval)
        
        #### size by percent of total? ### might be a solution to the sizing problem...... looks great so far tho! 
        leafletProxy("map", data = ejshp) %>%
            clearShapes() %>%
            clearMarkers() %>%
            removeControl(layerId = "legend") %>%
            addPolygons(color = "#444444", weight = .2, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5,
                        fillColor = ~colorQuantile("viridis",  myval)(myval),
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE),
                        popup = ~paste(input$Allmets, myval, sep = ": ")) %>%                                                                            
            addLegend("bottomleft", pal = pal, values = ejshp$myval,
                      title = "NYenviroScreen Percentile", opacity = 1, layerId = "legend")
        }
        
    })
    
    observeEvent(input$button_click,{
        df1 <- Myval() 
        df1 <- df1 %>% dplyr::select(GEOID, StP_nyeScore1, nyeScore1, SocioEcon1,
                                     Sensitivepops1, exposure1,effects1)
        ejshp <- merge(ejshp, df1, by = "GEOID", all.x = TRUE)
        # pal <-  colorQuantile("viridis",  -ejshp$StP_nyeScore)
        
        if(input$onlyPEJA == T){
            ejshp <- ejshp[ejshp$StP_nyeScore1 >= input$thres, ]
        }
        
        myout <- df1 %>%
            filter(StP_nyeScore1 > input$thres)
        
        # output$population <- sum(myout$population, na.rm = T)

            #### size by percent of total? ### might be a solution to the sizing problem...... looks great so far tho! 
            leafletProxy("map", data = ejshp) %>%
                clearShapes() %>%
                clearMarkers() %>%
                removeControl(layerId = "legend") %>%
                addPolygons(color = "#444444", weight = .2, smoothFactor = 0.5,
                            opacity = 1.0, fillOpacity = 0.5,
                            fillColor = ~colorQuantile("viridis",  StP_nyeScore1)(StP_nyeScore1),
                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                bringToFront = TRUE),
                            popup = ~paste0("<b>Block Group: </b>", GEOID, "<br>",
                                            "<b><u>NYenviroScreen Score Percentile:</b></u> ", round(StP_nyeScore,2), "<br>",
                                            "<b>NYenviroScreen Score: </b>", round(nyeScore,2), "<br>",
                                            "<b>Sociodemographic Score: </b>", round(SocioEcon,2), "<br>",
                                            "<b>Sensative Population Score: </b>", round(Sensitivepops,2), "<br>",
                                            "<b>Environmental Exposure Score: </b>", round(exposure,2), "<br>",
                                            "<b>Environmental Effects Score: </b>", round(effects,2), "<br>",
                                            "<b><u>Custom Score Percentile:</b></u> ", round(StP_nyeScore1,2), "<br>",
                                            "<b>Custom Score:  </b>", round(nyeScore1,2), "<br>",
                                            "<b>Custom Sociodemographic Score: </b>", round(SocioEcon1,2), "<br>",
                                            "<b>Custom Sensative Population Score: </b>", round(Sensitivepops1,2), "<br>",
                                            "<b>Custom Environmental Exposure Score: </b>", round(exposure1,2), "<br>",
                                            "<b>Custom Environmental Effects Score: </b>", round(effects1,2))) %>%                                                                            
                addLegend("bottomleft", pal = colorQuantile("viridis",  ejshp$StP_nyeScore1), values = ejshp$StP_nyeScore1,
                          title = "NYenviroScreen Percentile", opacity = 1, layerId = "legend")

    })
    
}


shinyApp(ui, server)


