# maps 

#now lets map this shit

# some good mapping tools
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
# https://www.r-bloggers.com/inset-maps-with-ggplot2/

#which county has the largest change?

df$county <- substr(df$ID, 1, 5)
df <- left_join(df, countykey, by = c("county" = "GEOID"))

countytots <- df %>% group_by(county) %>% summarise(hybrid = sum(peja_18_hybrid, na.rm = T),
                                                    cal = sum(peja_18_cal, na.rm = T),
                                                    breaks = sum(peja_18_breaks, na.rm = T)) %>% ungroup()

countytots <- left_join(countytots, countykey, by = c("county" = "GEOID"))

#first lets look at the whole 
ejshp <- merge(nyblock_groups, df, by.x = "GEOID", by.y = "ID", all.x = TRUE)
ejshp_hybrid <- subset(ejshp, ejshp$peja_18_hybrid == 1)
library(leaflet)
leaflet(ejshp_hybrid) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = .2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("viridis",  -StP_nyeScore)(-StP_nyeScore),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>Block Group: </b>", GEOID, "<br>",
                              "<b><u>NYenviroScreen Score Percentile:</b></u> ", round(StP_nyeScore,2), "<br>",
                              "<b>NYenviroScreen Score: </b>", round(nyeScore,2), "<br>",
                              "<b>Sociodemographic Score: </b>", round(SocioEcon,2), "<br>",
                              "<b>Sensative Population Score: </b>", round(Sensitivepops,2), "<br>",
                              "<b>Environmental Exposure Score: </b>", round(exposure,2), "<br>",
                              "<b>Environmental Effects Score: </b>", round(effects,2), "<br>",
                              "<b>Health Score: </b>", round(Health,2), "<br>",
                              "<b>Urban: </b>", Urban_Area_50per, "<br>",
                              "<b>Tribe area or border: </b>", Tribal_Area_or_Border, "<br>",
                              "<b>Percent Minority: </b>", round(MINORPCT,2), "<br>",
                              "<b>Poverty Percent: </b>", round(per_poverty,2), "<br>"
              ))




#Richmond, Cattaraugus, Niagra 
Rich <- subset(ejshp, ejshp$`County Name` == "Richmond" & ejshp$ALAND > 0)
Rich00 <- subset(ej2k, ej2k$COUNTY == "085")
#ok lets go with a 4 by 4 design

#first box is going to be NYS with the inset box to show zoomed in area
#next three with be the new methods overlayed infront of the 2003 areas
Rich$GEOID
install.packages("tmap")
library(tmap)
myroads <- roads("New York", "085")
mainroads <- subset(myroads, myroads$RTTYP %in% c("I", "S", "O"))

Rich$peja_18_hybrid_cat <- ifelse(Rich$peja_18_hybrid == 1 & Rich$peja_18_breaks == 1 & 
                                    Rich$peja_18_cal == 1, "Both",
                                  ifelse(Rich$peja_18_breaks == 1 & Rich$peja_18_cal == 0, "Breaks",
                                         ifelse(Rich$peja_18_cal == 1 & Rich$peja_18_breaks == 0, "CalEPA", "No")))


map1 <- tm_shape(Rich) +
  tm_fill(col = "peja_18_hybrid_cat",
          style = "cat",
          title = "PEJA",
          alpha = 1,
          palette = c("blue", "green", "yellow", "tan", "grey")) +
  tm_borders() +
  # tm_compass(type = "8star", position = c("left", "middle")) + 
  tm_layout("2020 - Hybrid - Richmond County, NY",
            legend.position = c("left","top"),
            bg.color = "white",
            title.size = 1,
            inner.margin = .08,
            legend.title.size = 1)

map1

Rich00$peja_00_cat <- ifelse(Rich00$peja2k.1 == 1, "Yes", "No")
map2 <- tm_shape(Rich00) +
  tm_fill(col = "peja_00_cat",
          style = "cat",
          title = "PEJA",
          alpha = 1,
          palette = c("tan", "purple", "grey")) +
  tm_borders() +
  tm_layout("2000 - Richmond County, NY",
            legend.position = c("left","top"),
            bg.color = "white",
            title.size = 1,
            inner.margin = .08,
            legend.title.size = 1)
map2

Rich$peja_18_cal_cat <- ifelse(Rich$peja_18_cal == 1, "Yes", "No")
map3 <- tm_shape(Rich) +
  tm_fill(col = "peja_18_cal_cat",
          style = "cat",
          title = "PEJA",
          alpha = 1,
          palette = c("tan", "yellow", "grey")) +
  tm_borders() +
  # tm_compass(type = "8star", position = c("left", "middle")) + 
  tm_layout("2020 - CalEPA - Richmond County, NY",
            legend.position = c("left","top"),
            bg.color = "white",
            title.size = 1,
            inner.margin = .08,
            legend.title.size = 1)

map3

Rich$peja_18_breaks_cat <- ifelse(Rich$peja_18_breaks == 1, "Yes", "No")
map4 <- tm_shape(Rich) +
  tm_fill(col = "peja_18_breaks_cat",
          style = "cat",
          title = "PEJA",
          alpha = 1,
          palette = c("tan", "green", "grey")) +
  tm_borders() +
  # tm_compass(type = "8star", position = c("left", "middle")) + 
  tm_layout("2020 - Breaks - Richmond County, NY",
            legend.position = c("left","top"),
            bg.color = "white",
            title.size = 1,
            inner.margin = .08,
            legend.title.size = 1)

map4



#now for the inset map 
download.file("http://gis.ny.gov/gisdata/fileserver/?DSID=927&file=NYS_Civil_Boundaries.shp.zip",
              destfile = "data/NYS_Civil_Boundaries.shp.zip", mode="wb")
unzip("data/NYS_Civil_Boundaries.shp.zip", exdir = "./data/nyshp")

s <- shapefile("./data/NYS_Civil_Boundaries_SHP/State_Shorline.shp")
shapeny <- st_read(dsn = "./data/NYS_Civil_Boundaries_SHP", layer = "State_Shoreline")

nycounties <- st_read(dsn = "./data/NYS_Civil_Boundaries_SHP", layer = "Counties_Shoreline")
mycounty <- subset(nycounties, nycounties$NAME == "Richmond")

#for the box

bbox_new <- st_bbox(shapeny) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[1] <- bbox_new[1] + (0.725 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] - (0.31 * xrange) # xmax - right
bbox_new[2] <- bbox_new[2] + (0.00000001 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] - (0.955 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

# draw a prettier chart
tm_shape(hranice, bbox = bbox_new) + tm_polygons(col = "red") +
  tm_layout(title = "A Big, Fat Title...", frame = T, title.size = 5)

inset_m <- tm_shape(shapeny) +
  tm_fill(col = "grey") +
  tm_borders() +
  tm_shape(mycounty) +
  tm_fill(col = "red") +
  tm_borders() +
  tm_shape(bbox_new) +
  tm_borders(col = "red", lwd = 3) 
inset_m


library(grid)
tmap_arrange(map2, map3, map4, map1)
print(inset_m, vp = grid::viewport(0.4, 0.62, width = 0.18, height = 0.18))
#tm_scale_bar(breaks = c(0, 10, 20), text.size = 1)


#Cattaraugus
Rich <- subset(ejshp, ejshp$`County Name` %in% c("Niagara", "Orleans") & ejshp$ALAND > 0)
Rich00 <- subset(ej2k, ej2k$COUNTY %in% c("063", "073"))
#ok lets go with a 4 by 4 design
#first box is going to be NYS with the inset box to show zoomed in area
#next three with be the new methods overlayed infront of the 2003 areas
library(tmap)

Rich$peja_18_hybrid_cat <- ifelse(Rich$peja_18_hybrid == 1 & Rich$peja_18_breaks == 1 & 
                                    Rich$peja_18_cal == 1, "Both",
                                  ifelse(Rich$peja_18_breaks == 1 & Rich$peja_18_cal == 0, "Breaks",
                                         ifelse(Rich$peja_18_cal == 1 & Rich$peja_18_breaks == 0, "CalEPA", "No")))


map1 <- tm_shape(Rich) +
  tm_fill(col = "peja_18_hybrid_cat",
          style = "cat",
          title = "PEJA",
          alpha = 1,
          palette = c("blue", "green", "yellow", "tan", "grey")) +
  tm_borders() +
  # tm_compass(type = "8star", position = c("left", "middle")) + 
  tm_layout("2020 - Hybrid - Niagara & Orleans Counties, NY",
            legend.position = c("left","top"),
            bg.color = "white",
            title.size = 1,
            inner.margin = .08,
            legend.title.size = 1)

map1

Rich00$peja_00_cat <- ifelse(Rich00$peja2k.1 == 1, "Yes", "No")
map2 <- tm_shape(Rich00) +
  tm_fill(col = "peja_00_cat",
          style = "cat",
          title = "PEJA",
          alpha = 1,
          palette = c("tan", "purple", "grey")) +
  tm_borders() +
  tm_layout("2000 - Niagara & Orleans, NY",
            legend.position = c("left","top"),
            bg.color = "white",
            title.size = 1,
            inner.margin = .08,
            legend.title.size = 1)
map2

Rich$peja_18_cal_cat <- ifelse(Rich$peja_18_cal == 1, "Yes", "No")
map3 <- tm_shape(Rich) +
  tm_fill(col = "peja_18_cal_cat",
          style = "cat",
          title = "PEJA",
          alpha = 1,
          palette = c("tan", "yellow", "grey")) +
  tm_borders() +
  # tm_compass(type = "8star", position = c("left", "middle")) + 
  tm_layout("2020 - CalEPA - Niagara & Orleans, NY",
            legend.position = c("left","top"),
            bg.color = "white",
            title.size = 1,
            inner.margin = .08,
            legend.title.size = 1)

map3

Rich$peja_18_breaks_cat <- ifelse(Rich$peja_18_breaks == 1, "Yes", "No")
map4 <- tm_shape(Rich) +
  tm_fill(col = "peja_18_breaks_cat",
          style = "cat",
          title = "PEJA",
          alpha = 1,
          palette = c("tan", "green", "grey")) +
  tm_borders() +
  # tm_compass(type = "8star", position = c("left", "middle")) + 
  tm_layout("2020 - Breaks - Niagara & Orleans, NY",
            legend.position = c("left","top"),
            bg.color = "white",
            title.size = 1,
            inner.margin = .08,
            legend.title.size = 1)

map4



#now for the inset map 
mycounty <- subset(nycounties, nycounties$NAME %in% c("Niagara", "Orleans"))

#for the box

bbox_new <- st_bbox(shapeny) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[1] <- bbox_new[1] + (0.08 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] - (0.75 * xrange) # xmax - right
bbox_new[2] <- bbox_new[2] + (0.55 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] - (0.32 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

# draw a prettier chart
inset_m <- tm_shape(shapeny) +
  tm_fill(col = "grey") +
  tm_shape(mycounty) +
  tm_fill(col = "tan") +
  tm_borders() +
  tm_shape(bbox_new) +
  tm_borders(col = "red", lwd = 3) 
inset_m


library(grid)
tmap_arrange(map2, map1)
print(inset_m, vp = grid::viewport(0.89, 0.605, width = 0.18, height = 0.18))
#tm_scale_bar(breaks = c(0, 10, 20), text.size = 1)

#####################################################################################

# first map of the state top half and bottom half

mycounty <- subset(nycounties, nycounties$NAME %in% c("Ulster", "Sullivan",
                                                      "Dutchess", "Orange",
                                                      "Putnam", "Westchester",
                                                      "Nassau", "Queens",
                                                      "Kings", "Suffolk",
                                                      "Richmond", "New York",
                                                      "Bronx", "Rockland"))


Rich <- subset(ejshp, ejshp$`County Name` %in% c("Ulster", "Sullivan",
                                                 "Dutchess", "Orange",
                                                 "Putnam", "Westchester",
                                                 "Nassau", "Queens",
                                                 "Kings", "Suffolk",
                                                 "Richmond", "New York",
                                                 "Bronx", "Rockland") & ejshp$ALAND > 0)
Rich00 <- subset(ej2k, ej2k$COUNTY %in% unique(substr(Rich$GEOID, 3, 5)))
#ok lets go with a 4 by 4 design
#first box is going to be NYS with the inset box to show zoomed in area
#next three with be the new methods overlayed infront of the 2003 areas
library(tmap)

Rich$peja_18_hybrid_cat <- ifelse(Rich$peja_18_cal == 1 & Rich$peja_18_breaks == 1, "Both Models",
                                  ifelse(Rich$peja_18_breaks == 1 & Rich$peja_18_cal == 0, "Natural Breaks",
                                         ifelse(Rich$peja_18_cal == 1 & Rich$peja_18_breaks == 0, "CalEPA",
                                                ifelse(!is.na(Rich$Tribal_Area_or_Border), "Inside or Adjacent to Tribe Area",
                                                       "No"))))
Rich <- subset(Rich, Rich$peja_18_hybrid_cat %in% c("Both Models", "CalEPA", "Natural Breaks", "Inside or Adjacent to Tribe Area"))

unique(Rich$peja_18_hybrid_cat)
map1a <-   tm_shape(mycounty) +
  tm_fill(col = "#f2f2f2") +
tm_borders() + tm_shape(Rich) +
  tm_fill(col = "peja_18_hybrid_cat",
          style = "cat",
          title = "Legend - PEJA Type",
          alpha = 1,
          palette = c('#33a02c', "#b2df8a", "#1f78b4", "#a6cee3")) +
  # tm_compass(type = "8star", position = c("left", "middle")) + 
  tm_layout("(b)",
            legend.position = c("left","bottom"),
            bg.color = "white",
            title.size = 1,
            inner.margin = .08,
            legend.title.size = 1)


Rich00$peja_00_cat <- ifelse(Rich00$peja2k.1 == 1, "2003 PEJA", "No")
Rich00 <- subset(Rich00, Rich00$peja_00_cat %in% c("2003 PEJA"))

map2 <- tm_shape(mycounty) +
  tm_fill(col = "#f2f2f2") +
  tm_borders() + tm_shape(Rich00) +
  tm_fill(col = "peja_00_cat",
          style = "cat",
          title = "Legend - PEJA Type",
          alpha = 1,
          palette = c("#8073ac")) +
  tm_layout("(a)",
            legend.position = c("left","bottom"),
            bg.color = "white",
            title.size = 1,
            inner.margin = .08,
            legend.title.size = 1)


#for the box

bbox_new <- st_bbox(shapeny) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[1] <- bbox_new[1] + (0.57 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] - (0.000000000000001 * xrange) # xmax - right
bbox_new[2] <- bbox_new[2] + (0.000000000000001 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] - (0.57 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

# draw a prettier chart
inset_m <- tm_shape(shapeny) +
  tm_fill(col = "grey") +
  tm_shape(mycounty) +
  tm_fill(col = "#f2f2f2") +
  tm_borders() +
  tm_shape(bbox_new) +
  tm_borders(col = "red", lwd = 3) 
inset_m


library(grid)
tmap_arrange(map2, map1a)
print(inset_m, vp = grid::viewport(0.89, 0.605, width = 0.3, height = 0.3))


#####################################################################################

# first map of the state top half and bottom half

mycounty <- subset(nycounties, !nycounties$NAME %in% c("Ulster", "Sullivan",
                                                      "Dutchess", "Orange",
                                                      "Putnam", "Westchester",
                                                      "Nassau", "Queens",
                                                      "Kings", "Suffolk",
                                                      "Richmond", "New York",
                                                      "Bronx", "Rockland"))


Rich <- subset(ejshp, !ejshp$`County Name` %in% c("Ulster", "Sullivan",
                                                 "Dutchess", "Orange",
                                                 "Putnam", "Westchester",
                                                 "Nassau", "Queens",
                                                 "Kings", "Suffolk",
                                                 "Richmond", "New York",
                                                 "Bronx", "Rockland") & ejshp$ALAND > 0)
Rich00 <- subset(ej2k, ej2k$COUNTY %in% unique(substr(Rich$GEOID, 3, 5)))
#ok lets go with a 4 by 4 design
#first box is going to be NYS with the inset box to show zoomed in area
#next three with be the new methods overlayed infront of the 2003 areas
library(tmap)

Rich$peja_18_hybrid_cat <- ifelse(Rich$peja_18_cal == 1 & Rich$peja_18_breaks == 1, "Both Models",
                                  ifelse(Rich$peja_18_breaks == 1 & Rich$peja_18_cal == 0, "Natural Breaks",
                                         ifelse(Rich$peja_18_cal == 1 & Rich$peja_18_breaks == 0, "CalEPA",
                                                ifelse(!is.na(Rich$Tribal_Area_or_Border), "Inside or Adjacent to Tribe Area",
                                                       "No"))))
Rich <- subset(Rich, Rich$peja_18_hybrid_cat %in% c("Both Models", "CalEPA", "Natural Breaks", "Inside or Adjacent to Tribe Area"))

#unique(Rich$peja_18_hybrid_cat)
map1a <-   tm_shape(mycounty) +
  tm_fill(col = "#f2f2f2") +
  tm_borders() + tm_shape(Rich) +
  tm_fill(col = "peja_18_hybrid_cat",
          style = "cat",
          title = "Legend - PEJA Type",
          alpha = 1,
          palette = c('#33a02c', "#b2df8a", "#1f78b4", "#a6cee3")) +
  # tm_compass(type = "8star", position = c("left", "middle")) + 
  tm_layout("(b)",
            legend.position = c("left","top"),
            bg.color = "white",
            title.size = 1,
            inner.margin = .08,
            legend.title.size = 1)


Rich00$peja_00_cat <- ifelse(Rich00$peja2k.1 == 1, "2003 PEJA", "No")
Rich00 <- subset(Rich00, Rich00$peja_00_cat %in% c("2003 PEJA"))

map2 <- tm_shape(mycounty) +
  tm_fill(col = "#f2f2f2") +
  tm_borders() + tm_shape(Rich00) +
  tm_fill(col = "peja_00_cat",
          style = "cat",
          title = "Legend - PEJA Type",
          alpha = 1,
          palette = c("#8073ac")) +
  tm_layout("(a)",
            legend.position = c("left","top"),
            bg.color = "white",
            title.size = 1,
            inner.margin = .08,
            legend.title.size = 1)


#for the box

bbox_new <- st_bbox(shapeny) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[1] <- bbox_new[1] - (0.01 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] - (0.15 * xrange) # xmax - right
bbox_new[2] <- bbox_new[2] + (0.285 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.01 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

# draw a prettier chart
inset_m <- tm_shape(shapeny) +
  tm_fill(col = "grey") +
  tm_shape(mycounty) +
  tm_fill(col = "#f2f2f2") +
  tm_borders() +
  tm_shape(bbox_new) +
  tm_borders(col = "red", lwd = 3) 
inset_m


library(grid)
tmap_arrange(map2, map1a)
print(inset_m, vp = grid::viewport(0.89, 0.605, width = 0.3, height = 0.3))















