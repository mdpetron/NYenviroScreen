
#state map 
library(tmap)
library(sf)

### pre-process and currate data

#get county shapes 

# download.file("http://gis.ny.gov/gisdata/fileserver/?DSID=927&file=NYS_Civil_Boundaries.shp.zip",
#               destfile = "data/NYS_Civil_Boundaries.shp.zip", mode="wb")
# unzip("data/NYS_Civil_Boundaries.shp.zip", exdir = "./data/nyshp")

#read them into workspace

#state shape
shapeny <- st_read(dsn = "./data/NYS_Civil_Boundaries_SHP", layer = "State_Shoreline")

#counties 
nycounties <- st_read(dsn = "./data/NYS_Civil_Boundaries_SHP", layer = "Counties_Shoreline")

#get county names in the ejshp spdataframe 


countykey <- countykey %>%
  mutate(FIPS_CODE = substr(GEOID, 3,5))


ejshp <- merge(ejshp, countykey, by.x = "COUNTYFP", by.y = "FIPS_CODE")

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
Rich00 <- subset(ej2k, ej2k$COUNTY %in% unique(Rich$COUNTYFP))
#ok lets go with a 4 by 4 design
#first box is going to be NYS with the inset box to show zoomed in area
#next three with be the new methods overlayed infront of the 2003 areas

Rich$peja_18_hybrid_cat <- ifelse(Rich$peja_18_cal == 1 & Rich$peja_18_breaks == 1, "Both 2020 Models",
                                  ifelse(Rich$peja_18_breaks == 1 & Rich$peja_18_cal == 0, "2020 Cluster Threshold Model",
                                         ifelse(Rich$peja_18_cal == 1 & Rich$peja_18_breaks == 0, "2020 Cumulative Impact Model",
                                                ifelse(!is.na(Rich$Tribal_Area_or_Border), "Inside or Adjacent to Tribe Area Only",
                                                       "No"))))
Rich <- subset(Rich, Rich$peja_18_hybrid_cat %in% c("Both 2020 Models", "2020 Cumulative Impact Model",
                                                    "2020 Cluster Threshold Model", "Inside or Adjacent to Tribe Area Only"))

#unique(Rich$peja_18_hybrid_cat)
map1a   <-   tm_shape(mycounty) +
  tm_fill(col = "#f2f2f2") +
  tm_borders() + tm_shape(Rich) +
  tm_fill(col = "peja_18_hybrid_cat",
          style = "cat",
          title = "Legend: PEJA Identified by",
          alpha = 1,
          palette = c("#8073ac", "#ffe75e",  '#de7119', "#14b1ab")) +
  # tm_compass(type = "8star", position = c("left", "middle")) + 
  tm_layout("(b)",
            legend.position = c("left","top"),
            bg.color = "white",
            title.size = 2,
            inner.margin = .08,
            legend.text.size = 1.4,
            legend.title.size = 2)

map1a


Rich00$peja_00_cat <- ifelse(Rich00$peja2k.1 == 1, "2003 Cluster Threshold Model", "No")
Rich00 <- subset(Rich00, Rich00$peja_00_cat %in% c("2003 Cluster Threshold Model"))

map2 <- tm_shape(mycounty) +
  tm_fill(col = "#f2f2f2") +
  tm_borders() + tm_shape(Rich00) +
  tm_fill(col = "peja_00_cat",
          style = "cat",
          title = "Legend: PEJA Identified by",
          alpha = 1,
          palette = c("#8073ac")) +
  tm_layout("(a)",
            legend.position = c("left","top"),
            bg.color = "white",
            title.size = 2,
            inner.margin = .08,
            legend.text.size = 1.4,
            legend.title.size = 2)


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


#####

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
Rich00 <- subset(ej2k, ej2k$COUNTY %in% unique(Rich$COUNTYFP))
#ok lets go with a 4 by 4 design
#first box is going to be NYS with the inset box to show zoomed in area
#next three with be the new methods overlayed infront of the 2003 areas

Rich$peja_18_hybrid_cat <- ifelse(Rich$peja_18_cal == 1 & Rich$peja_18_breaks == 1, "Both 2020 Models",
                                  ifelse(Rich$peja_18_breaks == 1 & Rich$peja_18_cal == 0, "2020 Cluster Threshold Model",
                                         ifelse(Rich$peja_18_cal == 1 & Rich$peja_18_breaks == 0, "2020 Cumulative Impact Model",
                                                ifelse(!is.na(Rich$Tribal_Area_or_Border), "Inside or Adjacent to Tribe Area Only",
                                                       "No"))))
Rich <- subset(Rich, Rich$peja_18_hybrid_cat %in% c("Both 2020 Models", "2020 Cumulative Impact Model",
                                                    "2020 Cluster Threshold Model", "Inside or Adjacent to Tribe Area Only"))

#unique(Rich$peja_18_hybrid_cat)
map1a   <-   tm_shape(mycounty) +
  tm_fill(col = "#f2f2f2") +
  tm_borders() + tm_shape(Rich) +
  tm_fill(col = "peja_18_hybrid_cat",
          style = "cat",
          title = "Legend: PEJA Identified by",
          alpha = 1,
          palette = c("#8073ac", "#ffe75e",  '#de7119', "#14b1ab")) +
  # tm_compass(type = "8star", position = c("left", "middle")) + 
  tm_layout("(b)",
            legend.position = c("right","top"),
            bg.color = "white",
            title.size = 2,
            inner.margin = .08,
            legend.text.size = 1.4,
            legend.title.size = 2)

map1a


Rich00$peja_00_cat <- ifelse(Rich00$peja2k.1 == 1, "2003 Cluster Threshold Model", "No")
Rich00 <- subset(Rich00, Rich00$peja_00_cat %in% c("2003 Cluster Threshold Model"))

map2 <- tm_shape(mycounty) +
  tm_fill(col = "#f2f2f2") +
  tm_borders() + tm_shape(Rich00) +
  tm_fill(col = "peja_00_cat",
          style = "cat",
          title = "Legend: PEJA Identified by",
          alpha = 1,
          palette = c("#8073ac")) +
  tm_layout("(a)",
            legend.position = c("right","top"),
            bg.color = "white",
            title.size = 2,
            inner.margin = .08,
            legend.text.size = 1.4,
            legend.title.size = 2)



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























