## Activity 6 ##

## Introduction to the problem: glacial retreat and vegetation change ##

## Set up spatial packages ##
install.packages(c("sp","rgdal","dplyr"))

# package for vector data
library(sp)
# package for reading in spatial data
library(rgdal)
# data management package
library(dplyr)

## Reading in vector data ##
# read in shapefiles
# readOGR in rgdal does this
# glaciers in 1966
g1966 <- readOGR("/Users/JonBeniers/Desktop/ENVST206/Activity 6 Data/a06/GNPglaciers/GNPglaciers_1966.shp")

# glaciers in 2015
g2015 <- readOGR("/Users/JonBeniers/Desktop/ENVST206/Activity 6 Data/a06/GNPglaciers/GNPglaciers_2015.shp")

str(g2015)

# map of glaciers: filling in the polygons with light blue and making the borders grey
plot(g1966, col = "lightblue2", border="grey50")

# data stores all accompanying info/measurements for each spatial object
head(g2015@data)

g1966@proj4string

## Question 1 ## (Answered in word doc using above code)

# check glacier names
g1966@data$GLACNAME

g2015@data$GLACNAME


# fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

## Visualizing glacial retreat ##
## Question 2 ## (Answered in word doc using provided image)
## Question 3 ## (Answered in word doc using provided image)
## Question 4 ## (Answered in word doc using provided image - zoomed-in)

## Vector data analysis: glacier retreat ##
# combining area, first work with a smaller data frame
gdf66 <- data.frame(GLACNAME = g1966@data$GLACNAME,
                    area66 = g1966@data$Area1966)

gdf15 <- data.frame(GLACNAME = g2015@data$GLACNAME,
                    area15 = g2015@data$Area2015)

# join all data tables by glacier name
gAll <- full_join(gdf66,gdf15, by="GLACNAME")

## Question 5 ## (Answered in word doc using code above)
## Question 6 ## (Answered in word doc using the dplyr cheat sheet & image)

# calculate the % change in area from 1966 to 2015
gAll$gdiff <- ((gAll$area66-gAll$area15)/gAll$area66)*100

## Question 7 ## (Answered in word doc using code below)
# scatterplot of glacier area in 1966 vs. percent change in area
plot(x=gdf66$area66, y=gAll$gdiff,
     xlab="Glacier Area in 1966 (square km)",
     ylab="Percent Change in Area (square km)",
     pch=19)


# map with color code for % glacial loss between 2015 and 1966. 
# join data with the spatial data table and overwrite into spatial data table 
g1966@data <- left_join(g1966@data, gAll, by="GLACNAME")
# use spplot to shade polygons based on the % change of labels
# first argument is the spatial object
# second is the column in of data to display with the different colors
# add a title using main
# col changes the color of the borders. This argument sets them to transparent
spplot(g1966, "gdiff", main="% change in area", col="transparent")

# look at the Vulture glacier in 1966
vulture66 <- g1966[g1966@data$GLACNAME == "Vulture Glacier",]
plot(vulture66, main = "Vulture Glacier in 1966", col="slategray")


## Question 8 ## (Answered in word doc using code below)
# mean of the % loss
mean(gAll$gdiff, trim = 0, na.rm = FALSE)

# standard deviation of the % loss
sd(gAll$gdiff, na.rm = FALSE)

#  glacier in 1966 with the largest area
gAll[gAll$gdiff == max(gAll$gdiff),]
# max(gAll$gdiff)

#  glacier in 1966 with the smallest area
gAll[gAll$gdiff == min(gAll$gdiff),]
# min(gAll$gdiff)

# glacier in 1966 with the smallest area &  percentage change (found manually)
# glacier in 1966 with the largest area &  percentage change (found manually)

# map with color code for % glacial loss between 2015 and 1966. 
g1966@data <- left_join(g1966@data, gAll, by="GLACNAME")
# spplot color coded for the % glacial loss between 2015 and 1966
spplot(g1966, "gdiff", main="% change in area", col="transparent")

## Question 9 ## (Answered in word doc using code below)

# map of glacier footprints in 1966 & 2015 for the glacier with largest % loss.
# look at the "Boulder Glacier"
boulder66 <- g1966[g1966@data$GLACNAME == "Boulder Glacier",]
plot(boulder66, main = "Boulder Glacier in 1966", col="slategray")

boulder15 <- g2015[g2015@data$GLACNAME == "Boulder Glacier",]
plot(boulder15, main = "Boulder Glacier in 2015", col="slategray")

plot(boulder66, col="dodgerblue3")
plot(boulder15, col="tomato3", add=TRUE)
title(main = "Boulder Glacier Footprints in 1966 vs. 2015")
# add legend
legend("topleft",
       c("Boulder Glacier in 1966", "Boulder Glacier in 2015"),
       col= c("dodgerblue3", "tomato3"),
       lwd=5, 
       bty="n")

# map of glacier footprints in 1966 & 2015 for the glacier with smallest % loss.
# look at the “Pumpelly Glacier”
pumpelly66 <- g1966[g1966@data$GLACNAME == "Pumpelly Glacier",]
plot(pumpelly66, main = "Pumpelly Glacier in 1966", col="slategray")

pumpelly15 <- g2015[g2015@data$GLACNAME == "Pumpelly Glacier",]
plot(pumpelly15, main = "Pumpelly Glacier in 2015", col="slategray")

plot(pumpelly66, col="gold1")
plot(pumpelly15, col="darkviolet", add=TRUE)
title(main = "Pumpelly Glacier Footprints in 1966 vs. 2015")
# add legend
legend("topleft",
       c("Pumpelly Glacier in 1966", "Pumpelly Glacier in 2015"),
       col= c("gold1", "darkviolet"),
       lwd=5, 
       bty="n")