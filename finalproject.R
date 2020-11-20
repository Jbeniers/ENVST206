## Jon Beniers ##
## 11/20/20 ##
## Final Project ##
## Analyzing the Effects of Drought: ##
## A Time Series Analysis of Agricultural Lands in Oneida County via Geospatial and Drought Data ##

install.packages(c("raster"))
install.packages("lubridate")

library(raster)
library(ggplot2)
library(rgdal)
library(lubridate)

## CUGIR - Agricultural Districts, Oneida County NY, 2019 ##
CUGIR <- readOGR("/Users/JonBeniers/Desktop/ENVST206/Final Project Data/cugir-007975/agONEI2019.shp")

plot(CUGIR)

## U.S. Drought Monitor drought threshold data 2000-2020 ##
# D0 - Abnormally Dry (23 instances)
# D1 - Moderate Drought (7 instances)
# D2 - Severe Drought (0 instances)
# D3 - Extreme Drought (0 instances)
# D4 - Exceptional Drought (0 instances) 
NYDroughtD0 <- read.csv("/Users/JonBeniers/Desktop/ENVST206/Final Project Data/dm_export_NY_20000101_20200101.csv")
NYDroughtD1 <- read.csv("/Users/JonBeniers/Desktop/ENVST206/Final Project Data/dm_export_NY_20000101_20200101 (1).csv")
NYDroughtD2 <- read.csv("/Users/JonBeniers/Desktop/ENVST206/Final Project Data/dm_export_NY_20000101_20200101 (2).csv")
NYDroughtD3 <- read.csv("/Users/JonBeniers/Desktop/ENVST206/Final Project Data/dm_export_NY_20000101_20200101 (3).csv")
NYDroughtD4 <- read.csv("/Users/JonBeniers/Desktop/ENVST206/Final Project Data/dm_export_NY_20000101_20200101 (4).csv")                         

##### TEST 1 #####
## Oneida County in NYDroughtD0 ##
D0_Oneida <- NYDroughtD0[NYDroughtD0$County == "Oneida County", ]

D0_Oneida$startD <- as.Date(D0_Oneida$StartDate,"%Y-%m-%d")
D0_Oneida$Year <- year(D0_Oneida$StartDate)
D0_Oneida$DOY <- yday(D0_Oneida$StartDate)

# leap year = true
# use for regression
D0_Oneida$DD <- D0_Oneida$Year+ ((D0_Oneida$DOY - 1) / ifelse(leap_year(D0_Oneida$Year),366, 365))

## Drought Threshold D0 - Abnormally Dry ##
# plot the data using ggplot --> ggplot(x-axis, y-axis)
ggplot(data = D0_Oneida, aes(x=startD,y=ConsecutiveWeeks))+# data for plot
        geom_point()+ # make points at data point
        labs(x="Date of Drought Experienced", y="# of Consecutive Weeks in Abnormally Dry for Oneida")
        
## setting up regression for Drought Threshold D0 ##
D0_Oneida$DDC <- (D0_Oneida$DD - 2000)
NYD0.mod <- lm(D0_Oneida$ConsecutiveWeeks ~ D0_Oneida$DDC)

summary(NYD0.mod)

# standardized residuals
NYD0.res <- rstandard(NYD0.mod)

## checking assumptions ##
## NOTE: Assumptions -> residuals are normally distributed & equal variances ##
# set up qq plot
qqnorm(NYD0.res)
# add qq line
qqline(NYD0.res)

# make residual plot
## NO REAL PATTERNS/TRENDS = pass the equal variances test ##
plot(D0_Oneida$DDC, NYD0.res,
     xlab = "Drought Threshold D0 - Abnormally Dry", 
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)

## Interpreting results ##
summary(NYD0.mod)

# make plot drought threshold D0 and consecutive weeks of drought observed
plot(D0_Oneida$DDC, D0_Oneida$ConsecutiveWeeks, 
     pch = 19, 
     col = "royalblue4",
     ylab = "# of Consecutive Weeks in Abnormally Dry for Oneida",
     xlab =  "Date of Drought Experienced (2000-2020)")
# add regression line
# make line width thicker
abline(NYD0.mod, lwd=2)



##### TEST 2 #####
## Oneida County from NYDroughtD1 ##
D1_Oneida <- NYDroughtD1[NYDroughtD1$County == "Oneida County", ]

D1_Oneida$startD <- as.Date(D1_Oneida$StartDate,"%Y-%m-%d")
D1_Oneida$Year <- year(D1_Oneida$StartDate)
D1_Oneida$DOY <- yday(D1_Oneida$StartDate)

# leap year = true
# use for regression
D1_Oneida$DD <- D1_Oneida$Year+ ((D1_Oneida$DOY - 1) / ifelse(leap_year(D1_Oneida$Year),366, 365))


## Drought Threshold D1 - Moderate Drought ##
# plot the data using ggplot --> ggplot(x-axis, y-axis)
ggplot(data = D1_Oneida, aes(x=startD,y=ConsecutiveWeeks))+# data for plot
        geom_point()+ # make points at data point
        labs(x="Date of Drought Experienced", y="# of Consecutive Weeks in Moderate Drought for Oneida")


## setting up regression for Drought Threshold D1 ##
D1_Oneida$DDC <- (D1_Oneida$DD - 2000)
NYD1.mod <- lm(D1_Oneida$ConsecutiveWeeks ~ D1_Oneida$DDC)

summary(NYD1.mod)

# standardized residuals
NYD1.res <- rstandard(NYD1.mod)

## checking assumptions ##
## NOTE: Assumptions -> residuals are normally distributed & equal variances ##
# set up qq plot
qqnorm(NYD1.res)
# add qq line
qqline(NYD1.res)

# make residual plot
## NO REAL PATTERNS/TRENDS = pass the equal variances test ##
plot(D1_Oneida$DDC, NYD1.res,
     xlab = "Drought Threshold D1 - Moderate Drought", 
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)


## Interpreting results ##
summary(NYD1.mod)

# make plot drought threshold D1 and consecutive weeks of drought observed
plot(D1_Oneida$DDC, D1_Oneida$ConsecutiveWeeks, 
     pch = 19, 
     col = "royalblue4",
     ylab = "# of Consecutive Weeks in Moderate Drought for Oneida",
     xlab =  "Date of Drought Experienced (2000-2020)")
# add regression line
# make line width thicker
abline(NYD0.mod, lwd=2)