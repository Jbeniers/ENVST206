## Activity 5 - Intro to data visualizations ##

## installing package ##
# install.packages("ggplot2")

## run library at beginning of code ##
library(ggplot2)

## Fundamentals of plotting data ##

# read in weather station file from the data folder
datW <- read.csv("/Users/JonBeniers/Desktop/ENVST206/Activity 5 Data/noaa2011124.csv")

# specify that the name column should be a factor
datW$NAME<- as.factor(datW$NAME)

# set up a vector of all names for each level
nameS <- levels(datW$NAME)
nameS

# nameS[2] - can address a specific weather station

# make a dataframe with just precipitation, year, and site name
# remove NA using na.omit
datP <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           PRCP=datW$PRCP))

# total annual precipitation (mm)
precip <- aggregate(datW$PRCP, by=list(datW$NAME,datW$year), FUN="sum", na.rm=TRUE)

# use aggregate to get total annual precipitation
precip <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="sum", na.rm=TRUE)

# rename columns
colnames(precip) <- c("NAME","year","totalP")

# add the x column from aggregate looking at the length of observations in each year
precip$ncount <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="length")$x

## Question 1 ## (Answered using code above)

# make a new dataframe
pr <- precip[precip$ncount >=364, ]

# look at only livermore california and morrisville new york preciptiation
ca <- pr[pr$NAME == nameS[2], ]
ny <- pr[pr$NAME == nameS[5], ]

# make a plot of california precip
plot(ca$year, ca$totalP)

# make a plot of california precip
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year")

# make a plot of california precip
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n")
# add y axis
# arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
# las = 2 changes the labels to be read in horizontal direction
axis(2, seq(200,800, by=200), las=2 )

# adding New York precip to the plot with california precip
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n")
# add y axis
axis(2, seq(200,800, by=200), las=2 )
# add arizona
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")

# fixing the axes range so that all observations are visible for both sites
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(0, 1600))
# add y axis
axis(2, seq(0,1600, by=400), las=2 )
# add arizona
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")

# adding a legend to properly label the plot
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(0, 1600))
# add y axis
axis(2, seq(0,1600, by=400), las=2 )
# add arizona
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")
# add legend

legend("topleft", # position
       c("California", "New York"), # labels
       col= c("black", "tomato3"), # colors
       pch=19, # point shape
       lwd=1, # line thickness 1, anytime both point & line arguments are given both will be drawn
       bty="n") # always use this argument otherwise an ugly box is drawn

## Question 2 ## (Answered using code above)

## Question 3 ## (Answered using code below)

# make a dataframe with just maximum temperature, year, and site name
datT <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           TMAX=datW$TMAX))

#use aggregate to get mean annual maximum temperature
tmax <- aggregate(datT$TMAX, by=list(datT$NAME,datT$year), FUN="mean", na.rm=TRUE)

# rename columns
colnames(tmax) <- c("NAME","year","Tmax")

# add the x column from aggregate looking at the length of observations in each year
tmax$ncount <- aggregate(datT$TMAX, by=list(datT$NAME,datT$year), FUN="length")$x

#make a new dataframe
tm <- tmax[tmax$ncount >=364, ]

#look at morrisville new york and mandan north dakota mean annual max temp
ny_tm <- tm[tm$NAME == nameS[5], ]
nd_tm <- tm[tm$NAME == nameS[3], ]

# graph for morrisville ny and mandan nd
# add north dakota
plot(nd_tm$year, nd_tm$Tmax,
     type = "b",
     pch = 17,
     col ="dodgerblue3",
     ylab = "Annual max temp (C)",
     xlab = "Year", 
     yaxt = "n",
     xaxt = "n",
     ylim =c(8, 16),
     xlim = c(1930, 2020))
# add y axis
axis(2, seq(0,20, by=2), las=2 )
axis(1, seq(1930, 2020, by=5), las=1)

# add new york
points(ny_tm$year, ny_tm$Tmax,
       type = "b",
       pch = 19,
       col="tomato3")

# add legend
legend("topleft", # position
       c("North Dakota", "New York"),
       col= c("dodgerblue3", "tomato3"),
       pch= c(17, 19),
       lwd=1, 
       bty="n")

## Question 4 ## (Answered using code above)

## Using packages in R ##
## installing package ##
## install.packages("ggplot2")

## run library at beginning of code ##
library(ggplot2)

## Plotting in ggplot2 ##
# ggplot of the annual precipitation across years for all of the sites
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ # data for plot
        geom_point()+ # make points at data point
        geom_path()+ # use lines to connect data points
        labs(x="year", y="Annual Precipitation") # make axis labels

# annual precipitation across years for all of the sites without grey gridlines
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ # data for plot
        geom_point()+ # make points at data point
        geom_path()+ # use lines to connect data points
        labs(x="year", y="Annual Precipitation")+ # make axis labels
        theme_classic() # change plot theme

# changing plot colors & transparency for visibility/readability
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+
        geom_point(alpha=0.5)+
        geom_path(alpha=0.5)+
        labs(x="year", y="Annual Precipitation")+
        theme_classic()+
        scale_color_manual(values = c("#7FB3D5","#34495E", "#E7B800", "#FC4E07","#26A69A"))

## Question 5 ##
# my edited plot for question 5 with revised colors
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+
        geom_point(alpha=1.5)+
        geom_path(alpha=0.5)+
        labs(x="year", y="Annual Precipitation")+
        theme_classic()+
        scale_color_manual(values = c("#FF02FF","#1F75FF", "#FF5A1E", "#00841A","#EB3A1E"))


## Exploring different visualizations in ggplot2 ##
ggplot(data = datW, aes(x=NAME, y=TMIN))+ # look at daily tmin
        geom_violin(fill=rgb(0.933,0.953,0.98))+ # add a violin plot with blue color
        geom_boxplot(width=0.2,size=0.25, fill="grey90")+ # add grey boxplots and make them about 20% smaller than normal with 25% thinner lines than normal
        theme_classic() # git rid of ugly gridlines

# daily patterns within a year in Morman Flat AZ, 1974
sub <- datW[datW$NAME == nameS[4] & datW$ year == 1974,]

# specify date format
# %Y means a four number year 
# - indicates that the date uses dashes to seperate
# %m means month
# %d means day
sub$DATE <- as.Date(sub$DATE,"%Y-%m-%d")

## Question 6 ## (Answered below and in word doc)

## Jan-05-88 ##
# sub$DATE <- as.Date(sub$DATE,“%b-%d-%y”)

## 09/30/2020 ##
#sub$DATE <- as.Date(sub$DATE,“%m/%d/%Y”)

## 31-January-18
#sub$DATE <- as.Date(sub$DATE,“%d-%B-%y”)

# plot with date on the x axis in ggplot
ggplot(data=sub, aes(x=DATE, y=TMAX))+
        geom_point()+
        geom_path()+
        theme_classic()+
        labs(x="year", y="Maximimum temperature (C)")

# Barplot for precipitation since we are often looking at totals for a day/year
ggplot(data=sub, aes(x=DATE, y=PRCP))+
        geom_col(fill="royalblue3")+
        theme_classic()+
        labs(x="year", y="Daily precipitation (mm)")


## Question 7 ## (Answered using code above)


## Question 8 ##
# daily patterns within a year in Aberdeen WA, 1974
subAb <- datW[datW$NAME == nameS[1] & datW$ year == 1974,]
subAb$DATE <- as.Date(subAb$DATE,"%Y-%m-%d")

# ggplot for maximum temp. in Aberdeen WA, 1974
ggplot(data=subAb, aes(x=DATE, y=TMAX))+
        geom_point()+
        geom_path()+
        theme_classic()+
        labs(x="year", y="Maximimum temperature (C)")

# Barplot for precipitation in Aberdeen WA, 1974
ggplot(data=subAb, aes(x=DATE, y=PRCP))+
        geom_col(fill="royalblue3")+
        theme_classic()+
        labs(x="year", y="Daily precipitation (mm)")

## Question 9 ##
# Aberdeen, WA 
ggplot(data = datW, aes(x=NAME, y=TMIN))+ # look at daily tmin
        geom_violin(fill=rgb(0.933,0.953,0.98))+ # add a violin plot with blue color
        geom_boxplot(width=0.2,size=0.25, fill="grey90")+ # add grey boxplots and make them about 20% smaller than normal with 25% thinner lines than normal
        theme_classic() # git rid of ugly gridlines
dev.off()
sub <- datW[datW$NAME == nameS[1] & datW$ year > 1999,]
sub$year <- as.factor(sub$year)


#make violin plot
ggplot(data = sub, aes(x=year, y=TMIN))+ # look at daily tmin
        geom_violin(fill=rgb(0.933,0.953,0.98))+ # add a violin plot with blue color
        geom_boxplot(width=0.2,size=0.25, fill="grey90")+ # add grey boxplots and make them about 20% smaller than normal with 25% thinner lines than normal
        theme_classic()+ # git rid of ugly gridlines
        labs(x="year", y="TMIN (degrees celcius)")
