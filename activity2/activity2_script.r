# Activity 2 

## Vectors ##
# make a vector of tree heights in meters
heights <- c(30,41,20,22)
# convert to cm
heights_cm <- heights*100
heights_cm

# look at the first tree height
heights[1]

# look at the 2nd and 3rd tree heights
heights[2:3]

## Matrices ##
# get more info on the matrix function
help(matrix)

# set up a matrix with 2 columns and fill in by rows
# first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

# set up a matrix that fills in by columns
# first argument is the vector of numbers to fill in the matrix
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol

# subset the matrix to look at row 1, column2
Mat.bycol[1,2]

# look at all values in row 1
Mat.bycol[1,]

# look at all values in column 2
Mat.bycol[,2]

## Dataframes ##
# read in weather station file from the data folder
# adjust the directory to match your folder
datW <- read.csv("/Users/JonBeniers/Desktop/ENVST206/Activity 2 Data/noaa2011124.csv")

## Question 1 ##
# get more information about the dataframe
str(datW)

datW$NAME <- as.factor(datW$NAME)

## Question 2 ##
# example character vector
vec_char <- c("j","o","n","a","t")

# example numeric vector
vec_num <- c(1.0,2.1,3.2,4.3,5.4)

# example integer vector 
vec_int <- c(6,7,8,9,10)

# example factor vector ???
# vec_fac <- 

## Descriptive Statistics and Histograms ##
# find out all unique site names
levels(datW$NAME)

# look at the mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])

# look at the mean maximum temperature for Aberdeen
# with na.rm argument set to true to ingnore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

# next look at the standard deviation
sd(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

# calculate the average daily temperature
# This temperature will be halfway between the minimum and maximum temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

# get the mean across all sites
# the by function is a list of one or more variables to index over.
# FUN indicates the function we want to use
# if you want to specify any function specific arguments use a comma and add them after the function
# here we want to use the na.rm arguments specific to 
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

# change the automatic output of column names to be more meaningful
# note that MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

# convert level to number for factor data type
# you will have to reference the level output or look at the row of data to see the character designation.
datW$siteN <- as.numeric(datW$NAME)

# make a histogram for the first site in our levels
# main= is the title name argument.
# Here you want to paste the actual name of the factor not the numeric index
# since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

## Question 3 ##
# look up help
help(hist) 

## Probability Distributions ##

## Question 4 ##
# make a histogram for another one of the weather stations - Mormon Flat, AZ US
# main= is the title name argument.
hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

## Probability Distributions Continued ##
help(dnorm)

# pnorm(value to evaluate at (note this will evaluate for all values and below),mean, standard deviation)
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

# pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

# pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE)) - pnorm(0,
                                                         mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                                                         sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

# pnrom of 20 gives me all probability (area of the curve) below 20 
# subtracting from one leaves me with the area above 20
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

# qnorm gives me the value at which all values and below equal the probability in my argument
# Here I'm calculating the value of the 95th quantile or a probability of 0.95
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

## Question 5 ## 
# Here I'm calculating the value of the 95th quantile or a probability of 0.95
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

# pnrom of 18.51026 gives me all probability (area of the curve) below 18.51026 
# subtracting from one leaves me with the area above 18.51026
1 - pnorm(18.51026,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

## Evaluate patterns in precipitation data ##
## Question 6 ##
# make a histogram of daily precipitation in Aberdeen
# main= is the title name argument.
# Here you want to paste the actual name of the factor not the numeric index
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily precipitation (mm)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

## Question 7 ##
# sum and aggregate function for precipitation for each year and site
totalPrcp <- aggregate(datW$TAVE, by=list(datW$NAME, datW$year), FUN="sum",na.rm=TRUE)
totalPrcp

## Question 8 ##
colnames(totalPrcp) <- c("NAME","year","sum")
# make a histogram of total annual precipitation in Aberdeen, WA US
hist(totalPrcp$sum[totalPrcp$NAME == "ABERDEEN, WA US"],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average precipitation (mm)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

# make a histogram of total annual precipitation in Mandan Station ND
hist(totalPrcp$sum[totalPrcp$NAME == "MANDAN EXPERIMENT STATION, ND US"],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average precipitation (mm)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

## Question 9 ##
# pnrom with 700 gives us the probability (area of the curve) of precipitation below 700 mm in Aberdeen WA 
pnorm(700,
      mean(totalPrcp$sum[totalPrcp$NAME == "ABERDEEN, WA US"],na.rm=TRUE),
      sd(totalPrcp$sum[totalPrcp$NAME == "ABERDEEN, WA US"],na.rm=TRUE))


# pnrom with 700 gives us the probability (area of the curve) of precipitation below 700 mm in MANDAN EXPERIMENT STATION, ND US 
pnorm(700,
      mean(totalPrcp$sum[totalPrcp$NAME == "MANDAN EXPERIMENT STATION, ND US"],na.rm=TRUE),
      sd(totalPrcp$sum[totalPrcp$NAME == "MANDAN EXPERIMENT STATION, ND US"],na.rm=TRUE))
