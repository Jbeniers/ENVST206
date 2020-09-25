## Activity 4 ##

## Simple linear regression: Are beavers flooding the Arctic? ##

datB <- read.csv("/Users/JonBeniers/Desktop/ENVST206/Activity 4 Data/beaver_dam.csv")
head(datB)

plot(datB$dams.n, datB$area.h, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab =  "Number of beaver dams")

## Question 1 ## (Answered in word doc.)

## setting up our regression ##
dam.mod <- lm(datB$area.ha ~ datB$dams.n)
# standardized residuals
dam.res <- rstandard(dam.mod)

## checking assumptions ##
## NOTE: Assumptions -> residuals are normally distributed & equal variances ##
# set up qq plot
qqnorm(dam.res)
# add qq line
qqline(dam.res)

## Shapiro-Wilks test for normal distribution (form of double-checking after using qq test) ##
## THE RESIDUALS ARE NORMALLY DISTRIBUTED, p-value greater than 0.05 ## ******
shapiro.test(dam.res)

# make residual plot
## NO REAL PATTERNS/TRENDS = pass the equal variances test ##
plot(datB$dams.n, dam.res,
     xlab = "beaver damns", 
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)

## Interpreting results ##
summary(dam.mod )

# make plot of beaver dams and surface water
plot(datB$dams.n, datB$area.h, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab =  "Number of beaver dams")
# add regression line
# make line width thicker
abline(dam.mod, lwd=2)

## Question 2 ## (Answered in word doc.)

## Multiple linear regression: What makes an early spring? ##
pheno <- read.csv("/Users/JonBeniers/Desktop/ENVST206/Activity 4 Data/red_maple_pheno.csv")

# set up panel of plots with one row and two columns
par(mfrow=c(1,2))
plot(pheno$Tmax,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Maximum temperature (C)")
plot(pheno$Prcp,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Precipitation (mm)")

## Question 3 ##
## Latitude Plot ##
par(mfrow=c(1,2))
plot(pheno$Lat,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Latitude (Degrees)")

## Elevation Plot ##
par(mfrow=c(1,2))
plot(pheno$elev,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Elevation (m)")

## Maximum Temperature Plot ##
par(mfrow=c(1,2))
plot(pheno$Tmax,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Maximum temperature (C)")

## Urban/Rural Plot (Boxplot) ## 
plot(as.factor(pheno$siteDesc), pheno$doy, xlab ="Urban” or “Rural", 
     ylab="Day of leaf out")

dev.off()

## Checking for multi-collinearity ##
plot( ~  pheno$Lat + pheno$Tmax+ pheno$Tmin +pheno$Prcp + pheno$elev + pheno$siteDesc)

## Question 4 ## (Answered in word doc.)

## Running the regression ##
pheno$urID <- ifelse(pheno$siteDesc == "Urban",1,0)
## PRACTICE: pheno$urID <- ifelse(pheno$siteDesc == "Suburban",1,0) ##

## Question 5 ## (Answered using the code above)


## Multiple regression set up ##
mlr <- lm(pheno$doy ~  pheno$Tmax  + pheno$Prcp + pheno$elev + pheno$urID)
# standardized residuals
mlFitted <- fitted(mlr)

mlr.res <- rstandard(mlr)

## checking assumptions for our multiple regression ##
# set up qq plot
qqnorm(mlr.res)
# add qq line
qqline(mlr.res)

# make residual plot
plot(mlFitted, mlr.res,
     xlab = "Fitted", 
     ylab = "standardized residual")
# add a horizontal line at zero
abline(h=0)

## Question 6 ## (Answered using the code above)

## Interpreting the regression ##
summary(mlr)

## Question 7 ## (Answered using the code above)
## Question 8 ## (Answered using the code above)
## Question 9 ## (Answered using the code above)

