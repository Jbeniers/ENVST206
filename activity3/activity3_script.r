## Activity 3 ## 

## t-tests ##
ch4 <- read.csv("/Users/JonBeniers/Desktop/ENVST206/Activity 3 Data/lemming_herbivory.csv")

ch4$herbivory <- as.factor(ch4$herbivory)

# NOTE: Here positive values indicate that methane is being emitted from the plot
# NOTE: Negative value indicates that there is methane uptake occurring over the plot surface.
plot(ch4$CH4_Flux ~ ch4$herbivory, xlab ="Treatment", 
     ylab="CH4 fluxes (mgC m –2 day–1) ")

# use the shapiro wilks test to look for normality in each treatment
# shapiro-wilk test on grazing plots
# NOTE: p-value less than 0.05 = reject null hypothesis. 
# NOTE: p-value greater than 0.05 = assume that our data is normally distributed. 
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])

# shapiro-wilk test on grazing exclusion plots
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])

# use bartlett test since testing for equal variance
# dependent variable ~ independent variable
# NOTE: test statistic is well above 0.05 so you can assume the variances are equal.
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)

t.test(ch4$CH4_Flux ~ ch4$herbivory)

## Question 1 ## (Answered using the above code)
## Question 2 ## (Answered using R documentation & online resources)

## Analysis of Variance (ANOVA) ##
#read in insect data
datI <- read.csv("/Users/JonBeniers/Desktop/ENVST206/Activity 3 Data/insect_richness.csv")

## Question 3 ## (Answered in my word doc.)

datI$urbanName <- as.factor(datI$urbanName)

## Question 4 ## (Answered in my word doc.)
## Question 5 ## (Answered in my word doc. using the provided boxplots)

# specify model for species richness and urban type
in.mod <- lm(datI$Richness ~ datI$urbanName)
# run the ANOVA
in.aov <- aov(in.mod)
# print out ANOVA table
summary(in.aov)

# run Tukey HSD
tukeyT <- TukeyHSD(in.aov)
# view results
tukeyT

# make a plot
# make axes labels smaller than usual to fit on plot using cex.axis 
plot(tukeyT, cex.axis=0.75)

## Question 6 ## (Answered in my word doc.)

tapply(datI$Richness, datI$urbanName, "mean")

## Question 7 ## (Answered in my word doc.)

## Chi-squared goodness of fit test ##
# set up contingency table
species <- matrix(c(18,8,15,32), ncol=2, byrow = TRUE) 
colnames(species) <- c("Not protected", "Protected")
rownames(species) <- c("Declining", "Stable/Increase")

# make a mosaic plot with an informative title and axes labels
mosaicplot(species, xlab="population status", ylab="legal protection",
           main="Legal protection impacts on populations")

## Question 8 ## (Answered in my word doc. using the contingency table above)

# Conduct a chi-squared test
chisq.test(species)

## Question 9 ## (Answered using the chisq.test above)
