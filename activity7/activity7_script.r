## Activity 7 ##

## Question 1 ## (Answered in word doc)
## Question 2 ## (Answered in word doc)
## Question 3 ## (Answered in word doc)
## Question 4 ## (Answered in word doc)

## Question 5 ##

NYDrought <- read.csv("/Users/JonBeniers/Desktop/ENVST206/Activity 7 Data/dm_export_20191022_20201022.csv")

# Mean & SD of NY D1 level drought from 10/22/2019 to 10/22/2020
mean(NYDrought[,5])
sd(NYDrought[,5])

# plot of NY D1 level drought from 10/22/2019 to 10/22/2020
plot(NYDrought[,5],
     type = "b",
     pch = 19,
     ylab = "Cumulative % Area of NY in D1 Level of Drought",
     xlab = "53 Observations Recorded between 10/22/2019 - 10/22/2020 ")

## Question 6 ## (Answered in word doc)
## Question 7 ## (Answered in word doc)
## Question 8 ## (Answered in word doc)