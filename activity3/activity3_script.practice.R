## Activity 3 ## 

ch4 <- read.csv("/Users/JonBeniers/Desktop/ENVST206/Activity 3 Data/lemming_herbivory.csv")

ch4$herbivory <- as.factor(ch4$herbivory)

plot(ch4$CH4_Flux ~ ch4$herbivory)

# shapiro test is good for smaller data sets (under a couple thousand)
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])

# dependent variable ~ independent variable
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)

t.test(ch4$CH4_Flux ~ ch4$herbivory)

