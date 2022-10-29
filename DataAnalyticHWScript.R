# Library used in this script
library(readr)
library(moments)

# Import the data set
data <- read.csv("Data.csv", header=T)

# Gather the data set
bodyHeight <- data$Body_height
typingSpeed <- data$Typing_speed

##########################
# Descriptive statistics #
# ---- BODY  HEIGHT ---- #
## Basic statistic
summary(bodyHeight)
var(bodyHeight)
quantile(bodyHeight, c(0.1,0.2, 0.25,0.5,0.75,0.8,0.9))
skewness(bodyHeight)

## Statistic visualization
hist(bodyHeight, 15,  xlim = c(150,210),
     main = 'Body Height Histogram', 
     xlab = 'Body Height (cm)', ylab = 'Frequency')
boxplot(bodyHeight,ylim=c(150,210),ylab="Body Height (cm)", main = "Body Height")

# ---- TYPING SPEED ---- #
## Basic statistic
summary(typingSpeed)
var(typingSpeed)
quantile(typingSpeed, c(0.1,0.2, 0.25,0.5,0.75,0.8,0.9))
skewness(typingSpeed)

## Statistic visualization
hist(typingSpeed, 12, xlim = c(20,120), 
     main = 'Typing Speed Histogram', 
     xlab = 'Typing Speed (wpm)', ylab = 'Frequency')
boxplot(typingSpeed,ylim=c(20,120),ylab="Typing Speed (wpm)", main = "Typing Speed")

####################
# Advanced Section #

# Statistical Hypothesis Test
heightTest <- t.test(bodyHeight,alternative = "greater",mu = 158, conf.level = 0.95)
heightTest
speedTest <- t.test(typingSpeed,alternative = "two.sided",mu = 40, conf.level = 0.95)
speedTest

# Body height to Typing Speed Correlation
cor(bodyHeight,typingSpeed)
plot(bodyHeight,typingSpeed, main = 'Body Height vs Typing Speed', 
     xlab = 'Body Height (cm)', ylab = 'Typing Speed (wpm)')

# Split data by Gender
dataMale <- subset(data, data$Gender == "Male")
dataFemale <- subset(data, data$Gender == "Female")

## Get the typing speed data
maleSpeed <- dataMale[,3]
femaleSpeed <- dataFemale[,3]

## Comparison by hypothetical testing
genderComparison <- t.test(maleSpeed, femaleSpeed, conf.level = 0.95,alternative = "greater")
genderComparison

## Visualization
boxplot(list(maleSpeed, femaleSpeed), names = c("Male","Female"), ylim = c(20,120), main = "Typing Speed by Gender", ylab="Typing Speed (wpm)")
