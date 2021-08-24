#uploading the data from a csv file
data <- read.csv("C:\\Users\\naman\\Desktop\\Acads\\Spring 2021\\Foundation of Analytics\\Project\\StudentsPerformance.csv", header = T)

library(UsingR)

#taking a look and exploring the data
summary(data)

head(data)

gender_num = ifelse(gender=="female", 1, 0)
cor(gender_num, math.score)
cor(gender_num, reading.score)
cor(gender_num, writing.score)
#we observe that the correlations with gender are weak

# install.packages("tidyverse")
library(tidyverse)

glimpse(data)

attach(data)

#-------------------------------------------------------------------------------
#Cleaning the data

#check if there are NA's
is.na(data)

#see whether na.omit removes anything
newdata <- na.omit(data)

length(newdata$gender)
#length is the same, this means that nothing was omitted and there are no NAs

par(mfrow = c(1,3))
#exploring the numerical variables
boxplot(math.score, main = "Math score", col = 'red')
boxplot(reading.score, main = "Reading score", col = 'red')
boxplot(writing.score, main = "Writing score", col = 'red')
#we observe that there were some outliers
par(mfrow = c(1,1))

#outliers in math score
#outlier values
boxplot(data$math.score)$out
#outlier indexes
outliers <- which(data$math.score %in% boxplot(data$math.score)$out)

#remove outliers from data
data = data[-outliers, ]

#outliers in reading score
#outlier values
boxplot(data$reading.score)$out
#outlier indexes
outliers <- which(data$reading.score %in% boxplot(data$reading.score)$out)

#remove outliers from data
data = data[-outliers, ]

#outliers in writing score
#outlier values
boxplot(data$writing.score)$out
#no remaining outliers

length(data$gender)
#hence, the cleaned dataset is 981 rows long. That is, 19 outlier observations removed.  
#-------------------------------------------------------------------------------

#a function to get a pie chart of given (categorical)column
pieplotter <- function(x){
  empty_list <- vector(mode = "list", length = nlevels(as.factor(x))) #a list to store the number of observations in the corresponding level
  for (i in seq(1, nlevels(as.factor(x)))){
    empty_list[i] <- sum(x == levels(as.factor(x))[i])
  }#for
  data_temp <- table(x)
  slice.labels <- levels(as.factor(x))
  slice.percents <- round(data_temp/sum(data_temp)*100)
  slice.labels <- paste(slice.labels, slice.percents)
  slice.labels <- paste(slice.labels, "%", sep="")
  pie(data_temp, labels = slice.labels, main = title)
}#function

#constructing pieplots for categorical variables
title <- "Gender"
pieplotter(gender)
title <- "Race/Ethnicity"
pieplotter(race.ethnicity)
title <- "Parental level of education"
pieplotter(parental.level.of.education)
title <- "Lunch"
pieplotter(lunch)
title <- "Test prepatation course"
pieplotter(test.preparation.course)

#Quantiles and histograms of numerical variables
quantile(data$math.score)
quantile(data$reading.score)
quantile(data$writing.score)
par(mfrow = c(1, 3))
hist(data$math.score, xlab = "Math Score", main = "Histogram of scores in math")
hist(data$reading.score, xlab = "Reading Score", main = "Histogram of scores in reading")
hist(data$writing.score, xlab = "Writing Score", main = "Histogram of scores in writing")
par(mfrow = c(1,1))

#creating two way tables
table(gender, parental.level.of.education)
ff <-table(gender, parental.level.of.education)
dimnames(ff)<- list(gender=rownames(ff), parental.level.of.education= colnames(ff))
addmargins(ff)
prop.table(ff, 1)
prop.table(ff, 2)
#pretty even spread

table(race.ethnicity, parental.level.of.education)
ff <-table(race.ethnicity, parental.level.of.education)
dimnames(ff)<- list(race/ethnicity=rownames(ff), parental.level.of.education= colnames(ff))
addmargins(ff)
prop.table(ff, 1)
prop.table(ff, 2)

#---------------------------------------------------------------------------------------------
#Checking applicability of Central Limit Theorem on math scores
par(mfrow = c(1, 3))

#taking 1000 samples of size 20, store their mean is a list
empty_list <- vector(mode = "list", length = 1000) #a list to store the mean of the sample

for(i in seq(1, 1000)){
  empty_list[i] <- mean(sample(data$math.score, 20, replace = FALSE))
}

#empty_list

hist(unlist(empty_list, use.names = FALSE), prob = TRUE, main = "Sample size 20", xlab = "Sample means")
lines(density(unlist(empty_list, use.names = FALSE)))             # add a density estimate with defaults
lines(density(unlist(empty_list, use.names = FALSE), adjust=2), lty="dotted", col = "orange")   # add another "smoother" density
abline(v=mean(unlist(empty_list, use.names = FALSE)),col="red")

#taking 1000 samples of size 50, store their mean is a list
empty_list <- vector(mode = "list", length = 1000) #a list to store the mean of the sample

for(i in seq(1, 1000)){
  empty_list[i] <- mean(sample(data$math.score, 50, replace = FALSE))
}

#empty_list

hist(unlist(empty_list, use.names = FALSE), prob = TRUE, main = "Sample size 50", xlab = "Sample means")
lines(density(unlist(empty_list, use.names = FALSE)))             # add a density estimate with defaults
lines(density(unlist(empty_list, use.names = FALSE), adjust=2), lty="dotted", col = "orange")   # add another "smoother" density
abline(v=mean(unlist(empty_list, use.names = FALSE)),col="red")

#taking 1000 samples of size 100, store their mean is a list
empty_list <- vector(mode = "list", length = 1000) #a list to store the mean of the sample

for(i in seq(1, 1000)){
  empty_list[i] <- mean(sample(data$math.score, 100, replace = FALSE))
}

#empty_list

hist(unlist(empty_list, use.names = FALSE), prob = TRUE, main = "Sample size 100", xlab = "Sample means")
lines(density(unlist(empty_list, use.names = FALSE)))             # add a density estimate with defaults
lines(density(unlist(empty_list, use.names = FALSE), adjust=2), lty="dotted", col = "orange")   # add another "smoother" density
abline(v=mean(unlist(empty_list, use.names = FALSE)),col="red")


#Checking applicability of Central Limit Theorem on reading scores

#taking 1000 samples of size 20, store their mean is a list
empty_list <- vector(mode = "list", length = 1000) #a list to store the mean of the sample

for(i in seq(1, 1000)){
  empty_list[i] <- mean(sample(data$reading.score, 20, replace = FALSE))
}

#empty_list

hist(unlist(empty_list, use.names = FALSE), prob = TRUE, main = "Sample size 20", xlab = "Sample means")
lines(density(unlist(empty_list, use.names = FALSE)))             # add a density estimate with defaults
lines(density(unlist(empty_list, use.names = FALSE), adjust=2), lty="dotted", col = "orange")   # add another "smoother" density
abline(v=mean(unlist(empty_list, use.names = FALSE)),col="red")

#taking 1000 samples of size 50, store their mean is a list
empty_list <- vector(mode = "list", length = 1000) #a list to store the mean of the sample

for(i in seq(1, 1000)){
  empty_list[i] <- mean(sample(data$reading.score, 50, replace = FALSE))
}

#empty_list

hist(unlist(empty_list, use.names = FALSE), prob = TRUE, main = "Sample size 50", xlab = "Sample means")
lines(density(unlist(empty_list, use.names = FALSE)))             # add a density estimate with defaults
lines(density(unlist(empty_list, use.names = FALSE), adjust=2), lty="dotted", col = "orange")   # add another "smoother" density
abline(v=mean(unlist(empty_list, use.names = FALSE)),col="red")

#taking 1000 samples of size 100, store their mean is a list
empty_list <- vector(mode = "list", length = 1000) #a list to store the mean of the sample

for(i in seq(1, 1000)){
  empty_list[i] <- mean(sample(data$reading.score, 100, replace = FALSE))
}

#empty_list

hist(unlist(empty_list, use.names = FALSE), prob = TRUE, main = "Sample size 100", xlab = "Sample means")
lines(density(unlist(empty_list, use.names = FALSE)))             # add a density estimate with defaults
lines(density(unlist(empty_list, use.names = FALSE), adjust=2), lty="dotted", col = "orange")   # add another "smoother" density
abline(v=mean(unlist(empty_list, use.names = FALSE)),col="red")




#Checking applicability of Central Limit Theorem on writing scores

#taking 1000 samples of size 20, store their mean is a list
empty_list <- vector(mode = "list", length = 1000) #a list to store the mean of the sample

for(i in seq(1, 1000)){
  empty_list[i] <- mean(sample(data$writing.score, 20, replace = FALSE))
}

#empty_list

hist(unlist(empty_list, use.names = FALSE), prob = TRUE, main = "Sample size 20", xlab = "Sample means")
lines(density(unlist(empty_list, use.names = FALSE)))             # add a density estimate with defaults
lines(density(unlist(empty_list, use.names = FALSE), adjust=2), lty="dotted", col = "orange")   # add another "smoother" density
abline(v=mean(unlist(empty_list, use.names = FALSE)),col="red")

#taking 1000 samples of size 50, store their mean is a list
empty_list <- vector(mode = "list", length = 1000) #a list to store the mean of the sample

for(i in seq(1, 1000)){
  empty_list[i] <- mean(sample(data$writing.score, 50, replace = FALSE))
}

#empty_list

hist(unlist(empty_list, use.names = FALSE), prob = TRUE, main = "Sample size 50", xlab = "Sample means")
lines(density(unlist(empty_list, use.names = FALSE)))             # add a density estimate with defaults
lines(density(unlist(empty_list, use.names = FALSE), adjust=2), lty="dotted", col = "orange")   # add another "smoother" density
abline(v=mean(unlist(empty_list, use.names = FALSE)),col="red")

#taking 1000 samples of size 100, store their mean is a list
empty_list <- vector(mode = "list", length = 1000) #a list to store the mean of the sample

for(i in seq(1, 1000)){
  empty_list[i] <- mean(sample(data$writing.score, 100, replace = FALSE))
}

#empty_list

hist(unlist(empty_list, use.names = FALSE), prob = TRUE, main = "Sample size 100", xlab = "Sample means")
lines(density(unlist(empty_list, use.names = FALSE)))             # add a density estimate with defaults
lines(density(unlist(empty_list, use.names = FALSE), adjust=2), lty="dotted", col = "orange")   # add another "smoother" density
abline(v=mean(unlist(empty_list, use.names = FALSE)),col="red")

par(mfrow = c(1,1))

#-------------------------------------------------------------------
#Build a multiple linear regression model

m <- lm(formula = data$math.score ~ data$gender + data$race.ethnicity + data$parental.level.of.education + data$lunch + data$test.preparation.course)

install.packages("car")
library(car)
Anova(m)
#assessment of the model
summary(m)$adj.r.squared

#m2 <- lm(formula = data$math.score ~ data$parental.level.of.education + data$test.preparation.course)
#Anova(m2)
#summary(m2)$adj.r.squared

cor(data$math.score, data$reading.score)
cor(data$math.score, data$writing.score)
cor(data$writing.score, data$reading.score)
#all three scores are highly correlated

#try building a linear regression model, predicting one of the scores, with the other 2 scores as the features
m3 <- lm(formula = data$math.score ~ data$reading.score + data$writing.score)
Anova(m3)
summary(m3)$adj.r.squared

m4 <- lm(formula = data$reading.score ~ data$math.score + data$writing.score)
Anova(m4)
summary(m4)$adj.r.squared

m5 <- lm(formula = data$writing.score ~ data$reading.score + data$math.score)
Anova(m5)
summary(m5)$adj.r.squared

#-------------------------------------------------------------------

library(sampling)
# Simple random sampling without replacement - srswor
set.seed(153)
s <- srswor(50, nrow(data))
sample <- data[s != 0, ]
head(sample)

#run regression on this sample
m3 <- lm(formula = sample$math.score ~ sample$reading.score + sample$writing.score)
Anova(m3)
summary(m3)$adj.r.squared

m4 <- lm(formula = sample$reading.score ~ sample$math.score + sample$writing.score)
Anova(m4)
summary(m4)$adj.r.squared

m5 <- lm(formula = sample$writing.score ~ sample$reading.score + sample$math.score)
Anova(m5)
summary(m5)$adj.r.squared


#Systematic Sampling
set.seed(113)
N <- nrow(data)
n <- 50

#k <- ceiling(N / n)
k <- floor(N / n)
k

r <- sample(k, 1)
r
# select every kth item
s <- seq(r, by = k, length = n)

sample <- data[s, ]
tail(sample)
nrow(sample)

#run regression on this sample
m3 <- lm(formula = sample$math.score ~ sample$reading.score + sample$writing.score)
Anova(m3)
summary(m3)$adj.r.squared

m4 <- lm(formula = sample$reading.score ~ sample$math.score + sample$writing.score)
Anova(m4)
summary(m4)$adj.r.squared

m5 <- lm(formula = sample$writing.score ~ sample$reading.score + sample$math.score)
Anova(m5)
summary(m5)$adj.r.squared



# Strata with Two Variables
set.seed(123)
# Stratified, unequal sized strata

head(data)

data <- data[order(data$parental.level.of.education, data$test.preparation.course), ]

freq <- table(data$parental.level.of.education, data$test.preparation.course)
freq

st.sizes <- round(50 * freq / sum(freq))
st.sizes
class(st.sizes)

as.vector(st.sizes)

as.vector(t(st.sizes))

st.sizes <- as.vector(t(st.sizes))
st.sizes <- st.sizes[st.sizes != 0]

st.sizes

st.3 <- strata(data, 
               stratanames = c("parental.level.of.education", "test.preparation.course"),
               size = st.sizes, method = "srswor",
               description = TRUE)

st.3

sample <- getdata(data, st.3)

head(sample)

#run regression on this sample
m3 <- lm(formula = sample$math.score ~ sample$reading.score + sample$writing.score)
Anova(m3)
summary(m3)$adj.r.squared

m4 <- lm(formula = sample$reading.score ~ sample$math.score + sample$writing.score)
Anova(m4)
summary(m4)$adj.r.squared

m5 <- lm(formula = sample$writing.score ~ sample$reading.score + sample$math.score)
Anova(m5)
summary(m5)$adj.r.squared
#-------------------------------------------------------------------

