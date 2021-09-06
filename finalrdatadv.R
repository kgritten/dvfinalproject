#Renaming Columns in R
#changing column name male or female to gender 
names(dvdata)[names(dvdata) == "Male or Female"] <- "Gender"

#Subsetting using Column Names
#having only responses, gender, military/civilian, age when left, year left as columns only seen
keeps <- c("Responses", "Gender", "Military/civilian", "Age when left", "year left")
dvdata2 <- dvdata[keeps]

#Recoding into a New Variable in R
#changing m (male) to 0 variable and f (female) to 1 variable
dvdata2$Gender[dvdata2$Gender=='m'] <- 0
dvdata2$Gender[dvdata2$Gender=='f'] <- 1

#changing m (military) to 0 variable and c (civilian) to 1 variable
dvdata2$`Military/civilian`[dvdata2$`Military/civilian`=='m'] <- 0
dvdata2$`Military/civilian`[dvdata2$`Military/civilian`=='c'] <- 1

#changing all years 2019 and earlier to 0 variable and 2020 and further to 1 variable
dvdata2$`year left`[dvdata2$`year left`=='2016-2019'] <- 0
dvdata2$`year left`[dvdata2$`year left`=='before 2000'] <- 0
dvdata2$`year left`[dvdata2$`year left`=='2006-2010'] <- 0
dvdata2$`year left`[dvdata2$`year left`=='2011-2015'] <- 0
dvdata2$`year left`[dvdata2$`year left`=='2001-2005'] <- 0
dvdata2$`year left`[dvdata2$`year left`=='covid 2020'] <- 1
dvdata2$`year left`[dvdata2$`year left`=='covid 2021'] <- 1

library(ggplot2)
library(lattice)
barchart(dvdata2$Gender)
barchart(dvdata2$`Military/civilian`)
barchart(dvdata2$`year left`)
barchart(dvdata2$`Age when left`)

#Adding Labels and a Title
barchart(dvdata2$Gender, main="Gender", ylab= "Gender Category", xlab="Frequency", col="purple")
barchart(dvdata2$`Military/civilian`, main="Military or Civilian", ylab= "M/C Category", xlab="Frequency", col="blue")
barchart(dvdata2$`year left`, main="Year Left Relationship/Before Covid or Covid", ylab= "Year Category", xlab="Frequency", col="green")
barchart(dvdata2$`Age when left`, main="Age when left Relationship", ylab= "Age Category", xlab="Frequency", col="gray")

#mean, median, mode of gender
#Create a vector 
x <- c(1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1)
#Find Mean of gender
result.mean <- mean(x)
print(result.mean)

#Find the median of gender
median.result <- median(x)
print(median.result)

#find the mode
x <- c(1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1)
mode(x)

#mean, median, mode of military/civilian
#Create a vector 
m <- c(0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1)
#Find Mean of gender
result.mean <- mean(m)
print(result.mean)

#Find the median of gender
median.result <- median(m)
print(median.result)

#find the mode
m <- c(0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1)
mode(m)

#mean, median, mode of year left
#Create a vector 
y <- c(0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)
#Find Mean of gender
result.mean <- mean(y)
print(result.mean)

#Find the median of gender
median.result <- median(y)
print(median.result)

#find the mode
y <- c(0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)
mode(y)

#mean, median, mode of age left
#Create a vector 
a <- c(25, 45, 18, 17, 35, 25, 35, 45, 35, 25, 18, 25, 17, 35, 25, 35, 18, 17, 45, 45, 25, 35, 55, 35)
#Find Mean of gender
result.mean <- mean(a)
print(result.mean)

#Find the median of gender
median.result <- median(a)
print(median.result)

#find the mode
a <- c(25, 45, 18, 17, 35, 25, 35, 45, 35, 25, 18, 25, 17, 35, 25, 35, 18, 17, 45, 45, 25, 35, 55, 35)
mode(a)

#one proportion testing 
prop.test(x=20, n=25, alternative = "less")
#for male vs female, female being 20 out of 25 results, the pvalue is .9 which means that the proportion is not equal. Under sample estimates, 80% are female and 20% are male

prop.test(x=8, n=24, alternative = "less")
#for military/civilian, military having 8 out of 25 results, one result was n/a, the pvalue is .07 it is true being less than .5. It is not equal and less than civilian. 33% are military. 67% are civilian

prop.test(x=5, n=24, alternative = "less")
#for year left, before covid and covid and later. Covid and later is 5, the rest is before covid out of 24 results and one result being n/a. The pvalue is less than .5 which means the hypothesis is true. The estimate is 20% of those who left covid and later and 80% left before covid 

#2 proportion t-test
prop.test(x= c(20, 8), n= c(25, 24), alternative = "two.sided")
#male vs female and military vs civilian. pvalue for both together is .002603 estimates are the same as one proportion ttest

library("rcompanion")
library("car")
library("effects")
library("multcomp")

#ensure the iv is a factor
str(dvdata2$Gender) 
dvdata2$Gender <- as.factor(dvdata2$Gender)

str(dvdata2$`Military/civilian`) 
dvdata2$`Military/civilian` <- as.factor(dvdata2$`Military/civilian`)

str(dvdata2$`Age when left`) 
dvdata2$`Age when left` <- as.factor(dvdata2$`Age when left`)

str(dvdata2$`year left`) 
dvdata2$`year left` <- as.factor(dvdata2$`year left`)

conf_mat <- caret::confusionMatrix(dvdata2$Gender, dvdata2$`Military/civilian`)
conf_mat


