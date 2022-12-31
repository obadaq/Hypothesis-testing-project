# Hypothesis testing in R 
# Submitted to Dr. Monjed Samuh as a project for Statistics Course 
# The Data set used is "Titanic Data set"
# The objective is to test 3 different hypothesis with 3 different tests

# First : Prepere the data set

titanic.df <- read.csv('titanic.csv')


# Analayzing sub-groubs of the data frame and visualize results

# sub-group 1 : relationship b\w age and survival 

age.su <- data.frame(titanic.df$Age,titanic.df$Survived)
su10 <- subset(age.su,age.su$titanic.df.Survived==0)
su11 <- subset(age.su,age.su$titanic.df.Survived==1)

boxplot(su10$titanic.df.Age,su11$titanic.df.Age,main='Not - Survived                       Survived')
hist(su10$titanic.df.Age,main = 'histogram of Age - Not Survived',xlab = 'Age')
hist(su11$titanic.df.Age,main = 'histogram of Age - Not Survived',xlab = 'Age')



# test normality the two sub-sets
library(nortest)

shapiro.test(su10$titanic.df.Age)
shapiro.test(su11$titanic.df.Age)

# sub-group 2 : relationship b\w gender and survival taking the age into account

gender.su <- data.frame(titanic.df$Sex,titanic.df$Survived)
su20 <- subset(gender.su,gender.su$titanic.df.Survived==0)
su21 <- subset(gender.su,gender.su$titanic.df.Survived==1)


barplot(table(su21$titanic.df.Sex),main='Barplot for Not Survived people',ylab='Number of people')



