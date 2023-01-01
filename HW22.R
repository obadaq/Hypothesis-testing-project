# Hypothesis testing in R 
# Submitted to Dr. Monjed Samuh as a project for Statistics Course 
# The Data set used is "Titanic Data set"
# The objective is to test 3 different hypothesis with 3 different tests


# First : Prepere the data set
titanic.df <- read.csv('titanic3.csv')

# Analayzing sub-groubs of the data frame and visualize results

# sub-group 1 : relationship b\w age and survival 

age.su <- data.frame(titanic.df$age,titanic.df$survived)
su10 <- subset(age.su,age.su$titanic.df.survived==0)
su11 <- subset(age.su,age.su$titanic.df.survived==1)

boxplot(su10$titanic.df.age,su11$titanic.df.age,main='Not - Survived                       Survived')
hist(su10$titanic.df.age,main = 'Histogram of Age - Not Survived',xlab = 'Age')
hist(su11$titanic.df.age,main = 'Histogram of Age - Survived',xlab = 'Age')


# test normality the two sub-sets
library(nortest)

shapiro.test(su10$titanic.df.age)
shapiro.test(su11$titanic.df.age)

wilcox.test(su10$titanic.df.age,su11$titanic.df.age)

# sub-group 2 : relationship b\w gender and survival taking the age into account

gender.su <- data.frame(titanic.df$sex,titanic.df$age,titanic.df$survived)
su20 <- subset(gender.su,gender.su$titanic.df.survived==0)
su21 <- subset(gender.su,gender.su$titanic.df.survived==1)

barplot(table(su20$titanic.df.sex),main='Barplot for Not Survived people',ylab='Number of people')
barplot(table(su21$titanic.df.sex),main='Barplot for Survived people',ylab='Number of people')


msu0 <- subset(su20,su20$titanic.df.sex=='male')
fsu0 <- subset(su20,su20$titanic.df.sex=='female')
msu1 <- subset(su21,su21$titanic.df.sex=='male')
fsu1 <- subset(su21,su21$titanic.df.sex=='female')

boxplot(msu0$titanic.df.age,msu1$titanic.df.age,fsu0$titanic.df.age,fsu1$titanic.df.age,main='Male_not_survived     Male_Survived     Female_Not_Survived     Female_Survived',cex.main=0.9)

hist(msu0$titanic.df.age,main = 'histogram of Age - Not Survived - Male',xlab = 'Age')
hist(msu1$titanic.df.age,main = 'histogram of Age -  Survived - Male',xlab = 'Age')
hist(fsu0$titanic.df.age,main = 'histogram of Age - Not Survived - Female',xlab = 'Age')
hist(fsu1$titanic.df.age,main = 'histogram of Age - Survived - Female',xlab = 'Age')

shapiro.test(msu0$titanic.df.age)
shapiro.test(msu1$titanic.df.age)
shapiro.test(fsu0$titanic.df.age)
shapiro.test(fsu1$titanic.df.age)


# sub-group 3 : relationship b\w age and survival taking the gender into account

su31 <- su11
su30 <- su10

su30 <- cut(su30$titanic.df.age,c(0,18,30,55,150))
su31 <- cut(su31$titanic.df.age,c(0,18,30,55,150))

x <- table(su31)
y <- table(su30)
x <- data.frame(t(t(table(su31))))
y <- data.frame(t(t(table(su30))))
x <- x[-2]
y <- y[-2]

AgTable <- cbind(x,y$Freq)
names(AgTable)[names(AgTable) == "su31"] <- "def"
names(AgTable)[names(AgTable) == "Freq"] <- "Survived"
names(AgTable)[names(AgTable) == "y$Freq"] <- "Un-Survived"
rownames(AgTable)<-c("Young","Young Adult","Adult","Old")
AgTable


temp <- AgTable$Survived
names(temp)<-c("young","young adult","Adult","old")
barplot(temp,main = 'Survived')
boxplot(temp, main='Boxplot for Survived')

temp<-AgTable$`Un-Survived`
names(temp)<-c("young","young adult","Adult","old")
barplot(temp,main='Un-Survived')
boxplot(temp, main='Boxplot for Not-Survived')

AgTable<-AgTable[,-1]
chisq.test(AgTable)


# sub-group 4 : relationship b\w survival and gender

gen <- data.frame(titanic.df$sex,titanic.df$survived)
su40 <- subset(gen,gen$titanic.df.survived==0)
su41 <- subset(gen,gen$titanic.df.survived==1)

su40<-data.frame(t(t(table(su40$titanic.df.sex))))
su41<-data.frame(t(t(table(su41$titanic.df.sex))))


su40<-su40[-2]
su40<-su40[-1]
su41<-su41[-2]
su41<-su41[-1]
GenderTable<-cbind(su40,su41$Freq)
rownames(GenderTable)<-c("Female","Male")
colnames(GenderTable)<-c("Survived","Not-Survived")

GenderTable

temp<-GenderTable$Survived
names(temp)<-c("Female","Male")
barplot(temp,main = "Survived")

temp<-GenderTable$`Not-Survived`
names(temp)<-c("Female","Male")
barplot(temp,main = "Not-Survived")

# sub-group 5 : relationship b\w survival and Pclass
temp<-subset(titanic.df,titanic.df$survived==0)
temp<-subset(temp,temp$pclass==1)
class1n<-data.frame(temp$pclass,temp$survived)

temp<-subset(titanic.df,titanic.df$survived==1)
temp<-subset(temp,temp$pclass==1)
class1s<-data.frame(temp$pclass,temp$survived)

temp<-subset(titanic.df,titanic.df$survived==0)
temp<-subset(temp,temp$pclass==2)
class2n<-data.frame(temp$pclass,temp$survived)

temp<-subset(titanic.df,titanic.df$survived==1)
temp<-subset(temp,temp$pclass==2)
class2s<-data.frame(temp$pclass,temp$survived)

temp<-subset(titanic.df,titanic.df$survived==0)
temp<-subset(temp,temp$pclass==3)
class3n<-data.frame(temp$pclass,temp$survived)

temp<-subset(titanic.df,titanic.df$survived==1)
temp<-subset(temp,temp$pclass==3)
class3s<-data.frame(temp$pclass,temp$survived)

class1s<-table(class1s)
class1n<-table(class1n)
class2s<-table(class2s)
class2n<-table(class2n)
class3n<-table(class3n)
class3s<-table(class3s)

ClassTable<-matrix(c(class1s[-2:-3],class1n[-2:-3],class2s[1],class2n[1],class3s[1],class3n[1]),ncol=2,byrow=TRUE)
rownames(ClassTable)<-c("Class-1","Class-2","Class-3")
colnames(ClassTable)<-c("Survived","Un-Survived")
ClassTable
barplot(ClassTable)
library("nptest")
np.loc.test(ClassTable)

