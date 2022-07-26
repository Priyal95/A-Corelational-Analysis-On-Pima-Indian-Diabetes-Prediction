db<- read.csv(file.choose())
head(db)
tail(db)
summary(db)
str(db)
#Categorizing age
db$Age_Cat <- ifelse(db$Age < 21, "<21", 
                     ifelse((db$Age>=21) & (db$Age<=25), "21-25", 
                            ifelse((db$Age>25) & (db$Age<=30), "25-30",
                                   ifelse((db$Age>30) & (db$Age<=35), "30-35",
                                          ifelse((db$Age>35) & (db$Age<=40), "35-40",
                                                 ifelse((db$Age>40) & (db$Age<=50), "40-50",
                                                        ifelse((db$Age>50) & (db$Age<=60), "50-60",">60")))))))
db$Age_Cat <- factor(db$Age_Cat, levels = c('<21','21-25','25-30','30-35','35-40','40-50','50-60','>60'))
table(db$Age_Cat)

install.packages("ggplot2")
library(ggplot2)
#graphing age
ggplot(aes(x = Age), data=db) +
  geom_histogram(binwidth=1, color='black', fill = "blue") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5)) +
  xlab("Age") +
  ylab("No of people by age")

#graphing age categorically
ggplot(aes(x = Age_Cat), data = db) +
  geom_bar(col='green')
min(db$Age)
max(db$Age)  
#graphing bloodpressure
ggplot(aes(x =BloodPressure), data=db) +
  geom_histogram(binwidth=1, color='black', fill = "red") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5)) +
  xlab("Blood Pressure") +
  ylab("No of people by age")
#testing normality using Shapiro-Wilk normality test
shapiro.test(db$BloodPressure)
#using the moments package to find the skewness
install.packages("moments")
library("moments")
skewness(db$BloodPressure)
#understanding the min and max level of bp
min(db$BloodPressure)
max(db$BloodPressure)
#using table to understand the number of people with 0 bloodpressure
table(db$BloodPressure)
#replacing the 0 in the bloodpressurs with the median value
db$BloodPressure[db$BloodPressure==0]<- 72
table(db$BloodPressure)
#graphing blood pressure with 0 replaced to the median value
ggplot(aes(x =BloodPressure), data=db) +
  geom_histogram(binwidth=1, color='black', fill = "green") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5)) +
  xlab("Blood Pressure") +
  ylab("No of people")
#Age vs Blood Pressure
ggplot(aes(x=Age_Cat, y = BloodPressure), data = db) +
  geom_boxplot()+geom_jitter(color="blue", size=0.4, alpha=0.9)
#Summary of Blood Pressure and Age Categorically
by(db$BloodPressure, db$Age_Cat, summary)
#Age vs BMI
ggplot(aes(x=Age_Cat, y = BMI), data = db) +
  geom_boxplot()+geom_jitter(color="black", size=0.4, alpha=0.9)
#Summary of BMI and Age Categorically
by(db$BMI, db$Age_Cat, summary)
#Correlation between diffrent variables
db_cor <- round(cor(db[1:9]),1)
db_cor
#plotting a correlation matrix
install.packages("ggcorrplot")
library("ggcorrplot")
ggcorrplot(db_cor)
#regression of outcome on all x values
Regression1<-lm(db$Outcome~db$Pregnancies+db$Glucose+db$BloodPressure+db$SkinThickness+db$Insulin+db$BMI+db$DiabetesPedigreeFunction+db$Age+db$Age_Cat,data=db)
summary(Regression1)
#plotting age_cat and outcome
ggplot(aes(x=Age_Cat, y = Outcome), data = db) +
geom_boxplot()+geom_jitter(color="red", size=0.4, alpha=0.9)
by(db$Outcome, db$Age_Cat, summary)

str(db$Age_Cat)

Regression2<-lm(db$Age~log(db$BMI), data = db)
lbmi<-log(db$BMI)
lbmi
Regression2<-lm(db$Age~log(db$BMI), data = db)
install.packages("lmtest")
library("lmtest")
#Cheching for hetroskdaticity
bptest(db$Outcome~db$BMI+db$Glucose+db$DiabetesPedigreeFunction, data=db)

bptest(db$Outcome~db$loglp(BMI)+db$loglp(Glucose), data=db)
