library(car)
library(psych)#describe
library(caTools)#splitting
library(hydroGOF) #rmse


#Import data in R
LGD<-read.csv("C:/Users/Admin/Downloads/R_Module_Day_5.2_Data_Case_Study_Loss_Given_Default.csv")
dim(LGD)
View(LGD)
#variables in the data
names(LGD)
#Exploratory data analysis
#remove Account number because its just a reference and its unique
LGD[,1]<-NULL
#logLoss(LGD$Losses.in.Thousands)
#checking for missing values
colSums(is.na(LGD))
summary(LGD)
#boxplot
boxplot(LGD[,-c(4,5)])
boxplot(log(LGD$Losses.in.Thousands))
#Univariate analysis
#summary of the data
summary(LGD)
head(LGD)
#LGD$Gender[which.max(table(LGD$Gender))]
#mode function
mode1<-function(x){
  tl<-table(x)
m<-tl[which.max(tl)]
m
}
mode(LGD$Married)
# Create the mode function for imputation.
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#Transformation
class(LGD$Married)

#plot
hist(LGD$Losses.in.Thousands)
#Plots
logLoss<-log(LGD$Losses.in.Thousands)
hist(logLoss)
LGD<-data.frame(logLoss,LGD)

#Bivariate analysis
#correlation
#corr<-cor(LGD[,-c(4,5)])
#corr
View(Train1)
logLoss<-log2(Train1$Losses.in.Thousands)
Train1<-data.frame(logLoss,Train1)
View(Train1)
#Test for correlation between dependent and other variables
cor.test(LGD$Years.of.Experience,LGD$logLoss)
cor.test(Train1$Age,Train1$logLoss)
cor.test(Train1$Number.of.Vehicles,Train1$logLoss)
#all the variables were significant except number of vehicles
#ttest
test1<-t.test(logLoss~Gender,LGD)
test1
#summary(aov(LGD$Losses.in.Thousands~LGD$Gender))
test2<-t.test(logLoss~Married,LGD)
test2

#Multicollinearity
t.test(LGD$Age~LGD$Married)
t.test(LGD$Years.of.Experience~LGD$Married)
chisq.test(LGD$Married,LGD$Gender)
cor.test(LGD$Age,LGD$Years.of.Experience)
#repeat the same steps for Gender too
#Frequency table
table(LGD$Gender)
table(LGD$Married)
#converting to Factors
LGD$Gender<-as.factor(LGD$Gender)
LGD$Married<-as.factor(LGD$Married)
LGD$Losses.in.Thousands<-NULL

#Splitting data to train and test
split<-sample.split(LGD$logLoss,0.70)
Train1<-subset(LGD,split==T)
dim(Train1)
Test1<-subset(LGD,split==F)
dim(Test1)

#Test for correlation between dependent and other variables
cor.test(LGD$Years.of.Experience,LGD$logLoss)
cor.test(LGD$Age,LGD$Losses.in.Thousands)
cor.test(LGD$Number.of.Vehicles,LGD$logLoss)
#all the variables were significant except number of vehicles
#ttest
test1<-t.test(logLoss~Gender,LGD)
test1
#summary(aov(LGD$Losses.in.Thousands~LGD$Gender))
test2<-t.test(logLoss~Married,LGD)
test2

#Multicollinearity
t.test(LGD$Age~LGD$Married)
t.test(LGD$Years.of.Experience~LGD$Married)
chisq.test(LGD$Married,LGD$Gender)
cor.test(LGD$Age,LGD$Years.of.Experience)
#repeat the same steps for Gender too
#Frequency table
table(LGD$Gender)
table(LGD$Married)
#converting to Factors
LGD$Gender<-as.factor(LGD$Gender)
LGD$Married<-as.factor(LGD$Married)
LGD$Losses.in.Thousands<-NULL


#Building model
#Age and years of experience are correlated lets taken either of them in the model to avoid multicollinearity
attach(Train1)
Model1<-lm(logLoss~.-Losses.in.Thousands-Age-Number.of.Vehicles,data = Train1)
summary(Model1)

#Sloss<-scale(LGD$Losses.in.Thousands)

#Model1<-lm(sloss~Years.of.Experience+Gender+Married,data = Train1)
#summary(Model1)
#stepAIC(Model)
#Root mean squared error
#rmse<-rmse(Model$fitted.values,Train1$Losses.in.Thousands)
#rmse

#Prediction
predloss<-predict(Model1,newdata=Test1)
#SSE<-sqrt(mean((Test1$Losses.in.Thousands-predloss)^2))
RMSETest<-rmse(Test1$Losses.in.Thousands,predloss) 
RMSETest
#By comparing RMSE of Train and Test data, the Model fits well for Test data.
