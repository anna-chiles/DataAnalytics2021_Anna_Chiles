#ITWS 4600 - Data Analytics
#Lab 2 - Assignment 2
#Anna Chiles, chilea, 661396612


#Lab 2 Part 1a
#Measures of Central Tendencies
rm(list=ls())
#Laptop file path
#EPI_data <- read.csv("C:/Users/Anna/OneDrive/Documents/School Stuff/Grad School/ITWS 4600 - Data Analytics/Lab 2/EPI_data.csv")
#Desktop file path
EPI_data <- read.csv("C:/Users/315ac/OneDrive/Documents/School Stuff/Grad School/ITWS 4600 - Data Analytics/Lab 2/EPI_data.csv")
EPI <- EPI_data$EPI
#Cleaning of EPI variable set
tf <- is.na(EPI)
EPI <- EPI[!tf]
# outputs mean and median central tendencies for EPI variable
summary(EPI)
#function to calculate mode for a given variable x
mode <- function(x){
  ux <-unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}
#outputs mode central tendency for EPI variable
mode(EPI)
DALY <- EPI_data$DALY
#Cleaning of DALY variable set
ft <- is.na(DALY)
DALY <- DALY[!ft]
# outputs mean and median central tendencies for DALY variable
summary(DALY)
#outputs mode central tendency for EPI variable
mode(DALY)

#Histogram Generation
rm(list=ls())
#Laptop file path
#EPI_data2010 <- read.csv("C:/Users/Anna/OneDrive/Documents/School Stuff/Grad School/ITWS 4600 - Data Analytics/Lab 2/2010EPI_data.csv", skip = 1)
#Desktop file path
EPI_data2010 <- read.csv("C:/Users/315ac/OneDrive/Documents/School Stuff/Grad School/ITWS 4600 - Data Analytics/Lab 2/2010EPI_data.csv", skip = 1)
View(EPI_data2010)
EPI2010 <- EPI_data2010$EPI
#Cleaning of EPI variable set
tf <- is.na(EPI2010)
EPI2010 <- EPI2010[!tf]
#Correcting error of margins being too large
par(mar=c(2,2,2,2))
#Generating histogram for EPI variable
hist(EPI2010)
DALY2010 <- EPI_data2010$DALY
#Cleaning of DALY variable set
ft <- is.na(DALY2010)
DALY2010 <- DALY2010[!ft]
#Generating histogram for DALY variable
hist(DALY2010)

#Generating boxplot for both ENVHEALTH and ECOSYSTEM variables
boxplot(EPI_data2010$ENVHEALTH,EPI_data2010$ECOSYSTEM)
#Generating Q-Q plot for both ENVHEALTH and ECOSYSTEM variables
qqplot(EPI_data2010$ENVHEALTH,EPI_data2010$ECOSYSTEM)


#Lab 2 Part 1b
rm(list=ls())
#Laptop file path
#EPI_data <- read.csv("C:/Users/Anna/OneDrive/Documents/School Stuff/Grad School/ITWS 4600 - Data Analytics/Lab 2/EPI_data.csv")
#Desktop file path
EPI_data <- read.csv("C:/Users/315ac/OneDrive/Documents/School Stuff/Grad School/ITWS 4600 - Data Analytics/Lab 2/EPI_data.csv")
EPI_data <- na.omit(EPI_data)
boxplot(ENVHEALTH,DALY,AIR_H,WATER_H)
#create linear model
lmENVH <- lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
summary(lmENVH)
cENVH <- coef(lmENVH)
#create new variables to find a predicted trend along
DALYNEW <- c(seq(5,95,(90/230)))
AIR_HNEW <- c(seq(5,95,(90/230)))
WATER_HNEW <- c(seq(5,95,(90/230)))
NEW <- data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
#run prediction function
pENV <- predict(lmENVH,NEW,interval="prediction")
cENV <- predict(lmENVH,NEW,interval = "confidence")
#repeat for AIR_E
lmENAE <- lm(ENVHEALTH~AIR_E)
cENAE <- coef(lmENAE)
AIR_ENEW <- c(seq(5,95,(90/230)))
AENEW <- data.frame(AIR_ENEW)
pENAE <- predict(lmENAE,AENEW,interval="prediction")
cENAE <- predict(lmENAE,AENEW,interval = "confidence")
#Repeat for CLIMATE
lmENCL <- lm(ENVHEALTH~CLIMATE)
cENCL <- coef(lmENCL)
CLIMATENEW <- c(seq(5,95,(90/230)))
CLNEW <- data.frame(CLIMATENEW)
pENCL <- predict(lmENCL,CLNEW,interval="prediction")
cENCL <- predict(lmENCL,CLNEW,interval = "confidence")


#Lab 2 Part 2
#Exercise 1
rm(list=ls())
#Laptop file path
#EPI_data <- read.csv("C:/Users/Anna/OneDrive/Documents/School Stuff/Grad School/ITWS 4600 - Data Analytics/Lab 2/dataset_multipleRegression.csv")
#Desktop file path
mr_data <- read.csv("C:/Users/315ac/OneDrive/Documents/School Stuff/Grad School/ITWS 4600 - Data Analytics/Lab 2/dataset_multipleRegression.csv")
#same as part 1b, forming a linear model for prediction along certain variables
lmmr <- lm(mr_data$ROLL~mr_data$UNEM+mr_data$HGRAD)
lmmr
summary(lmmr)
cmr <- coef(lmmr)
UNEMNEW <- c(seq(5,7,2/28))
HGRADNEW <- c(seq(1000,90000,89000/28))
NEW <- data.frame(UNEMNEW,HGRADNEW)
pmr <- predict(lmmr,NEW,interval="prediction")
cmr <- predict(lmmr,NEW,interval = "confidence")
#Final predicted value
newroll<-pmr[29,1]
#adding per capita income
lmmr2 <- lm(mr_data$ROLL~mr_data$UNEM+mr_data$HGRAD+mr_data$INC)
lmmr2
summary(lmmr2)
cmr2 <- coef(lmmr2)
UNEMNEWNEW <- c(seq(5,7,2/28))
HGRADNEWNEW <- c(seq(1000,90000,89000/28))
INCNEWNEW <- c(seq(1000,25000,24000/28))
NEWNEW <- data.frame(UNEMNEWNEW,HGRADNEWNEW,INCNEWNEW)
pmr2 <- predict(lmmr2,NEWNEW,interval="prediction")
cmr2 <- predict(lmmr2,NEWNEW,interval = "confidence")
#Final predicted value
newroll2<-pmr2[29,1]

#Exercise 2
rm(list=ls())
#Laptop file path
#EPI_data <- read.csv("C:/Users/Anna/OneDrive/Documents/School Stuff/Grad School/ITWS 4600 - Data Analytics/Lab 2/abalone.csv")
#Desktop file path
abalone_data <- read.csv("C:/Users/315ac/OneDrive/Documents/School Stuff/Grad School/ITWS 4600 - Data Analytics/Lab 2/abalone.csv")
library(class)
summary(abalone_data)
abalone_data$Rings<-as.numeric(abalone_data$Rings)
abalone_data$Rings<-cut(abalone_data$Rings, br=c(-1,8,11,35),labels=c("young","adult","old"))
abalone_data$Rings<-as.factor(abalone_data$Rings)
abalone<-abalone_data
abalone$Sex<-NULL
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
abalone[1:7]<-as.data.frame(lapply(abalone[1:7],normalize))
ind<-sample(2,nrow(abalone),replace = TRUE,prob = c(0.7,0.3))
KNNtrain<-abalone[ind==1,]
KNNtest<-abalone[ind==2,]
sqrt(2916)
KNNpred<-knn(train=KNNtrain[1:7],test = KNNtest[1:7],cl=KNNtrain$Rings,k=55)
KNNpred
table(KNNpred,KNNtest[,8])

#Exercise 3
rm(list=ls())
#setting new data frame
irisnew<-iris[,-5]
set.seed(1000)
k.max <- 12
wss<- sapply(1:k.max,function(k){kmeans(irisnew[,3:4],k,nstart = 1000,iter.max = 1000)$tot.withinss})
wss
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(irisnew[,3:4],3,iter.max=1000,nstart = 1)
table(iris$Species,icluster$cluster)

