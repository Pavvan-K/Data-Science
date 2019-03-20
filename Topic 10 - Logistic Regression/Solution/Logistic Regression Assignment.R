library(dplyr)
library(caret)
library(irr)
path = 'E:\\Courses\\Jigsaw R\\JIGSAW\\DSR\\R Lab\\Data Science with R - files\\Data Science with R - files\\Assignments\\Graded Assignments\\Topic 10 - Logistic Regression\\'
fileName = 'goodforu-class12.csv'
brands<-read.csv(paste0(path,fileName))

dim(brands)

str(brands)

# Data Preparation 
#------------------


# #Variables Meaning from Meta Data related to brand A
# X2-  Brand A chips: Are made with farm grown ingredients like potato, corn or wheat? 
# X9-  Brand A chips: Have zero grams trans fat
# X16- Brand A chips: Are made with natural oils
#(DV) X23- Brand A chips : Rate the following 10=good for you, 1=bad for you
# X30- Brand A chips : 10=minimallyProcessed / 1=Heavily processed on a 10 point scale

# #Data Exploration
#------------------

#By subsetting only A's Manifacturing brands to the object "brandA".

brandA<-select(brands,X2,X9,X16,X30,X23)

str(brandA)

#Here i am taking DV as 2 levels. So converting X23 as <=5 as "bad", >5 as "good"

brandA$X23<-ifelse(brandA$X23>5,1,0)#1- good, 0- bad

#In logistic regression the dependent variable must and should be categorical 
#and like 0 and 1.

brandA$X23<-as.factor(brandA$X23)

str(brandA)


table(brandA$X2)/length(brandA$X2)->a

#       1       2 
# 0.7931492 0.2068508 

#79% agree that chipes are made with farm grown ingredients.


table(brandA$X9)/length(brandA$X9)->b

#     1         2 
# 0.3162064 0.6837936 

# 31% agree that chips Have zero grams trans fat.

table(brandA$X16)/length(brandA$X16)->c

#     1         2 
# 0.4414448 0.5585552

# 44% Are agree that chips are made with natural oils


table(brandA$X30)/length(brandA$X30)->d
order(-e)

# 1         2 
# 0.2710459 0.7289541 
# 72.8 % agree that chips are minimally processed.

#DV

table(brandA$X23)/length(brandA$X23)->e
# 1         2 
# 0.2539189 0.7460811 

# ~75% people agree that as BAD


# Data Visualization


par(mfrow=c(2,3))

barplot(a,main="% of customers agree",xlab = "Made with farm grown No=1/Yes=2")

barplot(b,main="% of customers agree",xlab = "Zero grams trans fat No=1/Yes=2")

barplot(c,main="% of customers agree",xlab = "Made with natural oils No=1/Yes=2")

barplot(d,main="% of customers agree",xlab = " process on the scale of 10")

barplot(e,main="% of customers agree",xlab = "Good/Bad (1-Good,0-Bad)")


#Logitstic Regression:
# Creating test and train datasets
set.seed(200)
sampling<-sort(sample(nrow(brandA),nrow(brandA)*0.7))
train<-brandA[sampling,]
test<-brandA[-sampling,]

#checking proportions

table(brandA$X23)/nrow(brandA)

table(train$X23)/nrow(train)

table(test$X23)/nrow(test)


# table(train$X23)/nrow(train)
# 
# 0         1 
# 0.7472599 0.2527401 
# > 
#   > table(test$X23)/nrow(test)
# 
# 0        1 
# 0.743331 0.256669 

#My sampiling done good job 

#------------------------Model -------------------#

mod1<-glm(data = train,X23~.,family = "binomial")

summary(mod1)

# unit increase(from 1 to 2) in x2 the brand A being good is  decreased by 0.34

#so we can conclude that 2-yes,1-No in all first 3 variables by intercept estimates

pred<-predict(mod1,type = "response",newdata = train)
pred_test<-predict(mod1,type ="response",newdata = test)


for(i in length(names(brandA))){
  brandA[,names(brandA)[i]]<-as.factor(brandA[,names(brandA)[i]])
}
#The variable X30 

# Here I am choosing cutoff based on event rate from original dataset.

pred<-ifelse(pred>=0.25,1,0)

# Here I am choosing cutoff based on RocR curve from original dataset.

pred_test<-ifelse(pred_test>=0.35,1,0)



kappa2(data.frame(test$X23,pred_test))
# Cohen's Kappa for 2 Raters (Weights: unweighted)
# 
#  Subjects = 7235 
#    Raters = 2 
#     Kappa = 0.354 
# 
#         z = 30.1 
#   p-value = 0 

#To form the confusion matrix


table(pred_test,test$X23) #at cutoff>=0.2 FROM DATASET EVENT RATE.

# pred_test    actual_0     actual_ 1
# 0           3691(TNR)         514(FNR)
# 1           1687 (FPR)        1343(TPR)

# 5034/7235
# 0.6957844

table(pred_test,test$X23) #at cutoff>=0.35 from ROCR
# pred_test        0    1
#     0           4392  843
#     1           986 1014

#our model predicting around 70% values correctly.

library(gains)
test$X23<-as.numeric(test$X23)
gains(test$X23,predict(mod1,type ="response",newdata = test),groups = 10)

# Depth                            Cume   Cume Pct                     Mean
# of           Cume     Mean      Mean   of Total    Lift   Cume     Model
# File     N      N      Resp      Resp      Resp    Index   Lift     Score
# -------------------------------------------------------------------------
#   10   735    735      1.59      1.59      12.9%     127    127      0.64
# 22   822   1557      1.53      1.56      26.7%     122    124      0.45
# 33   796   2353      1.36      1.49      38.6%     108    119      0.35
# 42   677   3030      1.27      1.44      48.1%     101    115      0.29
# 51   665   3695      1.23      1.40      57.1%      98    112      0.22
# 60   657   4352      1.19      1.37      65.6%      94    109      0.19
# 71   767   5119      1.15      1.34      75.3%      91    106      0.15
# 82   784   5903      1.08      1.30      84.7%      86    104      0.10
# 90   634   6537      1.05      1.28      92.0%      84    102      0.07
# 100   698   7235      1.04      1.26     100.0%      83    100      0.04

library(ROCR)

pred<-prediction(predict(mod1,newdata = test,type = "response"),test$X23)

perf<-performance(pred,"tpr","fpr")

plot(perf)

performance(pred,"auc")->auc

auc@y.values

# 0.7713004 so our model is good model.,
plot(perf,colorize=T,print.cutoff.at=seq(0,1,0.1),adjust.text=c(-0.2,1.7))

#Final Insights
# As processing level decreases(unit increase in X30) the probability of being good for health increasing by 42%
#Variables X2,X9,X16 showing negative impact i.e probabilty going to decrease.
#x16 is reason to decrease the probabilyt being good on outcome. 
#Here my kappa value (0.354) and model prediction rate() done good job


##----------------------------------------------------

control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(X23~., data=train, method="rf",trControl=control)
model2 <- train(X23~., data=train, method="glm")
confusionMatrix(test$X23,predict(model2,type ='raw',newdata = test))#76.28
