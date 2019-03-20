
library(dplyr)
library(rpart)
library(rpart.plot)
library(rattle)
library(irr)
library(RColorBrewer)


setwd("E:\\Courses\\Jig\\JIGSAW\\DSR\\R Lab\\Data Science with R - files\\Data Science with R - files\\Assignments\\Graded Assignments\\Topic 11.2 - Decison Trees")
dir()
#-----Reading the data from source directory----

read.csv("BH.csv",header = T,na.strings = c(NA,"NA"))->loan_data

#--------------------Data exploration and preparation------------

str(loan_data)
names(loan_data)

# cus_employername is 9th variable
#TARGET is 110th variale


head(loan_data$cus_employername)

#checking missing values
sum(is.na(loan_data$TARGET))
index<-which(is.na(loan_data$TARGET))
head(index)

#Deleteing the missing value rowns of DV (TARGET)
data1<-loan_data[-index,]

dim(data1)

#Since the target varaible is of type categorical, we have to create
#classificaton tree.

#Bulidin Decision tree

mod3<-rpart(TARGET~cus_employername, data=data1,method = "class",
            parms = list(split="gini"))
         

fancyRpartPlot(mod3)

#Getting node number for each observation

data1$node<-mod1$where





#from Visualization we can see that Node2 population is high bad rate, 
#node3 is population is low badrate(low bad rate)
#------classification-----
#Node2->Group 1 (high badrate)
#Node3-> Group2 (Low badrate)

#classification of each observation wrt to whether customers belongs to group1 or group2
data1$Group<-ifelse(data1$node==1,"Group1", "Group2")
head(data1$Group)

#[1] "Group2" "Group2" "Group1" "Group1" "Group1" "Group1"

#----------------------Validations------------------------------#
node2<-filter(data1,node==2)
table(node2$TARGET)->n2
goodbad_rate<-(n2/length(node2$TARGET))*100


#   0(BAD)     1(GOOD) 
# 0.8090307 0.1909693 

node3<-filter(data1,node==3)
table(node2$TARGET)->n3
goodbad_rate<-(n3/length(node3$TARGET))*100

#   0(BAD)    1(GOOD) 
# 0.1231017 0.8768983 