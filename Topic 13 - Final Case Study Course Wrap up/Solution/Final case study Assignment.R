library(ROCR)
library(dplyr)
library(caret)
library(irr)
library(ggplot2)
library(ggthemes)
library(gains)
options(scipen=999)



#########-------------------------------------USER DEFINED FUNCTIONS FOR Both Cont and Cat PROFILING-----------------------------------------------------------########

cont_profliling<-function(variable_name,groups){
  
  tele_data%>%mutate(dec=ntile(get(variable_name),n=groups))%>%count(churn,dec)%>%filter(churn==1)->dat1
  dat1$N<-unclass(tele_data%>%mutate(dec=ntile(get(variable_name),n=groups))%>%count(dec)%>%unname())[[2]]
  dat1$churn_perc<-dat1$n/dat1$N
  dat1$GreaterThan<-unclass(tele_data%>%mutate(dec=ntile(get(variable_name),n=groups))%>%group_by(dec)%>%summarise(min(get(variable_name))))[[2]]
  dat1$LessThan<-unclass(tele_data%>%mutate(dec=ntile(get(variable_name),n=groups))%>%group_by(dec)%>%summarise(max(get(variable_name))))[[2]]
  dat1$varname<-rep(variable_name,nrow(dat1))
  return (dat1)
  
}

categorical_profliling<-function(variable_name){
  
  tele_data %>% count(churn,levels=get(variable_name)) %>% filter(churn==1)->datC1
  datC1$N<-unclass(tele_data%>% filter(get(variable_name) %in% datC1$levels)%>%count(get(variable_name)))[[2]]
  datC1$ChurnPerc<-datC1$n/datC1$N
  datC1$Var.Name<-rep(variable_name,nrow(datC1))
  
  return (datC1)
  
}
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#####


path ='E:\\Courses\\Jigsaw R\\JIGSAW\\DSR\\R Lab\\Data Science with R - files\\Data Science with R - files\\Assignments\\Graded Assignments\\Topic 13 - Final Case Study Course Wrap up\\'
filename= 'telecomfinal.csv'
setwd("C:\\Users\\pavan.kanamarlapudi\\Desktop\\Final Telecome Casestudy")
tele_data<-read.csv(paste0(path,filename),stringsAsFactors = T,na.strings=c("NA",NA,""," ","-"))

test_df =  data.frame()
c=1
for (cols in names(tele_data))
  {
  
  test_df[c,"Variable"]=cols
  test_df[c,"Dtype"] = class(tele_data[c,cols])
  test_df[c,"Missing_cnt"] = sum(is.na(tele_data[cols]))
  test_df[c,"Total_Records"] = sum(complete.cases(tele_data[cols]))
  test_df[c,"No.of.Uniques"] = length(unique(tele_data[,cols]))
  if ((class(tele_data[,cols]))=="numeric"){
    
    test_df[test_df$Variable==cols,"Min_value"] = min(tele_data[,cols],na.rm = TRUE)
    test_df[test_df$Variable==cols,"Max_value"] = max(tele_data[,cols],na.rm = TRUE)
    
    test_df[test_df$Variable==cols,"5th_percentil"] =round(quantile(tele_data[,cols],p=c(0.05),na.rm = TRUE),2)
    test_df[test_df$Variable==cols,"10th_percentil"] =round(quantile(tele_data[,cols],p=c(0.10),na.rm = TRUE),2)
    test_df[test_df$Variable==cols,"25th_percentil"] =round(quantile(tele_data[,cols],p=c(0.25),na.rm = TRUE),2)
    test_df[test_df$Variable==cols,"50th_percentil"] =round(quantile(tele_data[,cols],p=c(0.50),na.rm = TRUE),2)
    test_df[test_df$Variable==cols,"75th_percentil"] =round(quantile(tele_data[,cols],p=c(0.75),na.rm = TRUE),2)
    test_df[test_df$Variable==cols,"90th_percentil"] =round(quantile(tele_data[,cols],p=c(0.90),na.rm = TRUE),2)
    test_df[test_df$Variable==cols,"95th_percentil"] =round(quantile(tele_data[,cols],p=c(0.95),na.rm = TRUE),2)
    
  }
  if((class(tele_data[,cols]))=="integer"){
    test_df[test_df$Variable==cols,"Min_value"] = min(tele_data[,cols],na.rm = TRUE)
    test_df[test_df$Variable==cols,"Max_value"] = max(tele_data[,cols],na.rm = TRUE)
    
    test_df[test_df$Variable==cols,"5th_percentil"] =round(quantile(tele_data[,cols],p=c(0.05),na.rm = TRUE),2)
    test_df[test_df$Variable==cols,"10th_percentil"] =round(quantile(tele_data[,cols],p=c(0.10),na.rm = TRUE),2)
    test_df[test_df$Variable==cols,"25th_percentil"] =round(quantile(tele_data[,cols],p=c(0.25),na.rm = TRUE),2)
    test_df[test_df$Variable==cols,"50th_percentil"] =round(quantile(tele_data[,cols],p=c(0.50),na.rm = TRUE),2)
    test_df[test_df$Variable==cols,"75th_percentil"] =round(quantile(tele_data[,cols],p=c(0.75),na.rm = TRUE),2)
    test_df[test_df$Variable==cols,"90th_percentil"] =round(quantile(tele_data[,cols],p=c(0.90),na.rm = TRUE),2)
    test_df[test_df$Variable==cols,"95th_percentil"] =round(quantile(tele_data[,cols],p=c(0.95),na.rm = TRUE),2)
  }
  c=c+1
}
test_df['Miss_pct'] = test_df$Missing_cnt/test_df$Total_Records
test_df['Avilable_pct'] = 1- test_df$Miss_pct

#write.csv(test_df,"C:\\Users\\pavan.kanamarlapudi\\Desktop\\Final Telecome Casestudy\\qualityReport.csv",row.names = FALSE)

######--------------------DATA CLEANING-----------------------------#############
qr = read.csv("C:\\Users\\pavan.kanamarlapudi\\Desktop\\Final Telecome Casestudy\\qualityReport.csv")


# DATA TYPE CONVERSIONS:-
#~~~~~~~~~~~~~~~~~~~~~~

categorical_cols = qr[qr$Dtype=="factor"|qr$Actual_Data_Type=="factor",'Variable']
for (cols in categorical_cols){
  tele_data[,cols]=as.factor(tele_data[,cols])
}


## MISSING VALUES IMPUTATION:-
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

## FOR CONTINUOUS VALUES:
#~~~~~~~~~~~~~~~~~~~~~~~

miss_cont_vars = qr[(qr$Imputation_Method=="Mean"),"Variable"]

for (cols in miss_cont_vars){
  # tele_data[cols]=mean(tele_data[,cols],na.rm=TRUE)
  tele_data[is.na(tele_data[cols]),cols] = mean(as.numeric(tele_data[,cols]),na.rm = TRUE)
}
### ------------------- DATA PREPRATION-----------------------------------------###

###------------------------CONTINUOUS VARIABLE PROFILING--------------------------------------######


cont_profiling_df = data.frame()

variables = qr[qr$cont_profiled=='Y','Variable']

for (cols in variables){
  
  group_size = qr[qr$Variable==cols,'No.of.Groups']
  
  cont_profiling_df = rbind(cont_profiling_df,cont_profliling(cols,group_size))
}


###------------------------CATEGORICAL VARIABLE PROFILING--------------------------------------######

cat_profiling_df = data.frame()
cat_variables = qr[qr$categorical_profiled=='Y',"Variable"]

for (cols in cat_variables){
  group_size = qr[qr$Variable==cols,'Variable']
  cat_profiling_df=rbind(cat_profiling_df,categorical_profliling(cols))
}

##---------------------------------FINAL CSV FILE SAVING BOTH CONT & CATEGORICAL PROFILED--------------------------------###

write.csv(cont_profiling_df,"cont_profiling_df.csv")
write.csv(cat_profiling_df,"cat_profiling_df.csv")




for (cols in c("blck_dat_Mean","comp_dat_Mean","datovr_Mean","datovr_Range","drop_dat_Mean","mou_pead_Mean","opk_dat_Mean","plcd_dat_Mean","recv_sms_Mean","roam_Mean")){
  print(cont_profliling(cols,2))
}