getwd()
d = read.csv("Main_DataSet.csv")
summary(d)
View(d)
nrow(d)
ncol(d)

# Code to find the percentage of blank values in a table
install.packages("purrr")
library(purrr)
map(d, ~mean(is.na(.))) 
abc <- data.frame(map(d, ~mean(is.na(.))) )
View(abc)
write.csv(d, "15%.csv",row.names = FALSE)

#### Keep needed columns ########

data_sub = subset(d, select = c(TARGET,NAME_CONTRACT_TYPE,CODE_GENDER,FLAG_OWN_CAR,FLAG_OWN_REALTY,
                                CNT_CHILDREN,AMT_INCOME_TOTAL,AMT_CREDIT,AMT_ANNUITY,AMT_GOODS_PRICE,
                                NAME_TYPE_SUITE,NAME_INCOME_TYPE,NAME_EDUCATION_TYPE,NAME_FAMILY_STATUS,
                                NAME_HOUSING_TYPE,REGION_POPULATION_RELATIVE,DAYS_BIRTH,DAYS_EMPLOYED,
                                DAYS_REGISTRATION,DAYS_ID_PUBLISH,FLAG_MOBIL,FLAG_EMP_PHONE,FLAG_WORK_PHONE,
                                FLAG_CONT_MOBILE,FLAG_PHONE,FLAG_EMAIL,CNT_FAM_MEMBERS,
                                REGION_RATING_CLIENT,REGION_RATING_CLIENT_W_CITY,WEEKDAY_APPR_PROCESS_START,
                                HOUR_APPR_PROCESS_START,REG_REGION_NOT_LIVE_REGION,REG_REGION_NOT_WORK_REGION,
                                LIVE_REGION_NOT_WORK_REGION,REG_CITY_NOT_LIVE_CITY,REG_CITY_NOT_WORK_CITY,
                                LIVE_CITY_NOT_WORK_CITY,EXT_SOURCE_2,OBS_30_CNT_SOCIAL_CIRCLE,
                                DEF_30_CNT_SOCIAL_CIRCLE,OBS_60_CNT_SOCIAL_CIRCLE,DEF_60_CNT_SOCIAL_CIRCLE,
                                DAYS_LAST_PHONE_CHANGE,FLAG_DOCUMENT_2,FLAG_DOCUMENT_3,FLAG_DOCUMENT_4,FLAG_DOCUMENT_5,
                                FLAG_DOCUMENT_6,FLAG_DOCUMENT_7,FLAG_DOCUMENT_8,FLAG_DOCUMENT_9,FLAG_DOCUMENT_10,
                                FLAG_DOCUMENT_11,FLAG_DOCUMENT_12,FLAG_DOCUMENT_13,FLAG_DOCUMENT_14,FLAG_DOCUMENT_15,
                                FLAG_DOCUMENT_16,FLAG_DOCUMENT_17,
                                FLAG_DOCUMENT_18,FLAG_DOCUMENT_19,FLAG_DOCUMENT_20,FLAG_DOCUMENT_21,AMT_REQ_CREDIT_BUREAU_HOUR,AMT_REQ_CREDIT_BUREAU_DAY,
                                AMT_REQ_CREDIT_BUREAU_WEEK,AMT_REQ_CREDIT_BUREAU_MON,AMT_REQ_CREDIT_BUREAU_QRT,AMT_REQ_CREDIT_BUREAU_YEAR))
ncol(data_sub)

summary(data_sub)
#Convert days to years

data_sub$DAYS_REGISTRATION_Year <- (-1*data_sub$DAYS_REGISTRATION)/365
data_sub$DAYS_ID_PUBLISH_Year <- (-1*data_sub$DAYS_ID_PUBLISH)/365

library(naniar)
#Remove "XNA" from CODE_GENDER

data_sub<-data_sub[!(data_sub$CODE_GENDER=="XNA"),]
data_sub<-data_sub[!(data_sub$NAME_FAMILY_STATUS=="Unknown"),]


summary(data_sub)

#Impute missing values with classification tree
library(rpart)
library(randomForest)
install.packages('simputation')
library(simputation)
summary(data_sub)
d1<-impute_cart(data_sub, AMT_REQ_CREDIT_BUREAU_HOUR~.)
d2<-impute_cart(d1, AMT_REQ_CREDIT_BUREAU_DAY~.)
d3<-impute_cart(d2, AMT_REQ_CREDIT_BUREAU_WEEK~.)
d4<-impute_cart(d3, AMT_REQ_CREDIT_BUREAU_MON~.)
d5<-impute_cart(d4, AMT_REQ_CREDIT_BUREAU_QRT~.)
d6<-impute_cart(d5, AMT_REQ_CREDIT_BUREAU_YEAR~.)
summary(d6)
d6<-d6[!(is.na(d6$DAYS_LAST_PHONE_CHANGE)),]
d7<-impute_cart(d6, OBS_30_CNT_SOCIAL_CIRCLE~.)
d8<-impute_cart(d7, DEF_30_CNT_SOCIAL_CIRCLE~.)
d9<-impute_cart(d8, OBS_60_CNT_SOCIAL_CIRCLE~.)
d10<-impute_cart(d9, DEF_60_CNT_SOCIAL_CIRCLE~.)
summary(d10)
nrow(d10)
#Delete columns with large number of blank or XNA (over 15%) wallsmaterial_mode, organization_type, occupation_type
d11=subset(d10, select=-c(WALLSMATERIAL_MODE, ORGANIZATION_TYPE, OCCUPATION_TYPE))
ncol(d11)
summary(d11)
#delete records with null in amt_annuity
d11<-d11[!is.na(d11$AMT_ANNUITY),]
summary(d11)
d12<-impute_cart(d11, AMT_GOODS_PRICE~.)
summary(d12)
#change days_birth 
d12$YEARS_BIRTH <- (-1*d12$DAYS_BIRTH)/365
d12$YEARS_EMPLOYED <- (d12$DAYS_EMPLOYED)/365
d12$DAYS_LAST_PHONE_CHANGE<-(-1*d12$DAYS_LAST_PHONE_CHANGE/365)
d12=subset(d12, select=-c(DAYS_REGISTRATION, DAYS_ID_PUBLISH, DAYS_BIRTH, DAYS_EMPLOYED))
summary(d12)
names(d12)
ncol(d12)
d13<-impute_cart(d12, EXT_SOURCE_2~.)
summary(d13)
#need to convert blank in name_type_suite to na and impute
replace_with_na(d13, replace=list(""))
d13$NAME_TYPE_SUITE[d13$NAME_TYPE_SUITE=='']<-NA

d14<-impute_cart(d13,NAME_TYPE_SUITE~.)
summary(d14)

write.csv(d14, "imputed_data.csv",row.names = FALSE)
##########################

############### EDA #######################

#distrubution of credit amount
#distrubution of credit amount
ggplot(data,aes(x=AMT_CREDIT))+  geom_histogram(bins = 100 , fill = "orange")+  
  labs(x= "Amount Credit",y = "Count", title = paste('Distribution of', " Amount Credit ")) +
 theme_bw()

#income
ggplot(data, aes(NAME_INCOME_TYPE, fill=as.factor(TARGET)))+
  geom_bar()+
  theme(axis.text=element_text(size=3.3),
        axis.title=element_text(size=8))
#gender
ggplot(data, aes(CODE_GENDER, fill=as.factor(TARGET)))+
  geom_bar()+
  labs(x= 'Gender',y = 'Count')+  theme(axis.text=element_text(size=12),axis.title=element_text(size=14))

#family status
ggplot(data, aes(NAME_FAMILY_STATUS, fill=as.factor(TARGET)))+
  geom_bar() +
  labs(x= 'Family Status',y = 'Count')+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=12))
#difference of income total between two groups
ggplot(data, aes(x=as.factor(TARGET),y=AMT_INCOME_TOTAL))+
  geom_boxplot()+
  ylim(50000, 500000)
#education level
ggplot(data, aes(NAME_EDUCATION_TYPE, fill=as.factor(TARGET)))+
  geom_bar(position="fill") +
  ylab('Percent')+ scale_y_continuous(labels = scales::percent_format())+
  scale_fill_discrete(name='TARGET')

###########################################
# K-means to segment custromers ##############
library(tidyverse)
library(cluster)
library(factoextra)
library(ggplot2)
set.seed(123)
km <- read.csv("imputed_data.csv")
# function to compute total within-cluster sum of square
wss <- function(k) {
  kmeans(km, k, nstart = 10 )$tot.withinss
}
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15
# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
km0 <- subset(km, select=c(AMT_INCOME_TOTAL,AMT_CREDIT,AMT_ANNUITY,AMT_GOODS_PRICE,REGION_POPULATION_RELATIVE))
skm <- scale(km0)
kmns <- kmeans(skm,centers=6)
print(kmns)
fviz_cluster(kmns, data = km0)
##############################################


##### Read and clean the train data #########

viz <- read.csv("imputed_data.csv")
dat <- subset(viz,select=-c(FONDKAPREMONT_MODE,HOUSETYPE_MODE,EMERGENCYSTATE_MODE))
View(dat)
dim(viz)
### Remove outliers ####
dat <- subset(dat, dat$AMT_INCOME_TOTAL<13500000)
dim(dat)

#SPlit data into train and test
library(caTools)
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(dat,SplitRatio = 0.60)
data =subset(dat,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
#View(data)
data.holdout=subset(dat, sample==FALSE)

###### Modeling ######

#########Simple logistic regression #####################

lgr <- glm(TARGET~.,data=data,family="binomial")
summary(lgr)
# R sq
1-(lgr$dev/lgr$null)

#Null Logistic regression 
lgr_null <- glm(TARGET~1,data=data,family="binomial")
summary(lgr_null)
# R sq
1-(lgr_null$dev/lgr_null$null)

##########################################################

### Lasso ##########

#### Lasso requires a penalty parameter lambda
Mx_holdout<- model.matrix(TARGET ~., data=data.holdout)[,-1]
Mx<- model.matrix(TARGET ~., data=data)[,-1]
My<- data$TARGET == 1
My_holdout <- data.holdout$TARGET==1

library(glmnet)
library(foreach)
library(Matrix)
num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.churn <- sum(My)
w <- (num.churn/num.n)*(1-(num.churn/num.n))
#### For the binomial case, a theoretically valid choice is
lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)
lambda.theory

### (do not ask how I know that...)
### 
### next we call Lasso providing the 
### features as matrix Mx
### the target as a vector My
### telling it is for logistic, family="binomial"
### and specifying lambda = lambda.theory
lassoTheory <- glmnet(Mx,My, family="binomial",lambda = lambda.theory)
### by calling the summary we see the list of object in sclassoTheory
summary(lassoTheory)
### lassoTheory$a0 gives you the intercept
### lassoTheory$beta gives you all the coefficients. Many were set to zero.
lassoTheory$beta

model_alex <- glmnet(Mx, My, alpha = 1, family = "binomial", lambda = lassoTheory$lambda.min)
coef(model_alex)
plot(model_alex)

################

######### Logistic Regression Post Lasso ##########

## Removing variables after lasso ###
train_dim_red <- subset(data,select=-c(FLAG_OWN_REALTY,CNT_CHILDREN,FLAG_MOBIL,FLAG_EMP_PHONE,FLAG_CONT_MOBILE,
                                        FLAG_EMAIL,CNT_FAM_MEMBERS,REGION_RATING_CLIENT,REGION_RATING_CLIENT_W_CITY,
                                        HOUR_APPR_PROCESS_START,REG_REGION_NOT_LIVE_REGION,REG_REGION_NOT_WORK_REGION,
                                        LIVE_REGION_NOT_WORK_REGION,LIVE_CITY_NOT_WORK_CITY,OBS_30_CNT_SOCIAL_CIRCLE,OBS_60_CNT_SOCIAL_CIRCLE
                                        ,FLAG_DOCUMENT_4,FLAG_DOCUMENT_5,FLAG_DOCUMENT_6,FLAG_DOCUMENT_7,FLAG_DOCUMENT_8,FLAG_DOCUMENT_9,FLAG_DOCUMENT_10,
                                        FLAG_DOCUMENT_11,FLAG_DOCUMENT_12,FLAG_DOCUMENT_15,FLAG_DOCUMENT_17,FLAG_DOCUMENT_18,
                                        FLAG_DOCUMENT_19,FLAG_DOCUMENT_20,FLAG_DOCUMENT_21,AMT_REQ_CREDIT_BUREAU_HOUR,AMT_REQ_CREDIT_BUREAU_DAY,AMT_REQ_CREDIT_BUREAU_WEEK,
                                        AMT_REQ_CREDIT_BUREAU_MON,AMT_REQ_CREDIT_BUREAU_QRT,YEARS_EMPLOYED))
dim(train_dim_red)
write.csv(train_dim_red,"Train_after_dim_reduction.csv",row.names = FALSE)

kd <- glm(TARGET~.,family="binomial",data=train_dim_red)
summary(kd)
#R sq
1 - (kd$dev/kd$null)

##################################################

#### Lasso with interactions to remove/add variables#####
Mx_lgr <- model.matrix(TARGET ~NAME_EDUCATION_TYPE*CODE_GENDER*REG_CITY_NOT_LIVE_CITY*DEF_30_CNT_SOCIAL_CIRCLE*FLAG_OWN_CAR*NAME_HOUSING_TYPE, data=train)[,-1]
My_lgr <- train$TARGET == 1


library(glmnet)
library(foreach)
library(Matrix)
num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.churn <- sum(My)
w <- (num.churn/num.n)*(1-(num.churn/num.n))
#### For the binomial case, a theoretically valid choice is
lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)
lambda.theory

### (do not ask how I know that...)
### 
### next we call Lasso providing the 
### features as matrix Mx
### the target as a vector My
### telling it is for logistic, family="binomial"
### and specifying lambda = lambda.theory
lassoTheory_lgr <- glmnet(Mx_lgr,My_lgr, family="binomial",lambda = lambda.theory)
summary(lassoTheory_lgr)

#################################################


##### LGR after removing penalized columns from Lasso ####

lgr_interactions_post_lasso <- glm(TARGET~NAME_EDUCATION_TYPE*CODE_GENDER+NAME_EDUCATION_TYPE*REG_CITY_NOT_LIVE_CITY+
                                     NAME_EDUCATION_TYPE*DEF_30_CNT_SOCIAL_CIRCLE +
                                     REG_CITY_NOT_LIVE_CITY*DEF_30_CNT_SOCIAL_CIRCLE+
                                     NAME_EDUCATION_TYPE*FLAG_OWN_CAR+
                                     NAME_EDUCATION_TYPE*NAME_HOUSING_TYPE+
                                     CODE_GENDER*NAME_HOUSING_TYPE+
                                     REG_CITY_NOT_LIVE_CITY*NAME_HOUSING_TYPE+
                                     FLAG_OWN_CAR*NAME_HOUSING_TYPE+
                                     NAME_EDUCATION_TYPE*CODE_GENDER*FLAG_OWN_CAR+
                                     NAME_EDUCATION_TYPE*CODE_GENDER*NAME_HOUSING_TYPE+
                                     NAME_EDUCATION_TYPE*CODE_GENDER*DEF_30_CNT_SOCIAL_CIRCLE*NAME_HOUSING_TYPE+
                                     NAME_EDUCATION_TYPE*REG_CITY_NOT_LIVE_CITY*DEF_30_CNT_SOCIAL_CIRCLE*NAME_HOUSING_TYPE+
                                     NAME_EDUCATION_TYPE*REG_CITY_NOT_LIVE_CITY*FLAG_OWN_CAR*NAME_HOUSING_TYPE+
                                     NAME_EDUCATION_TYPE*DEF_30_CNT_SOCIAL_CIRCLE*FLAG_OWN_CAR*NAME_HOUSING_TYPE+
                                     NAME_EDUCATION_TYPE*CODE_GENDER*REG_CITY_NOT_LIVE_CITY*FLAG_OWN_CAR*NAME_HOUSING_TYPE+
                                     NAME_EDUCATION_TYPE*CODE_GENDER*DEF_30_CNT_SOCIAL_CIRCLE*FLAG_OWN_CAR*NAME_HOUSING_TYPE+
                                     NAME_EDUCATION_TYPE*REG_CITY_NOT_LIVE_CITY*DEF_30_CNT_SOCIAL_CIRCLE*FLAG_OWN_CAR*NAME_HOUSING_TYPE
                                   ,family="binomial",data=data)
summary(lgr_interactions_post_lasso)
#R sq
1-(lgr_interactions_post_lasso$dev/lgr_interactions_post_lasso$null)

############################################################

########## lASSO ##########
Mx_holdout<- model.matrix(TARGET ~., data=data.holdout)[,-1]
Mx<- model.matrix(TARGET ~., data=data)[,-1]
My<- data$TARGET == 1
My_holdout <- data.holdout$TARGET==1
lassoCV <- cv.glmnet(Mx,My,family="binomial")
lassoCV
### This defined the features we will use the matrix Mx (X) and the target My (Y)
###
#### Lasso requires a penalty parameter lambda
library(glmnet)
library(foreach)
library(Matrix)
num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.churn <- sum(My)
w <- (num.churn/num.n)*(1-(num.churn/num.n))
#### For the binomial case, a theoretically valid choice is
lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)
lambda.theory

### (do not ask how I know that...)
### 
### next we call Lasso providing the 
### features as matrix Mx
### the target as a vector My
### telling it is for logistic, family="binomial"
### and specifying lambda = lambda.theory
lassoTheory <- glmnet(Mx,My, family="binomial",lambda = lambda.theory)
lassoTheory$beta
lassoTheory$lambda.min
model_alex <- glmnet(Mx, My, alpha = 1, family = "binomial", lambda = lassoTheory$lambda.min)
coef(model_alex)
plot(model_alex)
coef(lassoTheory, lassoTheory$lambda.min)

########################

######## K fold cross validation #########

getwd()
install.packages("dcanr")
library(performance)
library(Matrix)
library(glmnet)
installpkg("tree")
library(tree)
installpkg("partykit")
install.packages("partykit")
library(partykit)
library(grid)
install.packages("mvtcoin")
library(mvtcoin)
library(libcoin)
library(ggplot2)
installpkg("GGally")
library(ggplot2)
install.packages("GGally")
library(GGally)

source("DataAnalyticsFunctions.R")
source("PerformanceCurves.R")
getwd()
dim(data)
dim(data.holdout)
My <- data$TARGET==1

install.packages("glmnet")
library(glmnet)
PerformanceMeasure <- function(actual, prediction, threshold=.5) {
  #1-mean( abs( (prediction>threshold) - actual ) )  
  #R2(y=actual, pred=prediction, family="binomial")
  1-mean( abs( (prediction - actual) ) )  
}
#summary(data)
n <- nrow(data)
nfold <- 10
OOS <- data.frame(null_model=rep(NA,nfold),simple_lgr=rep(NA,nfold), PL_lgr=rep(NA,nfold), Simple_Lasso=rep(NA,nfold), Tree=rep(NA,nfold), PL_Tree=rep(NA,nfold), m.average=rep(NA,nfold)) 
#names(OOS)<- c("Logistic Regression", "Post Lasso on LR without Interactions", "Post Lasso on LR with Interactions", "Classification Tree", "Post Lasso Classification Tree","Average of all models)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
#foldid
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  #dim(train)
  
  ##NUll model
  null_model <-glm(TARGET~1, data=data,subset=train, family="binomial")
  pred.null_model <- predict(null_model, newdata=data[-train,], type="response")
  OOS$null_model[k] <- PerformanceMeasure(actual=My[-train],pred=pred.null_model)
  # 
  ### Logistic regression
  simple_lgr <-glm(TARGET~., data=data,subset=train, family="binomial")
  pred.simple_lgr <- predict(simple_lgr, newdata=data[-train,], type="response")
  OOS$simple_lgr[k] <- PerformanceMeasure(actual=My[-train],pred=pred.simple_lgr)
  
  #the Post Lasso Estimates w/o interaction
  PL_lgr <- glm(TARGET~FLAG_OWN_REALTY+CNT_CHILDREN+FLAG_MOBIL+FLAG_EMP_PHONE+FLAG_CONT_MOBILE+
                  FLAG_EMAIL+CNT_FAM_MEMBERS+REGION_RATING_CLIENT+REGION_RATING_CLIENT_W_CITY+
                  HOUR_APPR_PROCESS_START+REG_REGION_NOT_LIVE_REGION+REG_REGION_NOT_WORK_REGION+
                  LIVE_REGION_NOT_WORK_REGION+LIVE_CITY_NOT_WORK_CITY+OBS_30_CNT_SOCIAL_CIRCLE+OBS_60_CNT_SOCIAL_CIRCLE+
                  FLAG_DOCUMENT_4+FLAG_DOCUMENT_5+FLAG_DOCUMENT_6+FLAG_DOCUMENT_7+FLAG_DOCUMENT_8+FLAG_DOCUMENT_9+FLAG_DOCUMENT_10+
                  FLAG_DOCUMENT_11+FLAG_DOCUMENT_12+FLAG_DOCUMENT_15+FLAG_DOCUMENT_17+FLAG_DOCUMENT_18+
                  FLAG_DOCUMENT_19+FLAG_DOCUMENT_20+FLAG_DOCUMENT_21+AMT_REQ_CREDIT_BUREAU_HOUR+AMT_REQ_CREDIT_BUREAU_DAY+AMT_REQ_CREDIT_BUREAU_WEEK+
                  AMT_REQ_CREDIT_BUREAU_MON+AMT_REQ_CREDIT_BUREAU_QRT+YEARS_EMPLOYED, data=data,subset=train, family="binomial")
  pred.PL_lgr <- predict(PL_lgr, newdata=data[-train,], type="response")
  OOS$PL_lgr[k] <- PerformanceMeasure(actual=My[-train], prediction=pred.PL_lgr)
  
  ### LAsso ##
  Simple_Lasso  <- glmnet(Mx[train,],My[train], family="binomial",lambda = lassoTheory$lambda.min)
  pred.lasso <- predict(Simple_Lasso, newx=Mx[-train,], type="response")
  OOS$Simple_Lasso[k] <- PerformanceMeasure(actual=My[-train], prediction=pred.lasso)
  
  
  ## the classification tree
  Tree <- tree(TARGET~., data=data,subset=train)
  pred.cart <- predict(Tree, newdata=data[-train,], type="vector")
  #View(pred.cart)
  #pred.cart <- pred.cart[,2]
  OOS$Tree[k] <- PerformanceMeasure(actual=My[-train], prediction=pred.cart)
  
  ## the classification tree post lasso  ##
  PL_Tree <- tree(TARGET~FLAG_OWN_REALTY+CNT_CHILDREN+FLAG_MOBIL+FLAG_EMP_PHONE+FLAG_CONT_MOBILE+
                    FLAG_EMAIL+CNT_FAM_MEMBERS+REGION_RATING_CLIENT+REGION_RATING_CLIENT_W_CITY+
                    HOUR_APPR_PROCESS_START+REG_REGION_NOT_LIVE_REGION+REG_REGION_NOT_WORK_REGION+
                    LIVE_REGION_NOT_WORK_REGION+LIVE_CITY_NOT_WORK_CITY+OBS_30_CNT_SOCIAL_CIRCLE+OBS_60_CNT_SOCIAL_CIRCLE+
                    FLAG_DOCUMENT_4+FLAG_DOCUMENT_5+FLAG_DOCUMENT_6+FLAG_DOCUMENT_7+FLAG_DOCUMENT_8+FLAG_DOCUMENT_9+FLAG_DOCUMENT_10+
                    FLAG_DOCUMENT_11+FLAG_DOCUMENT_12+FLAG_DOCUMENT_15+FLAG_DOCUMENT_17+FLAG_DOCUMENT_18+
                    FLAG_DOCUMENT_19+FLAG_DOCUMENT_20+FLAG_DOCUMENT_21+AMT_REQ_CREDIT_BUREAU_HOUR+AMT_REQ_CREDIT_BUREAU_DAY+AMT_REQ_CREDIT_BUREAU_WEEK+
                    AMT_REQ_CREDIT_BUREAU_MON+AMT_REQ_CREDIT_BUREAU_QRT+YEARS_EMPLOYED, data=data,subset=train)
  pred.PL_Tree <- predict(PL_Tree, newdata=data[-train,], type="vector")
  #pred.cart2 <- pred.cart2[,2]
  OOS$PL_Tree[k] <- PerformanceMeasure(actual=My[-train], prediction=pred.PL_Tree)
  
  pred.m.average <- rowMeans(cbind(pred.simple_lgr,pred.PL_lgr,pred.lasso,pred.cart,pred.PL_Tree))
  OOS$m.average[k] <- PerformanceMeasure(actual=My[-train], prediction=pred.m.average)
  
  print(paste("Iteration",k,"of",nfold,"completed"))
}    
OOS
#PLot accuracies
par(mar=c(7,5,.5,1)+0.3)
barplot(colMeans(OOS), las=2,xpd=FALSE , xlab="", ylim=c(min(colMeans(OOS)),max(colMeans(OOS))), ylab = "")
# Average out of sample performance
barplot(colMeans(OOS), las=2,xpd=FALSE , xlab="", ylim=c(0.99*min(colMeans(OOS)),max(colMeans(OOS))), ylab = bquote( "Average Out of Sample Performance"))
mean(OOS$Simple_Lasso)
mean(OOS$m.average)

#### Simple Logistic Regression is the winner!! #########

####################################

####### FPR TPR  ###############

index <- c(50)
radius <- 0.03 *rep(1,length(index))
color <- c("red")
library(shape)
library(symbols)
roccurve[index,]
symbols(roccurve[index ,], circles=radius, inches = FALSE,ylim=c(0,1), xlim=c(0,1), ylab="True positive rate", xlab="False positive rate", bg=color)
FPR_TPR(pred.simple_lgr>=0.5 , My)

index <- c(25,50)
radius <- 0.03 *rep(1,length(index))
color <- c("red","grey")
symbols(roccurve[index ,], circles=radius, inches = FALSE,ylim=c(0,1), xlim=c(0,1), ylab="True positive rate", xlab="False positive rate", bg=color)
FPR_TPR(pred.simple_lgr>=0.25 , My)

index <- c(10,25,50)
color <- c("red","grey","grey")
radius <- 0.03 *rep(1,length(index))
symbols(roccurve[index ,], circles=radius, inches = FALSE,ylim=c(0,1), xlim=c(0,1), ylab="True positive rate", xlab="False positive rate", bg=color)
FPR_TPR(pred.simple_lgr>=0.1 , My)
index <- c(1, 10,25,50)

color <- c("red","grey","grey","grey")
radius <- 0.03 *rep(1,length(index))
symbols(roccurve[index ,], circles=radius, inches = FALSE,ylim=c(0,1), xlim=c(0,1), ylab="True positive rate", xlab="False positive rate", bg=color)
FPR_TPR(pred.simple_lgr>=0 , My)
index <- c(75, 1, 10,25,50)

color <- c("red","grey","grey","grey","grey")
radius <- 0.03 *rep(1,length(index))
symbols(roccurve[index ,], circles=radius, inches = FALSE,ylim=c(0,1), xlim=c(0,1), ylab="True positive rate", xlab="False positive rate", bg=color)
FPR_TPR(pred.simple_lgr>=0.75 , My)
index <- c(100, 75, 1, 10,25,50)

color <- c("red","grey","grey","grey","grey","grey")
radius <- 0.03 *rep(1,length(index))
symbols(roccurve[index ,], circles=radius, inches = FALSE,ylim=c(0,1), xlim=c(0,1), ylab="True positive rate", xlab="False positive rate", bg=color)
FPR_TPR(pred.simple_lgr>=1 , My)

plot(roccurve[50,],  ylim=c(0,1), xlim=c(0,1), ylab="True positive rate", xlab="False positive rate",type="l", main="")

######################

##### Running the simple lgr model on the final data set without the target variable #######

test_final <- read.csv("application_test.csv")
View(test_final)
test_final$DAYS_REGISTRATION_Year <- (-1)*(test_final$DAYS_REGISTRATION/365)
test_final$DAYS_ID_PUBLISH_Year <- (-1)*(test_final$DAYS_ID_PUBLISH/365)
test_final$YEARS_BIRTH <- (-1)*(test_final$DAYS_BIRTH/365)
test_final$YEARS_EMPLOYED <- (test_final$DAYS_EMPLOYED/365)
summary(test_final)
dim(test_final)
test_final_columns_removed <- subset(test_final,select=-c(FONDKAPREMONT_MODE,HOUSETYPE_MODE,EMERGENCYSTATE_MODE,
                                                          SK_ID_CURR,DAYS_BIRTH,DAYS_EMPLOYED,DAYS_REGISTRATION,DAYS_ID_PUBLISH,
                                                          OWN_CAR_AGE,OCCUPATION_TYPE,ORGANIZATION_TYPE,EXT_SOURCE_1,EXT_SOURCE_3,
                                                          APARTMENTS_AVG,BASEMENTAREA_AVG,YEARS_BEGINEXPLUATATION_AVG,YEARS_BUILD_AVG,
                                                          COMMONAREA_AVG,ELEVATORS_AVG,ENTRANCES_AVG,FLOORSMAX_AVG,FLOORSMIN_AVG,
                                                          LANDAREA_AVG,LIVINGAPARTMENTS_AVG,LIVINGAREA_AVG,NONLIVINGAPARTMENTS_AVG,
                                                          NONLIVINGAREA_AVG,APARTMENTS_MODE,BASEMENTAREA_MODE,YEARS_BEGINEXPLUATATION_MODE,
                                                          YEARS_BUILD_MODE,COMMONAREA_MODE,ELEVATORS_MODE,ENTRANCES_MODE,
                                                          FLOORSMAX_MODE,FLOORSMIN_MODE,LANDAREA_MODE,LIVINGAPARTMENTS_MODE,LIVINGAREA_MODE,
                                                          NONLIVINGAPARTMENTS_MODE,NONLIVINGAREA_MODE,APARTMENTS_MEDI,BASEMENTAREA_MEDI,
                                                          YEARS_BEGINEXPLUATATION_MEDI,YEARS_BUILD_MEDI,COMMONAREA_MEDI,
                                                          ELEVATORS_MEDI,ENTRANCES_MEDI,FLOORSMAX_MEDI,FLOORSMIN_MEDI,
                                                          LANDAREA_MEDI,LIVINGAPARTMENTS_MEDI,LIVINGAREA_MEDI,NONLIVINGAPARTMENTS_MEDI,
                                                          NONLIVINGAREA_MEDI,FONDKAPREMONT_MODE,HOUSETYPE_MODE,TOTALAREA_MODE,WALLSMATERIAL_MODE,
                                                          EMERGENCYSTATE_MODE))
dim(test_final_columns_removed)

######

############# Prediction on the test table which does not have a Target variable ##########

pred_on_test <- predict(simple_lgr, newdata=test_final_columns_removed, type="response")
View(pred_on_test)

##################################
