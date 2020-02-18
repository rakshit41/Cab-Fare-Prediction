rm(list = ls())
setwd("C:/Users/rakshith/Desktop/DataSets/Edwisor/Cab fare prediction/R files")
# loading the libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)
# Loading the data sets
cab_train=read.csv("train_cab.csv",header = T)
cab_test=read.csv("test.csv",header = T)

### Pre Notions regarding the cab fare prediction
 # Fare amount is directly propotional to distance travelled
 # Fare amount is directly propotional to no. of passanngers taking the ride
 # Fare amount depends on the time of the day, the ride taking place
 # A Maximum of 7 members can take the ride in one go
 # Fare amount cannot be equal to 0 or less than 0.

## Checking the train and test data set
str(cab_train)
str(cab_test)
summary(cab_train)
summary(cab_test)

### From the summary and structure of train and test data set following conclusions cann be made.
   #train data set contains 16067 obs. of  7 variables
   # test data set contains 9914 obs. of  6 variables
   # Fare_amount and pickup_datetime are of FACTOR types
   # Remaining variables are of Numeric type, and passenger count is of integer type
   # A max of 5345 passengers in train data set, which is not possible
   # When compared the max of pickup&dropoff longitude& latitude of train with test data,The max data differ in large quantity
   # test data set contains a max of 6 passengers. 

## Visualising the given Co-ordinates
#install.packages("sf")
#install.packages("mapview")
library(sf)
library(mapview)
pick_locations=st_as_sf(cab_train,coords = c("pickup_longitude","pickup_latitude"),crs=4326)
mapview(pick_locations)
drop_locations=st_as_sf(cab_train,coords = c("dropoff_longitude","dropoff_latitude"),crs=4326)
mapview(drop_locations)
## conclusions regarding Pickup points
 # Maximum points were located on NEWYORK state of USA
 # The co-ordinates with '0' lat and '0' long were found near "GULF OF GUINEA" in the African continent
 # The co-ordinates of longitude in the range of '40' and lat of '-73' were located in the "ANTARTICA" Region
 # Few of the pick up co-ordinates are located on "WATER"

## Conclusions regarding Drop-off points
 #Maximum drop off points are located on Newyork state of USA
 #Few drop off points were also seen on EUROPE,AFRIACA,ANTARTICA,SOUTH AMERICA
 #Few Drop points were also located on water.

#water pick_point row no's=(11620,10711,8585,8335,8177,14422,13737,4292,3024,4983,10230,11638,13029,615,11926,3990,3677,319)
#water drop_points row no's=(8335,14537,8585,12229,8177,14422,4568,4292,13737,13804,13276,10230,4983,11638,13029,4777,1470,3990)

### Exploratoey Data Analysis ###
## Excluding all the rows that contain water pick_up and drop_off points
cab_train=cab_train[-c(11620,10711,8585,8335,8177,14422,13737,4292,3024,4983,10230,11638,13029,615,11926,3990,3677,319),] #pick_up water points
cab_train=cab_train[-c(8335,14537,8585,12229,8177,14422,4568,4292,13737,13804,13276,10230,4983,11638,13029,4777,1470,3990),]# dropoff water points

## Removing all pick_up and drop_off points which are outside of newyork
# max pickup longitude=-72        min = -74.50
# max pickup latitude=42          min= 40
# max drop_off longitude= -72     min= -74.50
# max drop_off latitude=42        min=40
print(paste('pickup_longitude above -72=',nrow(cab_train[which(cab_train$pickup_longitude >-72 ),]))) #324
print(paste('pickup_longitude below -74.50=',nrow(cab_train[which(cab_train$pickup_longitude < -74.50 ),])))# 0
print(paste('pickup_latitude above 42=',nrow(cab_train[which(cab_train$pickup_latitude > 42 ),]))) #1
print(paste('pickup_latitude below 40=',nrow(cab_train[which(cab_train$pickup_latitude < 40 ),]))) #324
print(paste('dropoff_longitude above -72=',nrow(cab_train[which(cab_train$dropoff_longitude > -72 ),]))) #324
print(paste('dropoff_longitude below -74.50=',nrow(cab_train[which(cab_train$dropoff_longitude < -74.50 ),]))) #0
print(paste('dropoff_latitude above 42=',nrow(cab_train[which(cab_train$dropoff_latitude>42 ),]))) # 0
print(paste('dropoff_latitude below 40=',nrow(cab_train[which(cab_train$dropoff_latitude <40 ),]))) # 324

## Excluding all of the above co-ordinates from the train data set
cab_train=cab_train[-which(cab_train$pickup_longitude >-72),]
#cab_train=cab_train[-which(cab_train$pickup_longitude < -74.50),]
cab_train=cab_train[-which(cab_train$pickup_latitude >42),]
#cab_train=cab_train[-which(cab_train$pickup_latitude<40),]
cab_train=cab_train[-which(cab_train$dropoff_longitude>-72),]
#cab_train=cab_train[-which(cab_train$dropoff_longitude<-74.50),]
#cab_train=cab_train[-which(cab_train$dropoff_latitude>42),]
cab_train=cab_train[-which(cab_train$dropoff_latitude<40),]

## Replotting on map
pick_locations=st_as_sf(cab_train,coords = c("pickup_longitude","pickup_latitude"),crs=4326)
mapview(pick_locations)
drop_locations=st_as_sf(cab_train,coords = c("dropoff_longitude","dropoff_latitude"),crs=4326)
mapview(drop_locations)
cab_train=cab_train[-c(1470,4777),] # two were seen on water

### Considering no.of passengersfor analysis
# since test data set contains max of 6 passengers, will keep the same limit in train dataset

cab_train$passenger_count=round(cab_train$passenger_count)
print(paste("passengers count greater than 6 is",nrow(cab_train[which(cab_train$passenger_count>6),])))
print(paste("passenger count equal to 0 is",nrow(cab_train[which(cab_train$passenger_count==0),])))
print(paste("passenger count less than 0 is",nrow(cab_train[which(cab_train$passenger_count<0),])))
# removing them
cab_train=cab_train[-which(cab_train$passenger_count>6),]
cab_train=cab_train[-which(cab_train$passenger_count==0),]

### Considering Fare Amount for Anallysis
cab_train$fare_amount = as.numeric(as.character(cab_train$fare_amount))
print(paste("Fare amount less than 1 is",nrow(cab_train[which(cab_train$fare_amount<1),])))
cab_train=cab_train[-which(cab_train$fare_amount<1),]

## Re-checking the summary of train data
summary(cab_train)
## there are missing values and outliers

#######    MISSING VALUE ANALYSIS  #######
missing_val=data.frame(apply(cab_train,2,function(x){sum(is.na(x))}))
names(missing_val)="missing_count"
missing_val$variables=row.names(missing_val)
row.names(missing_val)=NULL
missing_val=missing_val[,c(2,1)]
## Only Fare amount and passenger count contains missing values

ggplot(data = missing_val, aes(x=variables,y = missing_count))+
   geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
   ggtitle("Missing data count") + theme_bw()
for (i in (1:6)){
   print(paste("passenger count equal to",i,nrow(cab_train[which(cab_train$passenger_count==i),])))
}
## Most of the time single passenger has taken the ride.
## Since large number of the times single passenger has taken ride, 
 #imputig the na's would be biased towards single passenger and so fare amount may also be biased
 # since missing number is less it is better to drop those rows 

## Dropping the rows of missing value
cab_train=na.omit(cab_train)
missing_num=apply(cab_train,2,function(x){sum(is.na(x))})
 # So all the missing numbers are removed.
 #write.csv(cab_train,"train_missing_rem.csv",row.names = FALSE)

## Misssing values of test data set
test_missing=apply(cab_test,2,function(x){sum(is.na(x))})
# test data set contains no missing value.

#cab_train=read.csv("train_missing_rem.csv",header = T)

#######        OUTLIER ANALYSIS         #######

##  Most of the outliers are already removed based on basic understanding of the data and map visualisations
summary(cab_train)
str(cab_train)
cab_train$passenger_count=as.numeric(cab_train$passenger_count)
numeric_train=cab_train[,sapply(cab_train,is.numeric)]
cnames=colnames(numeric_train)
#Plotting Box plot
outliers=boxplot(cab_train$fare_amount,plot = FALSE)$out
#print(outliers)
outlier_plot = ggplot(cab_train,aes(x =factor(passenger_count),y = fare_amount))
outlier_plot + geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,outlier.size=1, notch=FALSE)+ylim(0,500)

## Let's Remove Rows containing fare amount greater than 80
cab_train=cab_train[-which(cab_train$fare_amount>80),]
# as per boxplot upper limit for fair would be 25.25, but setting upper limit to 80, which may indicate peak hrs and airport rides.


##########        FEATURE ENGINEERING       ########
#  we are given time stamp of pickup_datetime
# Extracting new features from the same like Year,MOnth,day of week, hour of pickup, Distance Travelled

## CALCULATING THE DISTANCE  ##
#install.packages("geosphere")
library(dplyr)
library(geosphere)

cab_train = cab_train %>% 
   mutate(  
      dist_covered=distHaversine(cbind(pickup_longitude, pickup_latitude), cbind(dropoff_longitude,dropoff_latitude),r=6371))

summary(cab_train$dist_covered)
# removing distance covered is less than 500 mtrs.
cab_train=cab_train[-which(cab_train$dist_covered<0.5),] # keeping minium limit to 500mtrs

## plotting graph for distance and fare amount
plot(cab_train$fare_amount,cab_train$dist_covered,col='green',lwd=5,xlab = 'fare_amount',ylab = 'distance travelled',main='fare/dist plot')
gplot = ggplot(data=cab_train, aes(x=cab_train$dist_covered, y=cab_train$fare_amount)) + geom_point()+ geom_line()+ 
   ggtitle("Distance and Fare Plot") +
   xlab("Distance in KM ") + 
   ylab("Fare")
gplot

## Distance is directly impacting the fare amount

### Generating the distance in test data set
cab_test = cab_test %>% 
   mutate(  
      dist_covered=distHaversine(cbind(pickup_longitude, pickup_latitude), cbind(dropoff_longitude,dropoff_latitude),r=6371))

## extracting year,month time,day of week
cab_train$pickup_date = as.Date(as.character(cab_train$pickup_datetime))
cab_train$pickup_weekday = as.factor(format(cab_train$pickup_date,"%u")) #1=monday,2=tuesday so on
cab_train$pickup_month = as.numeric(format(cab_train$pickup_date,"%m"))
cab_train$pickup_yr = as.numeric(format(cab_train$pickup_date,"%Y"))
pickup_time = strptime(cab_train$pickup_datetime,"%Y-%m-%d %H:%M:%S")
cab_train$pickup_hour = as.numeric(format(pickup_time,"%H"))
cab_train$time_of_day=as.factor(ifelse(cab_train$pickup_hour >= 3 & cab_train$pickup_hour < 9,
                                       "Morning", ifelse(cab_train$pickup_hour >= 9 & cab_train$pickup_hour < 14, "Mid-Day",
                                                         ifelse(cab_train$pickup_hour >= 14 & cab_train$pickup_hour < 18, "Evening", "Night"))))

## THERE ARE 3 AIRPORTS IN NEWYORK AND CLUSTERS OF CO-ORDINATES ARE SEEN ON THESE PLACES, so deriving features for airport ride
# jfk cordinates from google map=(long=-73.7781,lat=40.6413)
# lagaurdia airpport=(long=-73.8740,lat=40.7769)
# newark liberty international=(long=40.6895,lat=74.1745)
jfk_long=-73.7781
jfk_lat=40.6413
jfk=c(jfk_long,jfk_lat)
lag=c(-73.8740,40.7769)
newark=c(-74.1745,40.6895)

cab_train = cab_train %>% 
   mutate(  
      dist_to_jfk=distHaversine(cbind(pickup_longitude, pickup_latitude),jfk,r=6371)+distHaversine(cbind(dropoff_longitude,dropoff_latitude),jfk,r=6371),
      dist_to_lag=distHaversine(cbind(pickup_longitude,pickup_latitude),lag,r=6371)+distHaversine(cbind(dropoff_longitude,dropoff_latitude),lag,r=6371),
      dist_to_newark=distHaversine(cbind(pickup_longitude,pickup_latitude),newark,r=6371)+distHaversine(cbind(dropoff_longitude,dropoff_latitude),newark,r=6371))

## NO points were seen travelling to newark airport when visualised on map

cab_train$pickup_jfk=as.factor(ifelse(cab_train$pickup_longitude >= -73.79 & cab_train$pickup_longitude < -73.76 & cab_train$pickup_latitude>40.63
                                      & cab_train$pickup_latitude<40.65,'yes','no'))
cab_train$dropoff_jfk=as.factor(ifelse(cab_train$dropoff_longitude >= -73.79 & cab_train$dropoff_longitude < -73.76 & cab_train$dropoff_latitude>40.63
                                      & cab_train$dropoff_latitude<40.65,'yes','no'))                                     

cab_train$pickup_lag=as.factor(ifelse(cab_train$pickup_longitude >= -73.88 & cab_train$pickup_longitude < -73.86 & cab_train$pickup_latitude>40.76
                                      & cab_train$pickup_latitude<40.78,'yes','no'))

cab_train$dropoff_lag=as.factor(ifelse(cab_train$dropoff_longitude >= -73.88 & cab_train$dropoff_longitude < -73.86 & cab_train$dropoff_latitude>40.76
                                       & cab_train$dropoff_latitude<40.78,'yes','no'))

str(cab_train)
unique(cab_train$pickup_weekday)
sum(is.na(cab_train)) # 6 na's were created
cab_train=na.omit(cab_train)

## plotting week day v/s Fare amount
 ggplot(data = cab_train, mapping = aes(x = as.factor(cab_train$pickup_weekday), y = cab_train$fare_amount)) +
    geom_bar(stat = "identity") +
    labs(x = 'week day')

 ## plotting hour v/s fare amount
 ggplot(data = cab_train, mapping = aes(x = as.factor(cab_train$pickup_hour), y = cab_train$fare_amount)) +
    geom_bar(stat = "identity") +
    labs(x = 'HOUR')
# as seen on the graph as time of day varies farea mount also varies.

## plotting year v/s fare amount
 ggplot(data = cab_train, mapping = aes(x = as.factor(cab_train$pickup_yr), y = cab_train$fare_amount)) +
    geom_bar(stat = "identity") +
    labs(x = 'YEAR')
# as seen from the graph year is not explaining much about the fare amount variation
 
 ## plotting time of day and fare amount
 ggplot(data = cab_train, mapping = aes(x = as.factor(cab_train$time_of_day), y = cab_train$fare_amount)) +
    geom_bar(stat = "identity") +
    labs(x = 'TIME OF THE DAY')
 ## plotting airport dist and fare amount
 ggplot(data = cab_train,aes(x=fare_amount,y=dist_to_jfk,group=1))+geom_line(linetype="dashed",color='red')+geom_point()
 ggplot(data=cab_train,aes(x=fare_amount,y=dist_to_lag,group=1))+geom_line(linetype='dashed',color='blue')+geom_point()
 ggplot(data=cab_train,aes(x=fare_amount,y=dist_to_newark,group=1))+geom_line(linetype='dashed',color='green')+geom_point()
 
## FEATURE ENGINEERING IN TEST DATA ##
 cab_test$pickup_date = as.Date(as.character(cab_test$pickup_datetime))
 cab_test$pickup_weekday = as.factor(format(cab_test$pickup_date,"%u")) #1=monday,2=tuesday so on
 cab_test$pickup_month = as.factor(format(cab_test$pickup_date,"%m"))
 cab_test$pickup_yr = as.factor(format(cab_test$pickup_date,"%Y"))
 pickup_time = strptime(cab_test$pickup_datetime,"%Y-%m-%d %H:%M:%S")
 cab_test$pickup_hour = as.numeric(format(pickup_time,"%H"))
 cab_test$time_of_day=as.factor(ifelse(cab_test$pickup_hour >= 3 & cab_test$pickup_hour < 9,
                                        "Morning", ifelse(cab_test$pickup_hour >= 9 & cab_test$pickup_hour < 14, "Mid-Day",
                                                          ifelse(cab_test$pickup_hour >= 14 & cab_test$pickup_hour < 18, "Evening", "Night"))))
 
 
 cab_test = cab_test %>% 
    mutate(  
       dist_to_jfk=distHaversine(cbind(pickup_longitude, pickup_latitude),jfk,r=6371)+distHaversine(cbind(dropoff_longitude,dropoff_latitude),jfk,r=6371),
       dist_to_lag=distHaversine(cbind(pickup_longitude,pickup_latitude),lag,r=6371)+distHaversine(cbind(dropoff_longitude,dropoff_latitude),lag,r=6371),
       dist_to_newark=distHaversine(cbind(pickup_longitude,pickup_latitude),newark,r=6371)+distHaversine(cbind(dropoff_longitude,dropoff_latitude),newark,r=6371))

 cab_test$pickup_jfk=as.factor(ifelse(cab_test$pickup_longitude >= -73.79 & cab_test$pickup_longitude < -73.76 & cab_test$pickup_latitude>40.63
                                       & cab_test$pickup_latitude<40.65,'yes','no'))
 cab_test$dropoff_jfk=as.factor(ifelse(cab_test$dropoff_longitude >= -73.79 & cab_test$dropoff_longitude < -73.76 & cab_test$dropoff_latitude>40.63
                                        & cab_test$dropoff_latitude<40.65,'yes','no'))                                     
 
 cab_test$pickup_lag=as.factor(ifelse(cab_test$pickup_longitude >= -73.88 & cab_test$pickup_longitude < -73.86 & cab_test$pickup_latitude>40.76
                                       & cab_test$pickup_latitude<40.78,'yes','no'))
 
 cab_test$dropoff_lag=as.factor(ifelse(cab_test$dropoff_longitude >= -73.88 & cab_test$dropoff_longitude < -73.86 & cab_test$dropoff_latitude>40.76
                                        & cab_test$dropoff_latitude<40.78,'yes','no')) 
 
 
 
#write.csv(cab_train,"train_renewed_21.csv",row.names = F) 
#write.csv(cab_test,"test_renewed_20.csv",row.names = F) 

#cab_train1=read.csv("train_renewed_21.csv",header = T)
#cab_test=read.csv("test_renewed_20.csv",header = T)


### correlation analysis ####
numeric_index=sapply(cab_train,is.numeric)
numeric_data=cab_train[,numeric_index]


corrgram(numeric_data,upper.panel=panel.pie,text.panel = panel.txt, main = "Correlation Plot")
# Distance is highly correlated with fare amount
#dist to airports are not explaining much of variance in fare amount
#dist_to newark is highly correlated with ditance covered,dropoff & pickup longitude
#dist_to_jfk is not correlated with fare amount

#### ANOVA for categorical features
anv_corr = aov(fare_amount ~ pickup_weekday + pickup_month + pickup_yr+time_of_day+pickup_jfk+dropoff_jfk+pickup_lag+dropoff_lag,data =cab_train)
summary(anv_corr)
# pickup_week day has p value greater rhan 0.05


## The finalised set of features
cab_train=subset(cab_train,select=c(fare_amount,pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude,pickup_yr,passenger_count,
                                    pickup_month,pickup_hour,dist_covered,dist_to_jfk,dist_to_lag,dist_to_newark,time_of_day))


cab_test=subset(cab_test,select=c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude,pickup_yr,passenger_count,
                                    pickup_month,pickup_hour,dist_covered,dist_to_jfk,dist_to_lag,dist_to_newark,time_of_day))

## Considering airports 2 sets of featuews were derived,the model with categorical features gave greater errors compared to features with airport distances
# So model with airport distances as features are considered as they are explaining much of variance of fare amount.

#train_sorted=read.csv("cab_train_sorted.csv",header = T)
#test_sorted=read.csv("cab_test_sorted.csv",header = T)

## Dropping pickup week day from train and test as p value is greater tha 0.05
#train_sorted=subset(train_sorted,select=-pickup_weekday)
#test_sorted=subset(test_sorted,select=-pickup_weekday)

#### splitting the data as train and test data  #####
set.seed(1234)
sel_index=createDataPartition(cab_train$fare_amount,p=.8,list = F)
train_data=cab_train[sel_index,]
test_data=cab_train[-sel_index,]

########## LINEAR REGRESSION MODEL  ######
lg_model=lm(fare_amount~.,data=train_data)
summary(lg_model)

lg_predictions = predict(lg_model,test_data[,2:14])
qplot(x=test_data[,1],y=lg_predictions,data = test_data,color='blue',geom = "point")

regr.eval(test_data[,1],lg_predictions)                     
#   mae        mse       rmse       mape 
#2.5297999 22.6721605  4.7615292  0.2790761 

#   mae        mse       rmse       mape     
#2.1010610 13.8241020  3.7180777  0.2203553
#   mae        mse       rmse       mape    without pickup weekday
#2.1012294 13.8202686  3.7175622  0.2202565 

#### DECISION TREE #####
dt_model=rpart(fare_amount~.,data=train_data,method = "anova")
summary(dt_model)

dt_predict=predict(dt_model,test_data[,2:14])
qplot(x=test_data[,1],y=dt_predict,data = test_data,color='blue',geom = "point")

regr.eval(test_data[,1],dt_predict)
#   mae        mse       rmse       mape              
#2.3397500 16.3244549  4.0403533  0.2306706

#   mae        mse       rmse       mape 
#2.2686271 15.4363258  3.9289090  0.2276376 

#####   RANDOM FOREST  #######
rf_model=randomForest(fare_amount~.,data = train_data)
summary(rf_model)

rf_predictions=predict(rf_model,test_data[,2:14])
qplot(x = test_data[,1], y = rf_predictions, data = test_data, color = I("blue"), geom = "point")
regr.eval(test_data[,1],rf_predictions)
#   mae        mse       rmse       mape 
#1.9293772 11.4287812  3.3806481  0.2026238 

#    mae       mse      rmse      mape 
#1.6990666 9.8017742 3.1307785 0.1736559 

#####   XGBOOSTING METHOD    #######
library(xgboost)
train_matrix=as.matrix(sapply(train_data[-1],as.numeric))
test_matrix=as.matrix(sapply(test_data[-1],as.numeric))

xgboost_model=xgboost(data=train_matrix,label = train_data$fare_amount,nrounds=100,max_depth=4,nfold=5,eta=.3,objective="reg:linear",verbose = 1)
summary(xgboost_model)
xg_predictions=predict(xgboost_model,test_matrix)
regr.eval(test_data[,1],xg_predictions)
#   mae        mse       rmse       mape 
#1.8680466 11.6708489  3.4162624  0.1847047 

#   mae       mse      rmse      mape 
#1.6650152 9.7753520 3.1265559 0.1670399


## CROSS VALIDATION to find the Best Iteration and Tune the parameters used

cv=xgb.cv(data=train_matrix,label = train_data$fare_amount,nrounds = 1000,nfold=5,objetive="reg:linear",
          eta=0.3,max_depth=5,eval_metric="rmse",early_stopping_rounds = 100,verbose = 1)

cv$best_iteration 
cv$evaluation_log
xgb = xgboost(data = train_matrix,label = train_data$fare_amount, nrounds = cv$best_iteration,eta=0.3,max_depth=5
                   ,early_stopping_rounds = 100, eval_metric = "rmse",verbose = 1)

xg_predictions=predict(xgb,test_matrix)
regr.eval(test_data[,1],xg_predictions)
#    mae        mse       rmse       mape 
#1.7998498 10.5317678  3.2452685  0.1803485

#   mae       mse      rmse      mape 
#1.6709979 9.5901453 3.0967960 0.1685755 

qplot(x = test_data[,1], y = xg_predictions, data = test_data, color = I("blue"), geom = "point")


###############  SELECTING THE MODEL     ###############
 # XGBOOST MODEL PROOVED TO BE BETTER WITH LEAST OF ERRORS AMONG ALL OTHER MODELS
saveRDS(xgb,"xgboost_final.rds")

## TRAINING THE XGBOOST MODEL ON COMPLETE TRAINING DATA SET AND USING IT ON TEST DATA SET 
cab_train_comp=as.matrix(sapply(cab_train[-1],as.numeric))
cab_test_com=as.matrix(sapply(cab_test,as.numeric))

cv=xgb.cv(data=cab_train_comp,label = cab_train$fare_amount,nrounds = 1000,nfold=5,objetive="reg:linear",
          eta=0.3,max_depth=5,eval_metric="rmse",early_stopping_rounds = 100,verbose = 1)
cv$best_iteration
xgboost_model=xgboost(data=cab_train_comp,label = cab_train$fare_amount,nrounds=cv$best_iteration,max_depth=5,nfold=5,eta=0.3,objective="reg:linear")
summary(xgboost_model)
final_predictions=predict(xgboost_model,cab_test_com)

# Reloading the test file
cab_test=read.csv("test.csv",header = T)

cab_test$predicted_fare_amount=final_predictions
write.csv(cab_test,"final_submission.csv",row.names = F)

################################################################################################################################################
