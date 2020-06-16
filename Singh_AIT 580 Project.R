############Loading Libraries and Data##############

library("tidyverse")
library("data.table")
library("lubridate")
library("gridExtra")
library("caret")
library("gbm")
library("ggplot2")

taxitrain <- read.csv("C:/Users/shrey/Desktop/AIT 580/train.csv")
taxitest <- read.csv("C:/Users/shrey/Desktop/AIT 580/test.csv")

##########Structure and Summary of Data########################

str(taxitrain)
summary(taxitrain)

#################Visualizing Dependent Variable####################

options(scipen = 99) #To convert exponential to number                    
ggplot(taxitrain,aes(x = trip_duration)) +
  geom_histogram(fill = "red", bins = 150) +
  scale_x_log10() +
  ggtitle("Histogram representing the Trip Duration")#Visualizing the log of trip duration

###################Transforming Dependent Variable - result - rmsle#####################

taxitrain$trip_duration = log(taxitrain$trip_duration + 1) 

###########Combining train and test##################

taxitest$trip_duration = 0
taxitest$dropoff_datetime = NA
taxicom =  bind_rows(taxitrain,taxitest)

#############Structure & Summary of Combined Dataset#####################

str(taxicom)
summary(taxicom)

##########Converting data acc to specific data types#########

taxicom = taxicom %>%
  mutate(pickup_datetime  = ymd_hms(pickup_datetime),
         dropoff_datetime = ymd_hms(dropoff_datetime),
         vendor_id        = factor(vendor_id),
         passenger_count  = factor(passenger_count)) 

##########################Univariate Analysis###########


options(scipen =  99)  

table(taxitrain$vendor_id) #Count of the data based on Vendor ID

p1 = ggplot(taxitrain,aes(x = vendor_id)) + 
  geom_bar(fill = "red") + 
  ggtitle("Bar Chart representing the Vendor ID")

taxitrain %>%
  select(passenger_count) %>%
  group_by(passenger_count) %>%
  count() #Count of the data based on Passenger Count

p2 = ggplot(taxitrain,aes(x = passenger_count)) + 
  geom_bar(fill = "blue") + 
  ggtitle("Bar Chart representing the Passenger Count")


p3 = ggplot(taxitrain,aes(x = store_and_fwd_flag)) + 
  geom_bar(fill = "green") + 
  ggtitle("Bar Chart representing the Store And Forward Flag")


grid.arrange(p1,p2,p3,ncol=3)

p4 =  ggplot(taxitrain,aes(pickup_latitude)) +
  geom_histogram(fill = "blue", bins = 40) +
  scale_x_continuous(limits = c(40.62,40.85)) + 
  ggtitle("Bar Chart representing the Pickup Latitude")

p5 = ggplot(taxitrain,aes(pickup_longitude)) +
  geom_histogram(fill = "red", bins = 40) +
  scale_x_continuous(limits = c(-74.05,-73.75)) + 
  ggtitle("Bar Chart representing the Pickup Longitude")

p6 =  ggplot(taxitrain,aes(dropoff_latitude)) +
  geom_histogram(fill = "blue", bins = 40) +
  scale_x_continuous(limits = c(40.62,40.85)) + 
  ggtitle("Bar Chart representing the Dropoff Latitude")

p7 = ggplot(taxitrain,aes(dropoff_longitude)) +
  geom_histogram(fill = "red", bins = 40) +
  scale_x_continuous(limits = c(-74.05,-73.75)) + 
  ggtitle("Bar Chart representing the Dropoff Longitude")

grid.arrange(p4,p5,p6,p7)

p8 =  ggplot(taxitrain,aes(x = day(pickup_datetime),col = vendor_id))+
  geom_histogram(fill = "red") + scale_x_continuous(breaks=1:31) + 
  ggtitle("Bar Chart representing the Pickup Day")

p9 = ggplot(taxitrain,aes(x = month(pickup_datetime)))+
  geom_histogram(fill = "blue") + 
  ggtitle("Bar Chart representing the Pickup Month")

p10 = ggplot(taxitrain,aes(x = hour(pickup_datetime)))+
  geom_histogram() +
  scale_x_continuous(breaks=1:24) + 
  ggtitle("Bar Chart representing the Pickup Time(Hour)t")

grid.arrange(p8,p9,p10)

##############Combing data & Creating  Features##############

taxicom$hour_pickup = hour(taxicom$pickup_datetime)
taxicom$month_pickup = month(taxicom$pickup_datetime)
taxicom$day_pickup = day(taxicom$pickup_datetime)
taxicom$wday_pickup = wday(taxicom$pickup_datetime)

taxicom$passenger_count  = as.numeric(taxicom$passenger_count)
taxicom$store_and_fwd_flag = ifelse(taxicom$store_and_fwd_flag == "Y",1,0)
taxicom$vendor_id = as.numeric(taxicom$vendor_id)

taxida = taxicom[ ,-c(1,3,4)]

###################Splitting ###################

library(randomForest)
traint = taxida[0:20000, ]
testt  = taxida[1458645:2083778, ]

#################Random Forest - Building Model########################

nycDatarf = train(trip_duration ~ ., data = traint,
                  method = "ranger",trControl = trainControl(method = "cv"),
                  na.action = na.pass,metric="RMSE", importance = "permutation")

nycDatarf
varImp(nycDatarf)#Variable importance

