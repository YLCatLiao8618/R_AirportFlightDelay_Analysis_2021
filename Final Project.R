setwd('C:/Users/Catherine Liao/Desktop')
flightData = read.csv('C:/Users/Catherine Liao/Desktop/Kaggle Datasets/archive (4)/flights.csv')
library(dplyr)
# Read the data from csv file
# Basic Info of Dataset
print (format (object.size (flightData), units="Mb"))   # storage in megabytes
print (c ("Number of columns: ", ncol (flightData)))
print (c ("Number of rows: ", nrow (flightData)))
print (colnames (flightData))

# Remove cancelled and diverted flight
flightData = subset(flightData, CANCELLED == 0 & DIVERTED == 0)

# Remove unuse columns
flightData = select(flightData, -c("YEAR", "FLIGHT_NUMBER", "TAIL_NUMBER", "CANCELLATION_REASON", "DIVERTED", "CANCELLED"))

# Create a column with values night, daytaime, and evening based on the scheduled ARRIVAL time
flightData = mutate(flightData, SCHEDULED_ARRIVAL_TIME_GROUP = ifelse(SCHEDULED_ARRIVAL >= 0 & SCHEDULED_ARRIVAL < 800, "NIGHT", ifelse(SCHEDULED_ARRIVAL >= 800 & SCHEDULED_ARRIVAL <= 1600, "Daytime","Evening")))
flightData = select(flightData, -c("SCHEDULED_ARRIVAL"))
flightData = na.omit(flightData)
flightData$AIRLINE= as.factor(flightData$AIRLINE)
flightData$ORIGIN_AIRPORT = as.factor(flightData$ORIGIN_AIRPORT)
flightData$DESTINATION_AIRPORT= as.factor(flightData$DESTINATION_AIRPORT)
flightData$SCHEDULED_ARRIVAL_TIME_GROUP = as.factor(flightData$SCHEDULED_ARRIVAL_TIME_GROUP)

# Make a smaller dataset
smallset=sort(sample(nrow(flightData), nrow(flightData)*.1))
flightData = flightData[smallset,]
summary(flightData)
str (flightData)

# Install
install.packages("wesanderson")
# Load
library(wesanderson)

par(mfrow=c(4,2))
# Make mean ARRIVAL delay plot for origin airport
airportMeanARRIVALDelay = flightData %>% group_by(ORIGIN_AIRPORT) %>% summarise(MEAN_ARRIVAL_DELAY = mean(ARRIVAL_DELAY, na.rm=T))
plot(airportMeanARRIVALDelay$MEAN_ARRIVAL_DELAY, xlab = "Airports Index", ylab = "Mean ARRIVAL Delay Time(Minutes)")

# Make mean ARRIVAL delay plot for airline
airlineMeanARRIVALDelay = flightData %>% group_by(AIRLINE) %>% summarise(MEAN_ARRIVAL_DELAY = mean(ARRIVAL_DELAY, na.rm=T))
barplot(airlineMeanARRIVALDelay$MEAN_ARRIVAL_DELAY, xlab = "Airlines Index", ylab = "Mean ARRIVAL Delay Time(Minutes)", names = c("AA","AS", "B6","DL","EV","F9","HA","MQ","NK","OO","UA","US","VX","WN"
), ylim=c(0, 60),col ="#69b3a2")

# Make mean ARRIVAL delay plot for month
monthMeanARRIVALDelay = flightData %>% group_by(MONTH) %>% summarise(MEAN_ARRIVAL_DELAY = mean(ARRIVAL_DELAY, na.rm=T))
plot(monthMeanARRIVALDelay$MEAN_ARRIVAL_DELAY, xlab = "Months", ylab = "Mean ARRIVAL Delay Time(Minutes)")

# Make mean ARRIVAL delay plot for day
dayMeanARRIVALDelay = flightData %>% group_by(DAY) %>% summarise(MEAN_ARRIVAL_DELAY = mean(ARRIVAL_DELAY, na.rm=T))
plot(dayMeanARRIVALDelay$MEAN_ARRIVAL_DELAY, xlab = "Days", ylab = "Mean ARRIVAL Delay Time(Minutes)")

# Make mean ARRIVAL delay plot for day of week
dayOfWeekMeanARRIVALDelay = flightData %>% group_by(DAY_OF_WEEK) %>% summarise(MEAN_ARRIVAL_DELAY = mean(ARRIVAL_DELAY, na.rm=T))
plot(dayOfWeekMeanARRIVALDelay$MEAN_ARRIVAL_DELAY, xlab = "Day of week", ylab = "Mean ARRIVAL Delay Time(Minutes)")

# Make mean ARRIVAL delay plot for distance
distanceMeanARRIVALDelay = flightData %>% group_by(DISTANCE) %>% summarise(MEAN_ARRIVAL_DELAY = mean(ARRIVAL_DELAY, na.rm=T))
plot(distanceMeanARRIVALDelay$MEAN_ARRIVAL_DELAY, xlab = "Distance", ylab = "Mean ARRIVAL Delay Time(Minutes)")

# Make mean ARRIVAL delay plot for scheduled ARRIVAL time group
scheduledARRIVALTimeGroupMeanARRIVALDelay = flightData %>% group_by(SCHEDULED_ARRIVAL_TIME_GROUP) %>% summarise(MEAN_ARRIVAL_DELAY = mean(ARRIVAL_DELAY, na.rm=T))
barplot(scheduledARRIVALTimeGroupMeanARRIVALDelay$MEAN_ARRIVAL_DELAY, xlab = "Scheduled ARRIVAL Time Group", ylab = "Mean ARRIVAL Delay Time(Minutes)", names = c("Daytime","Evening","Night"), ylim=c(0, 60),col=wes_palette(n=3, name="GrandBudapest1"))


# Subset data to Training and Testing Dataset
library(caret)
library(dplyr)

set.seed(123)
train_data_size <- 70000
train_index <- sample(seq_len(nrow(flightData)), size = train_data_size)   # 4571206
train_df <- flightData[train_index,]
test_df <- flightData[-train_index,]

memory.size() ### Checking your memory size
memory.limit() ## Checking the set limit
memory.limit(size=56000) 

train.control <- trainControl(method = "cv", number = 3)
model <- train(ARRIVAL_DELAY~., data = train_df, method = "lm",
               trControl = train.control)
predictions = model%>% predict(test_df)
library(Metrics)
rmse(predictions,test_df$ARRIVAL_DELAY) 

summary(model)
coef(model)

set.seed(123)
flight_drop = select(flightData, -c("DAY_OF_WEEK","MONTH","DAY","ORIGIN_AIRPORT","DESTINATION_AIRPORT","SCHEDULED_DEPARTURE","DEPARTURE_TIME","WHEELS_OFF","AIR_TIME","DISTANCE","WHEELS_ON","TAXI_IN","ARRIVAL_TIME","AIR_SYSTEM_DELAY","SECURITY_DELAY","AIRLINE_DELAY","LATE_AIRCRAFT_DELAY","WEATHER_DELAY","SCHEDULED_ARRIVAL_TIME_GROUP"))
train_df <- flight_drop[train_index,]
test_df <- flight_drop[-train_index,]
train.control <- trainControl(method = "cv", number = 3)
model2 <- train(ARRIVAL_DELAY~., data = train_df, method = "lm",
                trControl = train.control)
predictions2 = model2%>% predict(test_df)
rmse(predictions2,test_df$ARRIVAL_DELAY) 
summary(model2) 
print(model)
print(model2)

# Old Method
dt = sort(sample(nrow(flightData), nrow(flightData)*.8))
train<-flightData[dt,]
test<-flightData[-dt,]

install.packages('Metrics')
library(Metrics)
# Linear Regression Model
#train$ORIGIN_AIRPORT = droplevels(train$ORIGIN_AIRPORT)
length(levels(train$ORIGIN_AIRPORT))
lm_model=lm(ARRIVAL_DELAY~., data = train)
summary(lm_model)

train_drop = select(train, -c("DAY_OF_WEEK","ORIGIN_AIRPORT","DESTINATION_AIRPORT","ARRIVAL_TIME","DISTANCE","WHEELS_ON","TAXI_IN","AIR_SYSTEM_DELAY","SECURITY_DELAY","AIRLINE_DELAY","LATE_AIRCRAFT_DELAY","WEATHER_DELAY","SCHEDULED_ARRIVAL","AIR_TIME"))
lm_model_2=lm(ARRIVAL_DELAY~., data = train_drop)
summary(lm_model_2)


#Predicting and Assessing prediction accuracy

test$ORIGIN_AIRPORT = droplevels(test$ORIGIN_AIRPORT)
length(levels(test$ORIGIN_AIRPORT))
predictions2 = lm_model%>% predict(test)
rmse(predictions2,test$ARRIVAL_DELAY)

test_drop = select(test, -c("ARRIVAL_DELAY","DAY_OF_WEEK","ORIGIN_AIRPORT","DESTINATION_AIRPORT","ARRIVAL_TIME","DISTANCE","WHEELS_ON","TAXI_IN","AIR_SYSTEM_DELAY","SECURITY_DELAY","AIRLINE_DELAY","LATE_AIRCRAFT_DELAY","WEATHER_DELAY","SCHEDULED_ARRIVAL","AIR_TIME"))
predictions1 = lm_model_2%>% predict(test_drop)
rmse(predictions1,test$ARRIVAL_DELAY) 
