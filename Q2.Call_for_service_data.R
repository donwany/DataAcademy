
# Data Science Project

#1. data source you are using: pre-formatted, pre-cleaned < API < scraping
#2. problem you are tackling: will anyone care
#3. explain the analysis you are performing
#4. enough exploratory data analysis
#5. data visualization
#6. explain the plots or youtube
#7. git and Heroku for hosting


setwd("~/Desktop/Incubator")
data_2011 = read.csv("Calls_for_Service_2011.csv")
data_2012 = read.csv("Calls_for_Service_2012.csv")
data_2013 = read.csv("Calls_for_Service_2013.csv")
data_2014 = read.csv("Calls_for_Service_2014.csv")
data_2015 = read.csv("Calls_for_Service_2015.csv")

#Description of data says that column names may change from year to year.

colnames(data_2011)
colnames(data_2012)
colnames(data_2013)
colnames(data_2014)
colnames(data_2015)

#Column names are same for these years, join together into one dataframe

library(dplyr)
Calls = bind_rows(data_2011, data_2012, data_2013, data_2014, data_2015)

Calls$TimeDispatch = as.POSIXct(Calls$TimeDispatch, format="%m/%d/%Y  %I:%M:%S %p")
Calls$TimeArrive = as.POSIXct(Calls$TimeArrive, format="%m/%d/%Y  %I:%M:%S %p")

# What fraction of calls are the most common type?
table(Calls$Type_)
max(table(Calls$Type_))/nrow(Calls)
# 0.2453954

# What is the median response time (dispatch to arrival), in seconds, considering only valid (i.e. non-negative) times?
Calls$TimeArrive[13] - Calls$TimeDispatch[13]
Calls$ResponseTime = Calls$TimeArrive - Calls$TimeDispatch
median(Calls$ResponseTime[Calls$ResponseTime > 0], na.rm = TRUE)
# 289 Seconds

Calls$ResponseTime[Calls$ResponseTime < 0] <- NA

# Work out the average (mean) response time in each district. 
districts = sort(unique(Calls$PoliceDistrict))
MeanResponseTimeByDistrict = function(x){
        districtMean = (mean(Calls$ResponseTime[Calls$PoliceDistrict==x], na.rm=TRUE))
        return(districtMean)
}
districttimes = lapply(districts, FUN = MeanResponseTimeByDistrict)


# What is the difference between the average response times of the districts with the longest and shortest times?
max(as.numeric(districttimes))-min(as.numeric(districttimes))
# 190.0464

# What is the largest ratio of the conditional probability of an event type given a district to the unconditional probably of that event type? 

# Need to remove all calls at PoliceDistrict 0
NoAnonCalls = filter(Calls, PoliceDistrict != 0)

type_df = as.data.frame(table(NoAnonCalls$Type_))
# Consider only events types which have more than 100 events.
type_df = filter(type_df, Freq > 100)
type_df
total = sum(type_df$Freq)
type_df$UProb = type_df$Freq / total

