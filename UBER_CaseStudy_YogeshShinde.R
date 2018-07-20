#Calling the packages required for this assignment
library(lubridate)
require(dplyr)
require(ggplot2)

#Import the CSV
UberDS <- read.csv("Uber Request Data.csv",header = TRUE, stringsAsFactors = FALSE)

#Correct the Column Names
colnames(UberDS)[1] <- "RequestID"
colnames(UberDS)[2] <- "PickupPoint"
colnames(UberDS)[3] <- "DriverID"
colnames(UberDS)[5] <- "RequestTime"
colnames(UberDS)[6] <- "DropTime"

#There are two types of Date Format in column RequestTime; convert it into single format
UberDS$RequestTimeDate <- dmy_hms(UberDS$RequestTime, truncated = 1)
UberDS$DropTimeDate <- dmy_hms(UberDS$DropTime, truncated = 1)

#Create New table for Status and count
UberDS_Status <- table(UberDS$Status)

#Get the labels of table 
lbls <- paste(names(UberDS_Status), "\n", UberDS_Status, sep="")

#Draw a Pie chart showing contribution of each type of Status
pie(UberDS_Status, labels = lbls, main="Pie Chart of Status\n (with sample sizes)") 

#Get individual count
Count_TripCompleted <- length(which(UberDS$Status == "Trip Completed"))
Count_NoCarsAvailable <- length(which(UberDS$Status == "No Cars Available"))
Count_Cancelled <- length(which(UberDS$Status == "Cancelled"))

#Form a vector of counts
slices <- c(Count_TripCompleted, Count_NoCarsAvailable, Count_Cancelled)

#Form a vector names
lbls_1 <- c("TripCompleted", "NoCarsAvailable", "Cancelled")

#Take individual percentage
pct <- round(slices/sum(slices)*100)

#append percentage to names
lbls_1 <- paste(lbls_1, pct)

#append percentage sign to names
lbls_1 <- paste(lbls_1,"%",sep="") 

#Draw a Pie chart showing contribution of each type of Status in Percentage
pie(slices,labels = lbls_1, col=rainbow(length(lbls_1)), main="Pie Chart of Trip Status")

############### Further drill-down No Cars Availble ############### 

#Count for NoCarsAvailable from City
Count_NoCarsAvailable_City <- length(which(UberDS$Status == "No Cars Available" & UberDS$PickupPoint == "City"))

#Count for NoCarsAvailable from City
Count_NoCarsAvailable_Airport <- length(which(UberDS$Status == "No Cars Available" & UberDS$PickupPoint == "Airport"))

#Form a vector of counts
slices_NoCarsAvailable <- c(Count_NoCarsAvailable_City, Count_NoCarsAvailable_Airport)

#Form a vector names
lbls_NoCarsAvailable <- c("City", "Airport")

#Take individual percentage
pct_NoCarsAvailable <- round(slices_NoCarsAvailable/sum(slices_NoCarsAvailable)*100)

#append percentage to names
lbls_NoCarsAvailable <- paste(lbls_NoCarsAvailable, pct_NoCarsAvailable)

#append percentage sign to names
lbls_NoCarsAvailable <- paste(lbls_NoCarsAvailable,"%",sep="") 

#Draw a Pie chart showing contribution of City to Airport & Airport to City for No Cars Available
pie(slices_NoCarsAvailable,labels = lbls_NoCarsAvailable, col=rainbow(length(lbls_NoCarsAvailable)), main="Pie Chart of Trip Status - NoCarsAvailable")




############### Further drill-down Cancelled ############### 

#Count for Cancelled from City
Count_Cancelled_City <- length(which(UberDS$Status == "Cancelled" & UberDS$PickupPoint == "City"))

#Count for Cancelled from City
Count_Cancelled_Airport <- length(which(UberDS$Status == "Cancelled" & UberDS$PickupPoint == "Airport"))

#Form a vector of counts
slices_Cancelled <- c(Count_Cancelled_City, Count_Cancelled_Airport)

#Form a vector names
lbls_Cancelled <- c("City", "Airport")

#Take individual percentage
pct_Cancelled <- round(slices_Cancelled/sum(slices_Cancelled)*100)

#append percentage to names
lbls_Cancelled <- paste(lbls_Cancelled, pct_Cancelled)

#append percentage sign to names
lbls_Cancelled <- paste(lbls_Cancelled,"%",sep="") 

#Draw a Pie chart showing contribution of City to Airport & Airport to City for No Cars Available
pie(slices_Cancelled,labels = lbls_Cancelled, col=rainbow(length(lbls_Cancelled)), main="Pie Chart of Trip Status - Cancelled")

#Derive Metrics for hour
UberDS$request_hour = format(UberDS$RequestTimeDate, "%H")
UberDS$request_hour <- as.numeric(UberDS$request_hour)



#Make seperate Data-set for City
UberDS_City <- filter(UberDS, PickupPoint == "City")

#Derive Metrics for Gap
UberDS_City$Gap <- ifelse(UberDS_City$Status == "Trip Completed", "NoGap", "Gap" )

#Derive Metrics for TimeSlot
UberDS_City$TimeSlot <- ifelse((UberDS_City$request_hour >= 6) & (UberDS_City$request_hour < 12), "Morning",  
                               ifelse((UberDS_City$request_hour >= 12) & (UberDS_City$request_hour < 17), "Afternoon",  ifelse((UberDS_City$request_hour >= 17) & (UberDS_City$request_hour < 20), "Evening", "Night" ))
)

#Plot a chart
hourwise_request_count_city <- ggplot(UberDS_City,aes(x=factor(TimeSlot),fill=factor(Gap)))


plot_City <- hourwise_request_count_city+geom_bar(stat='count')+ ggtitle("Time Slot wise Demand & Gap for Cabs")+ labs(x="Time in Hours", y="Number of Cabs Requested")+ labs(fill="Pickup Point") +labs(x="",y="Number of Requests") 


#View Plot
plot_City



#Make seperate Data-set for Airport
UberDS_Airport <- filter(UberDS, PickupPoint == "Airport")

#Derive Metrics for Gap
UberDS_Airport$Gap <- ifelse(UberDS_Airport$Status == "Trip Completed", "NoGap", "Gap" )

#Derive Metrics for TimeSlot
UberDS_Airport$TimeSlot <- ifelse((UberDS_Airport$request_hour >= 6) & (UberDS_Airport$request_hour < 12), "Morning",  
                                  ifelse((UberDS_Airport$request_hour >= 12) & (UberDS_Airport$request_hour < 17), "Afternoon",  ifelse((UberDS_Airport$request_hour >= 17) & (UberDS_Airport$request_hour < 20), "Evening", "Night" ))
)

#Plot a chart
hourwise_request_count_Airport <- ggplot(UberDS_Airport,aes(x=factor(TimeSlot),fill=factor(Gap)))


plot_Airport <- hourwise_request_count_Airport+geom_bar(stat='count')+ ggtitle("Time Slot wise Demand & Gap for Cabs")+ labs(x="Time in Hours", y="Number of Cabs Requested")+ labs(fill="Pickup Point") +labs(x="",y="Number of Requests") 

#View Plot
plot_Airport