### setting my personal directory

setwd("/media/Respaldo/Coursera/Data Scientist - Especialización/5. Reproducible Research/Project 1")
## load the data ##
mydata <- read.table("activity.csv", sep = ",", header = T)
#libraries
library(plyr)
library(dplyr)

## count all steps by day
steps_by_day <- summarise(group_by(mydata, date), sum(steps))
## get the step mean and median for each day
steps_mean <- summarise(group_by(mydata, date), mean(steps))
steps_median <- summarise(group_by(mydata, date), median(steps))

#Plot steps by day
plot(steps_by_day, type = "h", ylab = "Steps")
dev.copy(png, file = "plot1.png")
dev.off()
#Time series plot of 5 min step and average by interval
#plot(mydata$interval, mydata$steps, type = "h", col = "blue")
#steps_by_interval <- summarise(group_by(mydata, interval), sum(steps))
#steps_by_interval <- sapply(split(mydata$steps, mydata$interval), mean, na.rm = T)
steps_by_interval <- ddply(mydata, c("interval"), summarise, mean(steps, na.rm = T))
plot(steps_by_interval, type = "l", col = "red")
dev.copy(png, file = "plot2.png")
dev.off()
#Time interval with maximum number of average steps
#interval_with_max_steps <- steps_by_interval[which.max(steps_by_interval$..1), 1]
interval_with_max_steps < steps_by_interval[which.max(steps_by_interval$`mean(steps, na.rm = T)`),]

#count na in dataframe
sum(is.na(mydata$steps))
#change na by mean of the day.
x <- mydata
for (i in 1:17568){
    if (is.na(x[i, 1])){
        intervalo <- x[i, 3]
        prom <- steps_by_interval[steps_by_interval$interval == intervalo, 2]
        x[i, 1] = prom
        }
}
# the new dataset whitout na's
x

#mean and media for new dataset
x_mean <- summarise(group_by(x, date), mean(steps))
x_median <- summarise(group_by(x, date), median(steps))
#steps by day, with new dataset
x_by_day <- summarise(group_by(x, date), sum(steps))
plot(x_by_day)
dev.copy(png, file = "plot3.png")
dev.off()
#The difference between two datasets is low

#Create new dataframe with extra column weekday or weekend
what_day <- function(z){
    if ((weekdays(as.Date(z)) == "sábado") || (weekdays(as.Date(z)) == "domingo")){
        return("weekend")
        }
    else{
        return("weekday")
        }
}
x["day"] <- 0
x$day <- lapply(x$date, what_day)

#make a plot by type of day
steps_weekend <- subset(x, day == "weekend")
steps_weekday <- subset(x, day == "weekday")
swd <- ddply(steps_weekday, c("interval"), summarise, mean(steps, na.rm = T))
swe <- ddply(steps_weekend, c("interval"), summarise, mean(steps, na.rm = T))
par(mfrow = c(2, 1))
plot(swd, type = "l", col = "blue", ylab = "number of steps", main = "Weekdays")
plot(swe, type = "l", col = "blue", ylab = "number of steps", main = "Weekends")
dev.copy(png, file = "plot4.png")
dev.off()