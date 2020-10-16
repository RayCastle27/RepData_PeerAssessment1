library("data.table")
library("ggplot2")

setwd("E:/MEEE/Coursera/Reproducible Research")
read.csv("activity.csv")
activityDT <- data.table::fread(input = "activity.csv")
Total_Steps <- activityDT[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 
head(Total_Steps, 10)
ggplot(Total_Steps, aes(x = steps)) +
  geom_histogram(fill = "green", binwidth = 500) +
  labs(title = "Daily Steps", x = "Steps", y = "Frequency")

Total_Steps[, .(Mean_Steps = mean(steps, na.rm = TRUE), Median_Steps = median(steps, na.rm = TRUE))]

IntervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 
ggplot(IntervalDT, aes(x = interval , y = steps)) + geom_line(color="RED", size=0.5) + labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")

IntervalDT[steps == max(steps), .(max_interval = interval)]
nrow(activityDT[is.na(steps),])
activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
data.table::fwrite(x = activityDT, file = "completeData.csv", quote = FALSE)
Total_Steps <- activityDT[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 
Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]

ggplot(Total_Steps, aes(x = steps)) + geom_histogram(fill = "red", binwidth = 500) + labs(title = "Daily Steps", x = "Steps", y = "Frequency")
activityDT <- data.table::fread(input = "activity.csv")
activityDT[, date := as.POSIXct(date, format = "%Y-%m-%d")]
activityDT[, `Day of Week`:= weekdays(x = date)]
activityDT[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "weekday or weekend"] <- "weekday"
activityDT[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "weekday or weekend"] <- "weekend"
activityDT[, `weekday or weekend` := as.factor(`weekday or weekend`)]
activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
IntervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 

ggplot(IntervalDT , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Avg. Daily Steps", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 3, nrow=3)
head(activityDT, 10)