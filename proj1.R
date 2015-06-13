# Project 1 Rcode
activity = read.csv("F:\\Coursera\\DataScience_JohnsHopkins\\5_ReproducibleResearch\\proj1\\repdata-data-activity\\activity.csv")
summary(activity)
tail(activity)
head(mydata)
tail(mydata)
mydata = as.data.frame(activity[complete.cases(activity), ])
summary(mydata)
str(mydata)
hist(totalSteps,
     main = "Histogram of the total number of steps taken each day")
head(mydata)
summary(mydata$date)

totalSteps = tapply(mydata$steps, mydata$date, sum)
hist(totalSteps)
mean(totalSteps, na.rm = TRUE)
median(totalSteps, na.rm = TRUE)

avgDailyPattern = aggregate(mydata$steps, by = list(mydata$interval), FUN = mean)
summary(avgDailyPattern)
head(avgDailyPattern)
plot(avgDailyPattern[, 1], avgDailyPattern[, 2], type = "l", 
     xlab = "5 min Intervals in a day", 
     ylab = "Average Number of Steps", 
     main = "The Average Daily Activity Pattern")
maxStepsInterval = avgDailyPattern[which.max(avgDailyPattern[, 2]), 1]
maxStepsInterval

sum(!complete.cases(activity))
fillData = activity

length(activity$steps)
nrow(activity)

newdata = activity
len1 = nrow(newdata)
len2 = nrow(avgDailyPattern)
for (i in 1:len1) {
  if (is.na(newdata$steps[i])) {
    for (j in 1:len2) {
      if (newdata$interval[i] == avgDailyPattern[j, 1]) {
        newdata$steps[i] = avgDailyPattern[j, 2]
      }
    } 
  }    
}
summary(newdata)

fill = function(){
  newdata = activity
  len1 = nrow(newdata)
  len2 = nrow(avgDailyPattern)
  for (i in 1:len1) {
    if (is.na(newdata$steps[i])) {
      for (j in 1:len2) {
        if (newdata$interval[i] == avgDailyPattern[j, 1]) {
          newdata$steps[i] = avgDailyPattern[j, 2]
        }
      }
    }
  }
}

totalStepsNew = tapply(newdata$steps, newdata$date, sum)
hist(totalStepsNew)
meanTotStepsNew = mean(totalStepsNew, na.rm = TRUE)
meanTotStepsNew
medianTotStepsNew = median(totalStepsNew, na.rm = TRUE)
medianTotStepsNew


mydata$weekday = TRUE
weekday = weekdays(as.POSIXct(mydata$date, format = "%Y-%m-%d" ))
for (i in 1:length(weekday)) {
  if (weekday[i] == "Saturday" | weekday[i] == "Sunday") {
    mydata$weekday[i] = FALSE
  }
}
dataWeekdays = mydata[which(mydata$weekday == TRUE), ]
dataWeekend = mydata[which(mydata$weekday == FALSE), ]

avgWeekdaysPattern = aggregate(dataWeekdays$steps, 
                               by = list(dataWeekdays$interval), 
                               FUN = mean)
names(avgWeekdaysPattern) = c("interval", "steps")
avgWeekdaysPattern$dayTag = "weekdays"
avgWeekendPattern = aggregate(dataWeekend$steps, 
                              by = list(dataWeekend$interval), 
                              FUN = mean)
names(avgWeekendPattern)= c("interval", "steps")
avgWeekendPattern$dayTag = "weekend"

avgPattern = rbind(avgWeekdaysPattern, avgWeekendPattern)
#avgPattern$dayTag = factor(avgPattern$dayTag)
str(avgPattern)
head(avgPattern)
library(lattice)
xyplot(steps ~ interval | dayTag, data = avgPattern, 
       type = "l", layout = c(1, 2))