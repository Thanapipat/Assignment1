library(knitr)
library(ggplot2)
library(dplyr)
library(plyr)
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp, mode="wb")
unzip(temp, "activity.csv")
activity <- read.csv("activity.csv",header=T)
unlink(temp)
totalstepsperday <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
head(totalstepsperday)

activity$date <- as.Date(activity$date, "%Y-%m-%d")
## calculate steps as it relates to date using SUM (per day)
hist(totalstepsperday$steps, 
     main="Total Steps per Day", 
     xlab="Number of Steps per Day", 
     ylab = "Interval",
     col="orange",
     breaks=50)

msteps <- mean(totalstepsperday$steps)
msteps

medsteps <- median(totalstepsperday$steps)
medsteps

summary(totalstepsperday)

fivemin <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)
## line chart
plot(x = fivemin$interval, 
     y = fivemin$steps, 
     type = "l", 
     col = "orange",
     xlab = "5-minute Intervals",
     ylab = "Average Steps Taken ~ Days",
     main = "Average Daily Activity Pattern")

maxsteps <- fivemin$interval[which.max(fivemin$steps)]
maxsteps

activity2 <- activity
nas <- is.na(activity2$steps)
avg_interval <- tapply(activity2$steps, activity2$interval, mean, na.rm=TRUE, simplify = TRUE)
activity2$steps[nas] <- avg_interval[as.character(activity2$interval[nas])]
names(activity2)

sum(is.na(activity2))

par(mfrow=c(1,2))

## Similar analysis without NAs now
totalstepsperday2 <- aggregate(steps ~ date, data = activity2, FUN = sum, na.rm = TRUE)
head(totalstepsperday2)

hist(totalstepsperday2$steps, 
     main = "Total Steps per Day (no-NA)", 
     xlab = "Number of Steps per Day", 
     ylab = "Interval",
     col="green",
     breaks=50)
##Histogram with the orginal dataset
hist(totalstepsperday$steps, 
     main="Total Steps per Day (Original)", 
     xlab="Number of Steps per Day", 
     ylab = "Interval",
     col="orange",
     breaks=50)


#Resetting the panel
par(mfrow=c(1,1))
## What is the impact of imputing data?
summary(totalstepsperday)

summary(totalstepsperday2)

head(activity2)

activity2<- activity2%>%
  mutate(typeofday= ifelse(weekdays(activity2$date)=="Saturday" | weekdays(activity2$date)=="Sunday", "Weekend", "Weekday"))
head(activity2)


fivemin2<- aggregate(steps ~ interval, data = activity2, FUN = mean, na.rm = TRUE)
head(fivemin2)

ggplot(activity2, aes(x =interval , y=steps, color=typeofday)) +
  geom_line() +
  labs(title = "Ave Daily Steps (type of day)", x = "Interval", y = "Total Number of Steps") +
  facet_wrap(~ typeofday, ncol = 1, nrow=2)
