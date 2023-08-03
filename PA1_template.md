---
title: "PA1_template"
author: "Rucha"
date: "2023-08-03"
output: html_document
---

## Reproducible Research Week 2 Course Project 1

Loading and pre-processing the data

```{r loaddata}
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
```


## What is mean total number of steps taken per day?
```{r}
library(ggplot2)
library(dplyr)
# Calculate the total number of steps taken per day
stepsPerDay <- activity %>%
        group_by(date) %>%
        summarize(sumsteps = sum(steps, na.rm = TRUE)) 

# Make a histogram of the total number of steps taken each day
hist(stepsPerDay$sumsteps, main = "Histogram of Daily Steps", 
     col="lightblue", xlab="Steps", ylim = c(0,30))

# Calculate and report the mean and median of the total number of steps taken per day
meanPreNA <- round(mean(stepsPerDay$sumsteps))
medianPreNA <- round(median(stepsPerDay$sumsteps))
print(paste("The mean is: ", meanPreNA))
print(paste("The median is: ", medianPreNA))
```

## What is the average daily activity pattern?
```{r}
library(ggplot2)
# Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

stepsPerInterval <- activity %>%
        group_by(interval) %>%
        summarize(meansteps = mean(steps, na.rm = TRUE))

plot(stepsPerInterval$meansteps ~ stepsPerInterval$interval,
     col="red", type="l", xlab = "5 Minute Intervals", ylab = "Average Number of Steps",
     main = "Steps By Time Interval")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

print(paste("5-Minute Interval containing the most steps on average: ",stepsPerInterval$interval[which.max(stepsPerInterval$meansteps)]))
print(paste("Average steps for that interval: ",round(max(stepsPerInterval$meansteps))))
```


## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
print(paste("The total number of rows with NA is: ",sum(is.na(activity$steps))))

# Devise a strategy for filling in all of the missing values in the dataset. (c) Create a new dataset that is equal to the original dataset but with the missing data filled in.
#Strategy to solve for missing NA values: The average for the associated interval will be used. The average was built in an earlier step: First, loop through all records of a copy of the ‘activity’ data. Then, look for records containing NA values. Transform the ‘steps’ value based on matching the interval in the ‘stepsPerInterval’ data frame created in a prior step.

```{r}
activityNoNA <- activity  
for (i in 1:nrow(activity)){
        if(is.na(activity$steps[i])){
                activityNoNA$steps[i]<- stepsPerInterval$meansteps[activityNoNA$interval[i] == stepsPerInterval$interval]
        }
}
# Make a histogram of the total number of steps taken each day.
stepsPerDay <- activityNoNA %>%
        group_by(date) %>%
        summarize(sumsteps = sum(steps, na.rm = TRUE)) 

hist(stepsPerDay$sumsteps, main = "Histogram of Daily Steps", 
     col="lightblue", xlab="Steps")
#Calculate and report the mean and median total number of steps taken per day.
meanPostNA <- round(mean(stepsPerDay$sumsteps), digits = 2)
medianPostNA <- round(median(stepsPerDay$sumsteps), digits = 2)

print(paste("The mean is: ", mean(meanPostNA)))
print(paste("The median is: ", median(medianPostNA)))
NACompare <- data.frame(mean = c(meanPreNA,meanPostNA),median = c(medianPreNA,medianPostNA))
rownames(NACompare) <- c("Pre NA Transformation", "Post NA Transformation")
print(NACompare)

#When you include missing values for all included records you see an increase in both the mean and median. The mean increases from 9354.23 to 10766.19.Note that NA values in the first part of the project were ignored (na.rm = TRUE). Once averages were applied to the missing values the overall mean increased.
```

# Are there differences in activity patterns between weekdays and weekends?

```{r}
#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
activityDoW <- activityNoNA
activityDoW$date <- as.Date(activityDoW$date)
activityDoW$day <- ifelse(weekdays(activityDoW$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activityDoW$day <- as.factor(activityDoW$day)
#Make a panel plot containing a time series plot (i.e.type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
activityWeekday <- filter(activityDoW, activityDoW$day == "weekday")
activityWeekend <- filter(activityDoW, activityDoW$day == "weekend")

activityWeekday <- activityWeekday %>%
        group_by(interval) %>%
        summarize(steps = mean(steps)) 
activityWeekday$day <- "weekday"

activityWeekend <- activityWeekend %>%
        group_by(interval) %>%
        summarize(steps = mean(steps)) 
activityWeekend$day <- "weekend"

wkdayWkend <- rbind(activityWeekday, activityWeekend)
wkdayWkend$day <- as.factor(wkdayWkend$day)


g <- ggplot (wkdayWkend, aes (interval, steps))
g + geom_line() + facet_grid (day~.) + 
        theme(axis.text = element_text(size = 12),axis.title = element_text(size = 14)) + 
        labs(y = "Number of Steps") + labs(x = "Interval") + 
        ggtitle("Average Number of Steps: Weekday vs. Weekend") + 
        theme(plot.title = element_text(hjust = 0.5))
```

#The visualizations shows slight differences in the step patterns throughout the average daily intervals. Weekdays show a large spike in early morning which could coincide with people walking to work/school or transit stations. While step counts on weekends are more consistent throughout the day.


