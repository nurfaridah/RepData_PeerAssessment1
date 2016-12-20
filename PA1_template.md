---
title: "Reproducible Research Course Project 1"
author: "nurfaridah"
date: "December 16, 2016"
output: html_document
---

Project Assignment 1 (Peer Assessment)
======================================

Introduction 

This is the first assessment using r markdown. We will load the required data in r and massage the data to answer the required questions available.

1. Load data

```{r}
setwd("C:/Users/nurfaridah/Documents/DATA_SCIENCE/5.Reproducible_Research/week2")
rawdata <- read.csv("activity.csv", stringsAsFactors=FALSE)
```

2. Get to know the data

```{r}
head(rawdata)
dim(rawdata)
str(rawdata)
```
3.  Questions.

  - What is total number of steps taken per day?
  - Make a histogram of the total number of steps taken each day.
  - Calculate and report the mean and median of the total number of steps taken per day.

```{r}
total_steps_per_day <- tapply(rawdata$steps, rawdata$date, sum, na.rm=TRUE)
total_steps_per_day
hist(total_steps_per_day, col = heat.colors(3))

mean_steps <- mean(rawdata$steps, na.rm=TRUE)
mean_steps
median_steps <- median(rawdata$steps, na.rm=TRUE)
median_steps
summary(rawdata)
```


4.  Questions.

  - What is the average daily activity pattern?
  - Which, on average across all the days in the dataset, contains the maximum number of      steps?
  
```{r}
stepsdata <- aggregate(steps ~ interval, data=rawdata, mean, na.rm=TRUE)
plot(stepsdata$interval, stepsdata$steps, type="l", main="Average Steps per Five Minute Interval",
     xlab="Interval no", ylab="Daily steps")
stepsdata[which.max(stepsdata$steps), ]$interval
```

5.  Imputing missing values
  - Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
  
```{r}
sum(is.na(rawdata))
```
  - Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
  - Create a new dataset that is equal to the original dataset but with the missing data filled in.
  - Here, what I do is, im using the random number generation (set.seed)
  
```{r}
library(dplyr)
set.seed(1234)
newdata <- floor(runif(nrow(rawdata), 
                  min = min(rawdata$steps, na.rm = T), 
                  max = max(rawdata$steps, na.rm = T)/10))

rep  <- which(is.na(rawdata$steps))
head(rep)

rawdata$steps[rep]<- newdata[rep]

complete_data  <- rawdata %>% 
                  group_by(date) %>% 
                  summarise(daily_step_count = sum(steps))


```
  - Make a histogram of the total number of steps taken each day. Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
    
```{r}
hist(complete_data$daily_step_count, 
    breaks = 10,
    main = "Histogram of total steps per day",
    xlab = "Range of step totals",
    ylab = "Number of totals in range",
    border = "black",
    col = heat.colors(5),
    las = 2,
    ylim = c(0, 25))

newmean <- mean(complete_data$daily_step_count, na.rm=TRUE)
newmean

newmedian <- median(complete_data$daily_step_count, na.rm=TRUE)
newmedian
```

6. Are there differences in activity patterns between weekdays and weekends?
  - Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
#  indicating whether a given date is a weekday or weekend day.
```{r}
rawdatanew <- read.csv("activity.csv", stringsAsFactors=FALSE)

weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}

rawdatanew$date <- as.Date(rawdatanew$date)
rawdatanew$day <- sapply(rawdatanew$date, FUN=weekday.or.weekend)
```

  - Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
  
```{r}

library(ggplot2)

averages <- aggregate(steps ~ interval + day, data=rawdatanew, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
xlab("Interval") + ylab("Number of steps") 
```

```{r, include=FALSE}
   # add this chunk to end of mycode.rmd
   file.rename(from="scripts/mycode.md", 
               to="README.md")
```

 
