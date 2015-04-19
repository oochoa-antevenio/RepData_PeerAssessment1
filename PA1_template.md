---
title: "Reproducible Research Peer Assessment 1"
author: "Oscar Ochoa (oscare1970@gmail.com)"
date: "17/04/2015"
output: html_document
---

# Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data

We proceed to read the data and convert de date variable into a POSIXct class.


```r
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./data/activity.zip",method="curl")
fileunzip <- unzip("./data/activity.zip")
activity <- read.csv(fileunzip, header=TRUE, sep=',', na.strings="NA",stringsAsFactors=F)

summary(activity)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
activity$date <- as.Date(activity$date, format="%Y-%m-%d")

Sys.setlocale("LC_TIME", "C")
```

```
## [1] "C"
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


3. Calculate and report the mean and median of the total number of steps taken per day


```r
stepsdate <- aggregate(steps ~ date, data = activity, FUN = sum)

hist(stepsdate$steps, xlab="steps", main="histogram of the number of steps taken each day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
mean(stepsdate$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(stepsdate$steps, na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
stepsaverage <- aggregate(steps ~ interval, data = activity, FUN = mean)

plot(stepsaverage, type="l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
stepsaverage$interval[which.max(stepsaverage$steps)]
```

```
## [1] 835
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. I'm going to use the mean for the 5-minute interval.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
sum(is.na(activity))
```

```
## [1] 2304
```

```r
activity2 <- activity
for (i in which(is.na(activity2))){
        activity2$steps[i] = stepsaverage$steps[((i-1)%%288)+1]
}

stepsdate2 <- aggregate(steps ~ date, data = activity2, FUN = sum)

hist(stepsdate2$steps, xlab="steps", main="histogram of the number of steps taken each day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
mean(stepsdate2$steps)
```

```
## [1] 10766.19
```

```r
median(stepsdate2$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
Sys.setlocale("LC_TIME", "C")
```

```
## [1] "C"
```

```r
activity2$date <- as.Date(activity2$date, format="%Y-%m-%d")
activity2$day <- weekdays(activity2$date)

for (i in 1:length(activity2$steps)){
        if ((activity2$day[i] == "Saturday") | (activity2$day[i] == "Sunday"))  {
                activity2$day[i] <- "weekend"
        } else {
                activity2$day[i] <- "weekday"
        }
}

activity3 <- aggregate(steps ~ interval + day, data = activity2, FUN = mean)

library(lattice)
xyplot(steps ~ interval | day, 
          data = activity3,
          type = "l",
          xlab = "Interval",
          ylab = "Number of steps",
          layout=c(1,2))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

