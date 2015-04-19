---
title: "PA1_template"
author: "Yong-Sheng Cheng"
date: "April 18, 2015"
output: html_document
---

## Setting up working environment

This is the report for the Peer Assessment 1 by Yong-Sheng Cheng. This file is created by R Markdown file.

For this analytic task, a data directory was created in course home directory on my Macbook for downloading data from internet:


```r
setwd("~/Documents/Courses/Reproducible_Research_JHU")
if(!file.exists("data")) {
    dir.create("data")
}
```

## Loading and preprocessing the data

Then the http address was defined for the source data (activity.zip) as fileUrl, and then the file was  downloaded from internet and unzipped.


```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile="./data/activity.zip", method="curl")
download.time <- Sys.time()
unzip("./data/activity.zip")
```

The data was downloaded at time of 2015-04-19 07:12:30.

The next step was reading the csv file into R by *read.csv()*. The *na.strings* and *colClasses* are customly defined.


```r
activity <- read.csv("./activity.csv", colClasses=c("numeric","character","numeric"),na.strings="NA")
```

The variable **interval** was read as number and further processed as "HHMM" format. Then two new variables **date.ct** and **dt.ct** were created as POSIXct format of date and datetime.


```r
activity$interval <- sprintf("%04d",activity$interval)
activity$time <- as.POSIXct(strptime(activity$interval, "%H%M"))
activity$date.ct <- as.POSIXct(strptime(activity$date, "%Y-%m-%d"))
activity$dt.ct <- as.POSIXct(strptime(paste(activity$date, activity$interval), "%Y-%m-%d%H%M"))
```

## What is mean total number of steps taken per day?

For this part of analysis, the **mean** and **median** are calculated for the total number of steps taken per day. 

Firstly, the sum of steps was calculated for each day and assigned as stepSum.


```r
stepSum <- tapply(activity$steps, activity$date.ct, sum, na.rm=TRUE)
```

Then the **mean** and **median** were calculated from function *mean()* and *median()*.


```r
median(stepSum)
```

```
## [1] 10395
```

```r
mean(stepSum)
```

```
## [1] 9354
```

The histogram of the total number of steps taken per day was plotted over the time period.


```r
hist(stepSum, breaks=10, main="Histogram of total number of steps per day", xlab="Total number of steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

## What is the average daily activity pattern?

In order to see the pattern of daily activity, a time serie plot of average steps per day was plotted over the time variable. The average plot is a nice reflection of human activity. Majority of the activity is started from 6am to 9pm. From the average plot, you could also see that the most active time is from 8am to 9am. By function *which.max()*, the maximal active time point is determined as 8:35 am.


```r
stepMean <- tapply(activity$steps, activity$interval, mean, na.rm=TRUE)
plot(unique(activity$time),stepMean, type="l", main="Average steps per 5 minute interval", xlab="Time")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
which.max(stepMean)
```

```
## 0835 
##  104
```

## Imputing missing values

Next, let's check the NA values in the dataset activity. From the result we could see there is 2304 NA value in the steps variable, but not others.


```r
apply(is.na(activity),2,sum)
```

```
##    steps     date interval     time  date.ct    dt.ct 
##     2304        0        0        0        0        0
```

So the next step is imputing the NA value from the median of steps at each time interval by using function *impute()* from Hmisc package.

```r
require(Hmisc)
lst <- split(activity, activity$interval)
for (i in seq_along(lst)) {
    lst[[i]]$steps <- impute(lst[[i]]$steps, median)
}
activity.impute <- do.call(rbind, lst)
activity.impute <- activity.impute[with(activity.impute, order(date)),]
```

Then the histogram of total steps per day and the **mean** and **median** of total steps were calculated again from imputed dataset without any NA values. There is only slight increase in **mean**. The histogram and the **median** are the same as that from non-imputed dataset.


```r
stepSum.impute <- tapply(activity.impute$steps, activity.impute$date.ct, sum)
hist(stepSum.impute, breaks=10, main="Histogram of imputed total number of steps per day", xlab="Total number of steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

```r
median(stepSum.impute)
```

```
## [1] 10395
```

```r
mean(stepSum.impute)
```

```
## [1] 9504
```

## Are there differences in activity patterns between weekdays and weekends?

In order to check the difference in activity patterns between weekdays and weekends, a new variable called weekday was created to distinguish weekdays or weekend. It take two value, "weekday" and "weekend", based on the result of weekdays of date.ct by function *weekdays()*.


```r
weekendIdx <- weekdays(activity.impute$dt.ct) %in% c("Saturday","Sunday")
activity.impute$weekday <- factor(weekendIdx, labels=c("weekday", "weekend"))
```

Then the average steps at each time point of a day was plotted for weekday and weekend separately. The difference is rather clear. For weekend, the average steps during the day is higher than that from weekdays.


```r
odpar <- par(no.readonly=TRUE)
par(mfrow=c(2,1))
activity.weekday <- activity.impute[!weekendIdx,]
activity.weekend <- activity.impute[weekendIdx,]
stepMean.weekday <- tapply(activity.weekday$steps, activity.weekday$interval, mean)
stepMean.weekend <- tapply(activity.weekend$steps, activity.weekend$interval, mean)
plot(unique(activity.impute$time),stepMean.weekday, type="l", main="Average steps per 5 minute interval for weekdays", xlab="Time")
plot(unique(activity.impute$time),stepMean.weekend, type="l", main="Average steps per 5 minute interval for weekend", xlab="Time")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

```r
par(odpar)
```
