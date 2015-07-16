\---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
require(knitr)
opts_chunk$set(echo = TRUE, cache = TRUE, cache.path = "cache/", fig.path = "figure/")
```



Summary goes here $\sigma$

## Loading and preprocessing the data


```r
library(lubridate)
library(dplyr)
library(lattice)
activity<-read.table("activity.csv", header=TRUE, stringsAsFactor=FALSE, sep=",")
activity_wo_na<-activity[!is.na(activity$steps),]
```







## What is mean total number of steps taken per day?

```r
grouped_by_day<- group_by(activity_wo_na, date)
summary_steps <- summarise(grouped_by_day,
  total = sum(steps))
  avgsteps<-mean(summary_steps$total)
   mediansteps<-median(summary_steps$total)
   hist(summary_steps$total, xlab="Total Number of Steps", main="Frequency of Total Number of Steps per Day", col=1, breaks=12)
   abline(v = avgsteps, col = 7, lwd = 2)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

## What is the average daily activity pattern?

```r
grouped_by_int<- group_by(activity_wo_na, interval)
summary_stepsint <- summarise(grouped_by_int,  avg = mean(steps))
plot(strptime(sprintf("%04d", summary_stepsint$interval), format="%H%M"), summary_stepsint$avg, type="l", xlab="Time of Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 


## Imputing missing values

```r
activity_na_replace<-activity

M<-summary_stepsint$interval
for(i in M) 
  {
  int_avg<-summary_stepsint[summary_stepsint$interval==i,]$avg
	if(dim(activity_na_replace[activity_na_replace$interval==i & is.na(activity_na_replace$steps),])[1]>0)
		{
activity_na_replace[activity_na_replace$interval==i & is.na(activity_na_replace$steps),]$steps = int_avg
		}
			 			 
	}
	
grouped_by_day<- group_by(activity_na_replace, date)
summary_steps <- summarise(grouped_by_day,
  total = sum(steps))
  avgsteps<-mean(summary_steps$total)
   mediansteps<-median(summary_steps$total)
   hist(summary_steps$total, xlab="Total Number of Steps", main="Frequency of Total Number of Steps per Day", col=1, breaks=12)

  abline(v = avgsteps, col = 7, lwd = 2)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 


## Are there differences in activity patterns between weekdays and weekends?

```r
activity_na_replace$wday<-as.character(wday(activity_na_replace$date))
activity_na_replace[activity_na_replace$wday==1 | activity_na_replace$wday==6,]$wday = "Weekend"
activity_na_replace[activity_na_replace$wday!="Weekend",]$wday = "Weekday"
activity_na_replace$wday<-as.factor(activity_na_replace$wday)

grouped_by_int<- group_by(activity_na_replace, interval, wday)
 summary_stepsint <- summarise(grouped_by_int,  avg = mean(steps))

int<- as.POSIXct(strptime(sprintf("%04d", summary_stepsint$interval), format="%H%M"))

xyplot(summary_stepsint$avg~int, groups =summary_stepsint$wday,type="l", auto.key=TRUE, scales=list(x=list(at=NULL)), xlab="Time (Between 00:00 and 23:55)")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

