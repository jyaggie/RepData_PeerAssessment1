# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data


```r
library(xtable)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activity<-read.table("activity.csv", header=TRUE, stringsAsFactor=FALSE, sep=",")
activity_wo_na<-activity[!is.na(activity$steps),]
grouped_by_day<- group_by(activity_wo_na, date)
```







## What is mean total number of steps taken per day?

```r
summary_steps <- summarise(grouped_by_day,
  total = sum(steps))
  mean(summary_steps$total)
```

```
## [1] 10766.19
```

```r
   median(summary_steps$total)
```

```
## [1] 10765
```

```r
   hist(summary_steps$total, xlab="Total Number of Steps", main="Frequency of Total Number of Steps per Day", col=3)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 
## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
