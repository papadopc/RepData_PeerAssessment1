---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Loading and preprocessing the data
First, load and process the data with group_by from package dplyr.
The totals are calculated using na.rm=TRUE, in order to make meaningful plots 
later.
We're also grouping by date

```r
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
indata<-read.csv("activity.csv")
indata$interval<-as.factor(indata$interval)
bydate<-group_by(indata,date)
totaldf<-summarise(bydate,totals=sum(steps,na.rm=TRUE))
```

## What is mean total number of steps taken per day?
Each line gives the corresponding R code, for histogram, mean and median

```r
hist(totaldf$totals,xlab="Total steps taken per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
mean(totaldf$totals)
```

```
## [1] 9354.23
```

```r
median(totaldf$totals)
```

```
## [1] 10395
```


## What is the average daily activity pattern?
We group by interval now

```r
byinterval<-group_by(indata,interval)
int_df<-summarise(byinterval,averages=mean(steps,na.rm=TRUE))
plot(int_df$interval,int_df$averages,type="l",xlab="Interval (minutes)",ylab="Avg. steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
int_df[which.max(int_df$averages),1]
```

```
## Source: local data frame [1 x 1]
## 
##   interval
## 1      835
```

The last line gives the peak activity time (interval)

## Imputing missing values
### Replace NAs with the closest integer of the mean corresponding to this interval

```r
newdata<-indata %>% group_by(interval) %>% mutate(newsteps=ifelse(is.na(steps),round(mean(steps,na.rm=TRUE)),steps))
newbydate<-group_by(newdata,date)
newtotal<-summarise(newbydate,totals=sum(newsteps))
hist(newtotal$totals)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
mean(newtotal$totals)
```

```
## [1] 10765.64
```

```r
median(newtotal$totals)
```

```
## [1] 10762
```

The median and mean values are different now, and much closer to each other.
By replacing the NA values with the averages per interval, we have smoothed out the distribution, reducing
the contribution of tails and increasing the contribution of the middle


## Are there differences in activity patterns between weekdays and weekends?
This would probably be easier with ggplot2, but the base package works well too

```r
#change date column to name of day
newdata$date<-weekdays(as.Date(as.character(newdata$date)))
newdata<-mutate(newdata,isweekend=ifelse(date %in% c("Saturday","Sunday"),"weekend","weekday") )
par(mfrow=c(2,1))
weekends<-newdata[newdata$isweekend=="weekend",]
weekdays<-newdata[newdata$isweekend=="weekday",]
int_wkend<-weekends %>% group_by(interval) %>% summarise(averages=mean(newsteps))
int_wkday<-weekdays %>% group_by(interval) %>% summarise(averages=mean(newsteps))
plot(int_wkend$interval,int_wkend$averages,type="l",xlab="Interval",main="Weekeend")
plot(int_wkday$interval,int_wkday$averages,type="l",xlab="Interval",main="Weekday")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

There are differences, in the weekdays the morning peak (at about 8:30) is more pronounced
