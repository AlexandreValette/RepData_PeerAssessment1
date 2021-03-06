---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(ggplot2)
library(plyr)

data.raw <- read.csv(unz("activity.zip","activity.csv"))
data.raw$date <- as.Date(data.raw$date)
data.process <- data.raw[!is.na(data.raw$steps),]
```


## What is mean total number of steps taken per day?

```r
data.perday <- aggregate(data.process[,-2], by=list(data.process$date),FUN = sum)

perday.median <- median(data.perday$steps)
perday.mean <- mean(data.perday$steps)

ggplot(data.perday, aes(x=steps)) + 
    geom_histogram(binwidth=1000) + labs(title = "Total number of steps each day", x= "Steps per day")
```

![](PA1_template_files/figure-html/hist-1.png)<!-- -->

```r
print(paste("Mean steps per day :",round(perday.mean,1)))
```

```
## [1] "Mean steps per day : 10766.2"
```

```r
print(paste("Median steps per day :",round(perday.median)))
```

```
## [1] "Median steps per day : 10765"
```

## What is the average daily activity pattern?

```r
data.perinterval <- aggregate(data.process[,1], by=list(data.process$interval),FUN = mean)
names(data.perinterval)<-c("interval","steps")

g <- ggplot(data = data.perinterval) + aes(x = factor(interval), y = steps, group = 1) + geom_line() + labs(x ="5 minutes interval", y = "Average number of steps across all days") + theme(axis.text.x = element_text(size = 10)) 
g + scale_x_discrete(breaks=c(0,500,1000,1500,2000,2500))
```

![](PA1_template_files/figure-html/daily-1.png)<!-- -->

The inteval with the maximum number of steps is:


```r
data.perinterval[which.max(data.perinterval$steps),]$interval
```

```
## [1] 835
```

## Imputing missing values

There number of missing values for the "steps" variable is:


```r
sum(is.na(data.raw$steps))
```

```
## [1] 2304
```
The strategy to imput missing values is to take the mean across all days for the corresponding interval.


```r
data.process <- data.raw
data.nan <- data.process[is.na(data.process),]
fn <- function(interv) {data.perinterval[data.perinterval$interval ==interv,]$steps}
data.imput <- ddply(data.nan, c("steps","interval"), transform, steps=fn(interval))
data.imput <- data.imput[order(data.imput$date, data.imput$interval),] 
data.process[is.na(data.process),]$steps <- data.imput$steps
```

Let's replot the same histogram as the first figure of this document.


```r
data.perday <- aggregate(data.process[,-2], by=list(data.process$date),FUN = sum)

perday.median <- median(data.perday$steps)
perday.mean <- mean(data.perday$steps)

ggplot(data.perday, aes(x=steps)) + 
    geom_histogram(binwidth=1000) + labs(title = "Total number of steps each day", subtitle = "(with imputing of missing values)", x= "Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
print(paste("Mean steps per day :",round(perday.mean,1)))
```

```
## [1] "Mean steps per day : 10766.2"
```

```r
print(paste("Median steps per day :",round(perday.median)))
```

```
## [1] "Median steps per day : 10766"
```

Impact to imputing missing data is low for mean and median. This is due to the imputing strategy selected.

## Are there differences in activity patterns between weekdays and weekends?

```r
data.process$date <- as.Date(data.process$date)
which_day <- weekdays(data.process$date)
data.process$which_day <- ifelse(which_day == "Saturday" | which_day == "Sunday" , c("weekend"), c("weekday"))
data.process$which_day <- factor(data.process$which_day)

data.perday.weekday <- ddply(data.process[data.process$which_day == "weekday", ], .(interval), function(data) { mean(data$steps, na.rm = TRUE)})
names(data.perday.weekday) <- c("interval","steps")
data.perday.weekend <- ddply(data.process[data.process$which_day == "weekend", ], .(interval), function(data) { mean(data$steps, na.rm = TRUE)})
names(data.perday.weekend) <- c("interval","steps")
```
Merge both dataframe


```r
data.perday.weekday$whichday <- "weekday"
data.perday.weekend$whichday <- "weekend"
data.perday <- rbind(data.perday.weekend, data.perday.weekday)
```

Plot both dataframe as time-series with ggplot2.

```r
g <- ggplot(data = data.perday) + aes(x = factor(interval), y = steps, group = 1) + geom_line() + labs(x ="5-minute interval", y = "Average number of steps across weekdays") + theme(axis.text.x = element_text(size = 10)) + facet_wrap(~whichday, nrow=2)
g + scale_x_discrete(breaks=c(0,500,1000,1500,2000,2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
