---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
Setting global option to turn warnings off

```r
knitr::opts_chunk$set(warning=FALSE)
```

## Loading and preprocessing the data



```r
activity <- read.csv("activity.csv")
View(activity)

activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity,weekday)
summary(activity)
```

```
##      steps             date               interval           weekday    
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Friday   :2592  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Monday   :2592  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Saturday :2304  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5   Sunday   :2304  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2   Thursday :2592  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   Tuesday  :2592  
##  NA's   :2304                                           Wednesday:2592
```

## What is mean total number of steps taken per day?

```r
activity_totalsteps <- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(activity_totalsteps) <- c("date", "steps")
hist(activity_totalsteps$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "darkblue", ylim = c(0,20), breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

**The mean of the total number of steps taken per day:**

```r
mean(activity_totalsteps$steps)
```

```
## [1] 9354.23
```

**The median of the total number of steps taken per day:**

```r
median(activity_totalsteps$steps)
```

```
## [1] 10395
```



## What is the average daily activity pattern?

```r
daily_activity <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(daily_activity) <- c("interval", "mean")
plot(daily_activity$interval, daily_activity$mean, type = "l", col="darkblue", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
daily_activity[which.max(daily_activity$mean), ]$interval
```

```
## [1] 835
```



## Imputing missing values


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```


```r
steps <- daily_activity$mean[match(activity$interval, daily_activity$interval)]
```


```r
activity_imputed <- transform(activity, steps = ifelse(is.na(activity$steps), yes = steps, no = activity$steps))
total_steps_imputed <- aggregate(steps ~ date, activity_imputed, sum)
names(total_steps_imputed) <- c("date", "daily_steps")
```


```r
hist(total_steps_imputed$daily_steps, col = "darkblue", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
*The mean of the total number of steps taken per day:*

```r
mean(total_steps_imputed$daily_steps)
```

```
## [1] 10766.19
```
*The median of the total number of steps taken per day:*

```r
median(total_steps_imputed$daily_steps)
```

```
## [1] 10765
```


## Are there differences in activity patterns between weekdays and weekends?

```r
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
datetype <- sapply(activity$date, function(x) {
        if (weekdays(x) == "Sábado" | weekdays(x) =="Domingo") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })
```


```r
activity_by_date <- aggregate(steps~interval + datetype, data = activity, mean, na.rm = TRUE)
library(ggplot2)
plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
       geom_line() +
       labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
       facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
