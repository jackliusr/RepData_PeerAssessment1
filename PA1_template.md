# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
csvfile = "activity.csv"
zipfile = "activity.zip"
if (!file.exists(csvfile)){
  unzip(zipfile)
}

library(ggplot2)
library(knitr)

data <-read.csv(csvfile, colClasses = c("numeric","Date","numeric"))
```


```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```



```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
```


```r
summary(data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.
1. Calculate the total number of steps taken per day

```r
totalStepsPerDay <- aggregate(steps ~ date, data, sum)
```
2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
with(totalStepsPerDay, {
    par(oma=c(2,0,0,0), mar=c(6.75,6.75,3,0), mgp=c(5.75,0.75,0), las=2)
    barplot(
      height=totalStepsPerDay$step,
      width= 50,
      main="Graph of Total Steps taken per Day",
      xlab="Dates",
      ylab="Steps per Day",
      names.arg=totalStepsPerDay$date,
      space=c(0)
    )
})
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

3.Calculate and report the mean and median of the total number of steps taken per day

1. Mean

```r
daily.mean.steps <- mean(totalStepsPerDay$steps, na.rm = TRUE)
daily.mean.steps
```

```
## [1] 10766.19
```
2. Median

```r
daily.median.steps <- median(totalStepsPerDay$steps, na.rm=TRUE)
daily.median.steps
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps.interval <- aggregate(steps ~ interval, data= data, FUN=mean)
plot(steps.interval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 835
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data))
```

```
## [1] 2304
```
2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The average 5-minutues interval value is used to replaced the NA value. Decimal values will be rounded up to a whole number.


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data <- merge(data, steps.interval, by= "interval", suffixes = c("",".avg_steps"))
na_rows <- is.na(data$steps)
data$steps[na_rows] <- data$steps.avg_steps[na_rows]
data <- data[,1:3]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

4.A Make a histogram of the total number of steps taken each day

```r
totalStepsPerDay <- aggregate(steps ~ date, data, sum)
with(totalStepsPerDay, {
    par(oma=c(2,0,0,0), mar=c(6.75,6.75,3,0), mgp=c(5.75,0.75,0), las=2)
    barplot(
      height=totalStepsPerDay$step,
      width= 50,
      main="Graph of Total Steps taken per Day",
      xlab="Dates",
      ylab="Steps per Day",
      names.arg=totalStepsPerDay$date,
      space=c(0)
    )
})
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

4.B  Mean

```r
daily.mean.steps <- mean(totalStepsPerDay$steps, na.rm = TRUE)
daily.mean.steps
```

```
## [1] 10766.19
```
Median

```r
daily.median.steps <- median(totalStepsPerDay$steps, na.rm=TRUE)
daily.median.steps
```

```
## [1] 10766.19
```

There is no change in mean value. Median value changes from 10765 to 10766.19.  Total daily number of steps are increased on several days.
## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
dayType <- function(dt) {
  if(weekdays(as.Date(dt)) %in% c("Saturday","Sunday")) { 
    "weekend"
  } 
  else {
    "weekday"
    }
}
data$dayType<- as.factor(sapply(data$date, dayType))
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
dayTypeIntervalSteps <- aggregate(
    data=data,
    steps ~ dayType + interval,
    FUN=mean
)
library("lattice")

xyplot(
    type="l",
    data=dayTypeIntervalSteps,
    steps ~ interval | dayType,
    xlab="Interval",
    ylab="Number of steps",
    layout=c(1,2)
)
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
