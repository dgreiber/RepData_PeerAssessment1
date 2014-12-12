
# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data



- Load data from csv file
  

```r
activity <- read.csv("activity.csv")
```

- Process/transform the data (if necessary) into a format suitable for your analysis

```r
totalSteps <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```


- Make a histogram of the total number of steps taken each day

```r
hist(totalSteps$steps, col = "green", xlab = "Total Steps per Day", ylab = "Frequency", 
     main = "Histogram of Total Steps Taken Per Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 


- Calculate and report the mean and median total number of steps taken per day

```r
showMean <- mean(totalSteps$steps)
showMedian <- median(totalSteps$steps)
cat("The mean total number of steps per day is ",showMean,"\n")
```

```
## The mean total number of steps per day is  10766.19
```

```r
cat("The median total number of steps per day is ",showMedian,"\n")
```

```
## The median total number of steps per day is  10765
```


- Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepsInterval <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
plot(steps ~ interval, data = stepsInterval, type = "l", xlab = "Time Intervals (5-minute)", 
     ylab = "Mean number of steps taken (all Days)", main = "Average number of Steps Taken at different 5 minute Intervals", 
     col = "red")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 


- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
showStepInterval <- stepsInterval[which.max(stepsInterval$steps), ]$interval
cat("It is the ",showStepInterval,"th interval that contains the max # of steps\n")
```

```
## It is the  835 th interval that contains the max # of steps
```


- Calculate and report the total number of missing values in the dataset

```r
showSum <- sum(is.na(activity))
showSum
```

```
## [1] 2304
```

- Strategy for filling in all of the missing values in the dataset

```r
interval2steps <- function(interval) {
  stepsInterval[stepsInterval$interval == interval, ]$steps
}
```


- Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityFilled <- activity
count = 0
for (i in 1:nrow(activityFilled)) {
  if (is.na(activityFilled[i, ]$steps)) {
    activityFilled[i, ]$steps <- interval2steps(activityFilled[i, ]$interval)
    count = count + 1
  }
}
cat("Total ", count, "NA values were filled.\n")
```

```
## Total  2304 NA values were filled.
```


- Make a histogram of the total number of steps taken each day and Calculate and report 
 the mean and median total number of steps taken per day.

```r
totalStepsPerDays <- aggregate(steps ~ date, data = activityFilled, sum)
hist(totalStepsPerDays$steps, col = "green", xlab = "Total Number of Steps", 
     ylab = "Frequency", main = "Histogram of Total Number of Steps Taken Each Day")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 


- Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
activityFilled$day = ifelse(as.POSIXlt(as.Date(activityFilled$date))$wday%%6 == 0, "weekend", "weekday")
# For Sunday and Saturday : weekend, Other days : weekday
activityFilled$day = factor(activityFilled$day, levels = c("weekday", "weekend"))
```

- Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
stepsInterval2 = aggregate(steps ~ interval + day, activityFilled, mean)
library(lattice)
xyplot(steps ~ interval | factor(day), data = stepsInterval2, aspect = 1/2, type = "l")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

