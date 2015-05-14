# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
#### Show any code that is needed to
##### 1.Load the data (i.e. read.csv())

```r
df <- read.csv('activity.csv')
```

##### 2.Process/transform the data (if necessary) into a format suitable for your analysis
###### not necessary

## What is mean total number of steps taken per day?
#### For this part of the assignment, you can ignore the missing values in the dataset.
##### 1.Calculate the total number of steps taken per day

```r
totsteps <- aggregate(steps ~ date, df, sum)
```

##### 2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
barplot(totsteps$steps, names.arg = totsteps$date, main= "total number of steps taken per day", ylab = "steps", las = 2, cex.names = 0.6, cex.axis = 0.6)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

##### 3.Calculate and report the mean and median of the total number of steps taken per day

```r
mean(totsteps$steps)
```

```
## [1] 10766.19
```

```r
median(totsteps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
##### 1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
intvlsteps <- aggregate(steps ~ interval, df, mean)
plot(intvlsteps, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

##### 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
intvlsteps$interval[which.max(intvlsteps$steps)]
```

```
## [1] 835
```

## Imputing missing values
#### Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
##### 1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(df))
```

```
## [1] 2304
```

##### 2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
###### using he mean for that 5-minute interval

##### 3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
df1 <- merge(df, intvlsteps, by="interval")
df1$steps.x[is.na(df1$steps.x)] <- df1$steps.y[is.na(df1$steps.x)]
df1 <- df1[, c(1:3)]
```

##### 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
totsteps2 <- aggregate(steps.x ~ date, df1, sum)
barplot(totsteps2$steps.x, names.arg = totsteps2$date, main= "total number of steps taken per day", ylab = "steps", las = 2, cex.names = 0.6, cex.axis = 0.6)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
mean(totsteps2$steps.x)
```

```
## [1] 10766.19
```

```r
median(totsteps2$steps.x)
```

```
## [1] 10766.19
```
###### not much of an impact of imputing missing data on the estimates of the total daily number of steps

## Are there differences in activity patterns between weekdays and weekends?
####For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

#####1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
day <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}
df$day <- as.factor(sapply(df$date, day))
```

#####2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
par(mfrow = c(2, 1))
for (type in c("weekday", "weekend")) {
    wdaysteps <- aggregate(steps ~ interval, df, subset = df$day == type, mean)
    plot(wdaysteps, type = "l", main = type)
}
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

