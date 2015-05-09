# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
unzip('activity.zip')
activity <- read.csv('activity.csv')
activity$date <- as.Date(activity$date)
```

The only change to process the raw file needed for this study is to adjust the type of the "date" column to reflect actual dates.   This will be used in the last section that explores differences in the activity patterns between weekdays and weekends.

## What is mean total number of steps taken per day?


```r
total_daily_steps <- tapply(activity$steps, activity$date, sum)
```

A histogram of the total number of steps per day can be seen in the table below. 


```r
hist(total_daily_steps, main = "Frequency of Daily Step Counts", xlab = "Num Steps")
```

![plot of chunk unnamed-chunk-3](./PA1_template_files/figure-html/unnamed-chunk-3.png) 


```r
mean_steps <- mean(total_daily_steps, na.rm = TRUE)
median_steps <- median(total_daily_steps, na.rm = TRUE)
```

The mean total number of steps per day is 1.0766 &times; 10<sup>4</sup> and the median total number of steps per day is 10765. 

## What is the average daily activity pattern?

To answer this we will first create a data frame that shows the avg num steps for each 5 minute interval, as follows. 


```r
avg_interval <- aggregate(activity$steps, list(activity$interval), mean, na.rm=T)
names(avg_interval) <- c("interval", "avg_steps")
```


```r
plot(avg_interval$interval, avg_interval$avg_steps, type="l", main = "Highest Mean Activity by 5 Minute Interval", xlab="Interval", ylab="Mean number of steps")
```

![plot of chunk unnamed-chunk-6](./PA1_template_files/figure-html/unnamed-chunk-6.png) 


```r
max_interval <- avg_interval$interval[which.max(avg_interval$avg_steps)]
```

We can see that the 5 minute interval # 835 contains the maximium number of avg steps = 206.1698. 

## Imputing missing values


```r
num_NA <- sum(is.na(activity$steps))
```

The number of cases in the data where the step value is NA = 2304.

We will now create a new dataset called activity_imputed with imputed values for any of the NA cases noted above.   These values will be filled by the mean number of steps for the corresponding interval across all days. 


```r
activity_imputed <- activity

for (i in 1:nrow(activity)) {
     if(is.na(activity$steps[i])) {
         activity_imputed$steps[i] <- avg_interval$avg_steps[which(avg_interval$interval==activity$interval[i])]
     }
 }
```

We will now calculate the values for the total steps taken each day in this imputed data and compare it to the values explored in the original dataset with NA's.   


```r
total_daily_steps_imp <- tapply(activity_imputed$steps, activity_imputed$date, sum)
```

A histogram of the total number of steps per day can be seen in the table below. 


```r
hist(total_daily_steps_imp, main = "Frequency of Daily Step Counts (Imputed)", xlab = "Num Steps")
```

![plot of chunk unnamed-chunk-11](./PA1_template_files/figure-html/unnamed-chunk-11.png) 


```r
mean_steps_imp <- mean(total_daily_steps_imp, na.rm = TRUE)
median_steps_imp <- median(total_daily_steps_imp, na.rm = TRUE)
```

The mean total number of steps per day is 1.0766 &times; 10<sup>4</sup> and the median total number of steps per day is 1.0766 &times; 10<sup>4</sup>.  While the median differs only slightly, the mean is equivalent suggesting the impact for imputing data is low in this instance.   


## Are there differences in activity patterns between weekdays and weekends?

First we use the weekdays() function to add a new column of booleans that note which days are weekends and which are weekdays.     From there we can quickly calculate the means of steps in each and plot.   


```r
activity_imputed$weekend <- weekdays(activity_imputed$date) %in% c("Saturday", "Sunday")

weekend_mean <- aggregate(activity_imputed$steps[activity_imputed$weekend==T], list(activity_imputed$interval[activity_imputed$weekend==T]), mean)
names(weekend_mean) <- c("interval", "avg_steps")

weekday_mean <- aggregate(activity_imputed$steps[activity_imputed$weekend==F], list(activity_imputed$interval[activity_imputed$weekend==F]), mean)
names(weekday_mean) <- c("interval", "avg_steps")

par(mfrow=c(2,1))
plot(weekend_mean$interval, weekend_mean$avg_steps, type="l", main = "Weekend", xlab="Interval", ylab="Mean number of steps")
plot(weekday_mean$interval, weekday_mean$avg_steps, type="l", main = "Weekday", xlab="Interval", ylab="Mean number of steps")
```

![plot of chunk unnamed-chunk-13](./PA1_template_files/figure-html/unnamed-chunk-13.png) 

