# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
The following code unzips the "activity.zip" file if it hasn't already been done, and reads the file into a dataframe called "activity".


```r
setwd("~/Desktop/Aina/RepResearch/Week2/PeerAssessment1")
if(!"activity.csv" %in% list.files()){
        library(utils)
        unzip("activity.zip")
}
activity <-  read.csv("activity.csv", header = TRUE)
```



## What is mean total number of steps taken per day?

One can get an idea of the total number of steps taken each day by looking at the following histogram:


```r
stepsperday <- tapply(activity$steps, factor(activity$date),sum)
hist(stepsperday, breaks = 10, 
     main = "Histogram of the number of steps per day", 
     xlab = "Total number of steps per day",
     col = "turquoise", border = "white")
```

![](PA1_template_files/figure-html/stepsperday-1.png)<!-- -->

```r
MEA <- as.integer(mean(stepsperday, na.rm = TRUE))
MED <- median(stepsperday, na.rm = TRUE)
```

The average number of steps per day is 10766, whereas the median number of steps per day is 10765.

## What is the average daily activity pattern?

Here is a time series of the 5-minute interval and the average number of steps taken, averaged across all days of the experiment. 


```r
dpatt <- tapply(activity$steps, factor(activity$interval), mean, na.rm=TRUE)
plot(row.names(dpatt), dpatt, type = "l", col="blue",
     main = "Average daily activity pattern", 
     xlab = "minutes of the day",
     ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/daily-1.png)<!-- -->

```r
MAXS <- row.names(dpatt)[dpatt == max(dpatt)]
```

The maximum number of steps is done in the interval 835, which corresponds 
roughly to 2 pm.

## Imputing missing values


```r
ina <- which(is.na(activity$steps))
NNAS <-  length(ina)
```

There are 2304 missing values in this dataset. I used the following code to 
replace those missing values with the average for that interval, taken across all 
days. 


```r
activity2 <- activity
for(i in ina){
     activity2$steps[i] <- dpatt[row.names(dpatt) == activity$interval[i]]  
}
```

The resulting dataset does not differ much from the previous one. Understandably, 
there is a slightly higher total number of steps per day, but the pattern is roughly the same: 


```r
stepsperday2 <- tapply(activity2$steps, factor(activity2$date),sum)
hist(stepsperday2, breaks = 10, 
     main = "Histogram of the number of steps per day,\n with missing data imputed", 
     xlab = "Total number of steps per day",
     col = "turquoise", border = "white")
```

![](PA1_template_files/figure-html/stepsperday2-1.png)<!-- -->

```r
MEA2 <- mean(stepsperday2)
MED2 <- median(stepsperday2)
```

Now the median and the mean are the same, that is 1.0766189\times 10^{4} steps per day.

## Are there differences in activity patterns between weekdays and weekends?
To answer this question, the first step is to create a new factor variable in the
dataframe that contains either "weekday" or "weekend" depending on the date.


```r
activity2$day  <- vector(mode = "character",length = length(activity$steps))
activity2$day <- "weekday"
activity2$day[weekdays(as.Date(activity2$date)) == "Saturday"]  <- "weekend"
activity2$day[weekdays(as.Date(activity2$date)) == "Sunday"]  <- "weekend"
```

If we now compare the daily activity pattern on weekdays and weekends we see that
on week-ends there is a lot more walking activity during the day and 
things happen slightly later than on weekdays:


```r
dpattWD <- tapply(activity2$steps[activity2$day == "weekday"], 
                  factor(activity2$interval[activity2$day == "weekday"]), 
                  mean)
dpattWE <- tapply(activity2$steps[activity2$day == "weekend"], 
                  factor(activity2$interval[activity2$day == "weekend"]), 
                  mean)
par(mfrow = c(2,1))
plot(row.names(dpattWD), dpattWD, type = "l", col="blue",
     main = "Average daily activity pattern on weekdays", 
     xlab = "minutes of the day",
     ylab = "Average number of steps")
plot(row.names(dpattWE), dpattWE, type = "l", col="blue",
     main = "Average daily activity pattern on weekends", 
     xlab = "minutes of the day",
     ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/comparison-1.png)<!-- -->
