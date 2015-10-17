Reproducible Research: Peer Assessment 1
========================================
by shilm

github repo with RMarkdown source code:
https://github.com/shilmi/RepData_PeerAssessment1-1;

HTML format view: https://htmlpreview.github.io/?https://github.com/shilmi/RepData_PeerAssessment1-1/blob/master/PA1.html 

## Loading and preprocessing the data
We assume that the reader set the correct R working directory with the setwd() function.

1. Load the data (i.e. read.csv())
```{r}
# Clear the workspace
rm(list=ls())

# Load the raw activity data
activity1 <- read.csv("activity.csv", stringsAsFactors=FALSE)
```

2. Process/transform the data (if necessary) into a format suitable for your analysis
```{r}
# Clear the workspace
# Transform the date attribute to an actual date format
activity1$date <- as.POSIXct(activity1$date, format="%Y-%m-%d")

# Compute the weekdays from the date attribute
activity1 <- data.frame(date=activity1$date, 
                           weekday=tolower(weekdays(activity1$date)), 
                           steps=activity1$steps, 
                           interval=activity1$interval)

# Compute the day type (weekend or weekday)
activity1 <- cbind(activity1, 
                      daytype=ifelse(activity1$weekday == "saturday" | 
                                     activity1$weekday == "sunday", "weekend", 
                                     "weekday"))

# Create the final data.frame
activity.final <- data.frame(date=activity1$date, 
                       weekday=activity1$weekday, 
                       daytype=activity1$daytype, 
                       interval=activity1$interval,
                       steps=activity1$steps)

```

display the first few rows of the activity data frame:
```{r}
head(activity.final)
```

## What is the mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.
1. Make a histogram of the total number of steps taken each day

```{r}
# Compute the total number of steps each day (NA values removed)
sum_data <- aggregate(activity.final$steps, by=list(activity.final$date), FUN=sum, na.rm=TRUE)

# Rename the attributes
names(sum_data) <- c("date", "total")
```
display the first few rows of the sum_data data frame:
```{r}
head(sum_data)
```

The histogram is given by the following lines of code:
```{r}
# Compute the histogram of the total number of steps each day
hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="grey", 
     xlab="Total number of steps", 
     ylim=c(0, 20), 
     main="Histogram of the total number of steps taken each day\n(NA removed)")
```


2. Calculate and report the **mean** and **median** total number of
   steps taken per day

```{r}
mean(sum_data$total)
median(sum_data$total)
```
These formulas gives a mean and median of **9354** and **10395** respectively.

## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute
   interval (x-axis) and the average number of steps taken, averaged
   across all days (y-axis)

```{r}
# Clear the workspace
rm(sum_data)

# Compute the means of steps accross all days for each interval
mean_data <- aggregate(activity.final$steps, 
                       by=list(activity.final$interval), 
                       FUN=mean, 
                       na.rm=TRUE)

# Rename the attributes
names(mean_data) <- c("interval", "mean")

head(mean_data)
```

The time serie plot is created by the following lines of code

```{r}
# Compute the time series plot
plot(mean_data$interval, 
     mean_data$mean, 
     type="l", 
     col="blue", 
     lwd=2, 
     xlab="Interval [minutes]", 
     ylab="Average number of steps", 
     main="Time-series of the average number of steps per intervals\n(NA removed)")
```


2. Which 5-minute interval, on average across all the days in the
   dataset, contains the maximum number of steps?

```{r}
# We find the position of the maximum mean
max_pos <- which(mean_data$mean == max(mean_data$mean))

# We lookup the value of interval at this position
max_interval <- mean_data[max_pos, 1]

max_interval
```

The 5-minute interval that contains the maximum of steps, on average across all days, is **835**.


## Inputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
1. Calculate and report the total number of missing values in the
   dataset (i.e. the total number of rows with `NA`s)

```{r}
# We use the trick that a TRUE boolean value is equivalent to 1 and a FALSE to 0.
NA_count <- sum(is.na(activity.final$steps))
```
The number of `NA’s` is **2304**.

2. Devise a strategy for filling in all of the missing values in the
   dataset. The strategy does not need to be sophisticated. For
   example, you could use the mean/median for that day, or the mean
   for that 5-minute interval, etc.
```{r}
# Find the NA positions
na_pos <- which(is.na(activity.final$steps))

# Create a vector of means
mean_vec <- rep(mean(activity.final$steps, na.rm=TRUE), times=length(na_pos))
```


I will use the **means** for the 5-minute intervals as fillers for missing (`NA`)
values.

3. Create a new dataset that is equal to the original dataset but with
   the missing data filled in.

```{r}
# Replace the NAs by the means
activity.final[na_pos, "steps"] <- mean_vec

head(activity.final)
```

4. Make a histogram of the total number of steps taken each day and
   Calculate and report the **mean** and **median** total number of
   steps taken per day. Do these values differ from the estimates from
   the first part of the assignment? What is the impact of imputing
   missing data on the estimates of the total daily number of steps?

```{r}
# Compute the total number of steps each day (NA values removed)
sum_data <- aggregate(activity.final$steps, by=list(activity.final$date), FUN=sum)

# Rename the attributes
names(sum_data) <- c("date", "total")

# Compute the histogram of the total number of steps each day
hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="grey", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Histogram of the total number of steps taken each day\n(NA replaced by mean value)")
```

The mean and median are computed like
```{r}
mean(sum_data$total)
median(sum_data$total)
```

These formulas gives a mean and median of **10766** and **10766** respectively.

The impact of the missing data seems rather low, at least when
estimating the total number of steps per day.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels --
   "weekday" and "weekend" indicating whether a given date is a
   weekday or weekend day.

```{r, cache=TRUE}
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activity1$daytype <- as.factor(sapply(activity1$date, daytype))
```

2. Make a panel plot containing a time series plot (i.e. `type = "l"`)
   of the 5-minute interval (x-axis) and the average number of steps
   taken, averaged across all weekday days or weekend days
   (y-axis).

```{r}
# Clear the workspace
rm(sum_data)

# Load the lattice graphical library
library(lattice)

# Compute the average number of steps taken, averaged across all daytype variable
mean_data <- aggregate(activity.final$steps, 
                       by=list(activity.final$daytype, 
                               activity.final$weekday, activity.final$interval), mean)

# Rename the attributes
names(mean_data) <- c("daytype", "weekday", "interval", "mean")

head(mean_data)
```

The time series plot take the following form:
```{r}
# Compute the time serie plot
xyplot(mean ~ interval | daytype, mean_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))
```

