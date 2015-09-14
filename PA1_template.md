#Reproducible Research : Assignment 1
# 1
###Loading and preprocessing the data


```r
unzip("C:/Users/Alket/Documents/repdata-data-activity.zip")
activity <- read.csv("activity.csv")
```
### Structure of the activity data.frame

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

# 2
### The total number of steps is calculated as bellow : 

```r
totalSteps <- tapply(activity$steps, activity$date, sum, na.rm=T)
```
#Histogram

### The histogram of total number of steps : 

```r
hist(totalSteps, breaks = 10)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

# Mean

### The mean calculated on the total number of steps 

```r
stepMean <- mean(totalSteps)
stepMean
```

```
## [1] 9354.23
```

#Mediana
### The mediana calculated on the total number of steps 

```r
stepMedian <- median(totalSteps)
stepMedian
```

```
## [1] 10395
```

# 3
#What is the average daily activity pattern?
### Loading the necessary  R library ggplot2


```r
library(ggplot2)
```
### Put in averageDaily the aggregated result to be ploted as timeserie

```r
averageDaily <- aggregate(x = list(steps = activity$steps), by = list(interval = activity$interval), FUN = mean, na.rm = TRUE)
```

### The timeserie graph of the pattern

```r
ggplot(data = averageDaily, aes(x = interval,y = steps)) + geom_line()+xlab("5-minute interval") + ylab("average number of steps taken")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

### The maximum number of steps 

```r
max <- averageDaily[which.max(averageDaily$steps), ]
max
```

```
##     interval    steps
## 104      835 206.1698
```

#4
# Imputing missing values: devising the strateghy 

### the total number of missing values: 

```r
sum(is.na(activity))
```

```
## [1] 2304
```


### The strateghy for filling the missing number of values will be as follows: the missing values will be filled with the average of the existing  values

#The function for filling the missing values


```r
fillValues <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps)) 
        filled <- c(steps) else filled <- (averageDaily[averageDaily$interval == interval, "steps"])
    return(filled)
}
```
#Create a new dataset that will have the missed data filled in with the average value 

```r
filledData <- activity
filledData$steps <- mapply(fillValues, filledData$steps, filledData$interval)
totalSteps <- tapply(filledData$steps, filledData$date, FUN = sum)
```

# the histogram of the new dataset with the values filled in 


```r
hist(totalSteps, breaks = 10)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

knit2html()
### the mean of total number of steps after filling the dataset


```r
meanTotalSteps <- mean(totalSteps)
meanTotalSteps
```

```
## [1] 10766.19
```
### the meadiana of tot number of steps after filling the dataset 

```r
medianTotalSteps <- median(totalSteps)
medianTotalSteps
```

```
## [1] 10766.19
```

# the difference between patterns ....
