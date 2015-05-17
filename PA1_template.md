# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library("data.table")
library("dplyr")
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:data.table':
## 
##     between, last
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
library("lubridate")
```

```
## 
## Attaching package: 'lubridate'
## 
## The following objects are masked from 'package:data.table':
## 
##     hour, mday, month, quarter, wday, week, yday, year
```

```r
# for local use
setwd("~/Devel/R/DataScience/RepRes/Project1/Submit")
inFile <- "../activity.csv"
dt <- read.table(inFile, header = TRUE, stringsAsFactors = FALSE, sep = ",",
                 colClasses = c("numeric", "character", "numeric"))
# 288 obs of 3 vars for 61 days
dt[,"date"] <- ymd(dt[,"date"])  # convert dates from character to POSIXct
```

## What is mean total number of steps taken per day?

```r
# group data by date, plot a histogram of avg number of steps/day,
# also compute mean and median
days <- group_by(dt, date)
# from StackOverflow
sums <- days %>% summarise_each(funs(sum(., na.rm = TRUE)))
# this don't work
#sums <- days$steps %>% summarise_each(funs(sum(., na.rm = TRUE)))
# actually, I want to keep the NAs, rather than replace them with zeros
sumsNA <- days %>% summarise_each(funs(sum(., na.rm = FALSE)))
hist(sumsNA$steps,
     main = "Number of Steps / Day",
     xlab = "Number of Steps",
     ylab = "Frequency",
     breaks = 16)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
# also want mean and median - need to tell it to ignore NAs
meanSteps <- mean(sumsNA$steps, na.rm=TRUE)
medianSteps <- median(sumsNA$steps, na.rm=TRUE)
# mean 10766.189, median 10765
```

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
