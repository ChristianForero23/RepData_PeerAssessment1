---
title: '"Course Project 1 - Reproducible Research"'
author: "Christian Forero"
date: "27/10/2020"
output: 
  html_document: 
    keep_md: yes
    fig_caption: yes
---
##Introduction

This is an R Markdown document, created for the Coursera course "Reproducible Research", in completion of "Peer Assessment 1". The assignment requires students to write an R markdown document evidencing literate programming, using markdown and R programming techniques. There are 5 primary questions to be answered, dealing with processing and analysing data. The data provided to be worked upon, is called "activity monitoring data".


##Step 1
Code for reading in the dataset and/or processing the data.


```r
activity<-read.csv("activity.csv")
```
Exploring the basics of this data.

```r
dim(activity)
```

```
## [1] 17568     3
```

```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
head(activity)
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
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
sapply(activity, function(x) sum(is.na(x)))
```

```
##    steps     date interval 
##     2304        0        0
```
##Step 2
Histogram of the total number of steps taken each day.


```r
x<- aggregate(activity$steps, by=list(Date=activity$date), FUN=sum, na.rm=TRUE)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
x$Date <- ymd(x$Date)
names (x)[2] = "Steps"
rownames(x) <- x$Date
hist(x$Steps, breaks=20, xlab="Number of Steps Taken", 
     main="Histogram of the Total Number of Steps Taken per Day")
```

![](Project1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

##Step 3
Mean and median values of the total number of steps taken per day.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
totalStepsSummary <- summarise(x, mean_TotalSteps=mean(x$Steps),median_TotalSteps=median(x$Steps))
print(totalStepsSummary)
```

```
##   mean_TotalSteps median_TotalSteps
## 1         9354.23             10395
```
##Step 4
Time series plot of the average number of steps taken.


```r
s<-aggregate(activity$steps, by=list(Category=activity$interval), FUN=mean,  na.rm=TRUE)
library(lubridate)
names (s)[2] = "Steps"
names (s)[1] = "Intervals"
library(ggplot2)
ggplot(s, aes(y=Steps, x=Intervals)) + geom_line()
```

![](Project1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

##Step 5
##The 5-minute interval that, on average, contains the maximum number of steps

```r
activity<- read.csv('activity.csv')
s<-aggregate(activity$steps, by=list(Category=activity$interval), FUN=mean,  na.rm=TRUE)
r<-s %>% group_by(Category) %>% slice(which.max(x))
datos <- r[with(r, order(-r$x)), ]
names (datos)[2] = "Steps"
names (s)[1] = "interval"
datos[1,]
```

```
## # A tibble: 1 x 2
## # Groups:   Category [1]
##   Category Steps
##      <int> <dbl>
## 1      835  206.
```

##Step 6
Code to describe and show a strategy for imputing missing data
There are multiple strategies to deal with multiple value imputations.


```r
activity<- read.csv('activity.csv')
sapply(activity, function(x) sum(is.na(x)))
```

```
##    steps     date interval 
##     2304        0        0
```

```r
library(dplyr)
x<- aggregate(activity$steps, by=list(interval=activity$interval), FUN=sum, na.rm=TRUE)
names (x)[2] = "steps"
for (a in 1:length(x$steps)){
  for (i in 1:length(activity$steps)){
    if (x$interval[a]== activity$interval[i]){
      if(is.na(activity$steps[i])){
        activity$steps[i]=x$steps[a]
        
      }
      
      
    }
    
  }
  
}
head(activity)
```

```
##   steps       date interval
## 1    91 2012-10-01        0
## 2    18 2012-10-01        5
## 3     7 2012-10-01       10
## 4     8 2012-10-01       15
## 5     4 2012-10-01       20
## 6   111 2012-10-01       25
```

## Step 7
Histogram of the total number of steps taken each day after missing values are imputed.


```r
hist(activity$steps, breaks=20, xlab="Number of Steps Taken", 
     main="Histogram of Total Number of Steps Taken per Day (With Imputed Values)")
```

![](Project1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

