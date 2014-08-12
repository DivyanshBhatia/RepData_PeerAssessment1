# PeerAssessment1
====================================
## Each code chunks naming format is question no. _ question name
All figures are also exported to figures/ folder

### This section loads the data we have used ggplot2 for plot in Q5
shows current dataset top10 rows

```r
library("ggplot2")
```

```
## Warning: package 'ggplot2' was built under R version 3.1.1
```

```r
activity_frame<- read.csv("activity.csv",header=TRUE)
```

### Old Dataset data to view is available in PA1_old else uncomment this block

```r
#activity_frame
```


### This section calculates mean number of Steps

```r
bad_frame <- complete.cases(activity_frame)
Interval <-activity_frame[,3]
Date <- activity_frame[,2]
Steps <- activity_frame[,1]
step_sum <- aggregate(Steps ~ Date,data=activity_frame[bad_frame,],FUN="sum")
hist(step_sum$Steps,breaks=15,xlab="Total Steps per day",ylab="Frequency",main="Histogram of Steps per day")
dev.copy(png,'figures/TotalStepsPerDay.png')
```

```
## png 
##   3
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r
rug(step_sum$Steps)
```

![plot of chunk Q2_MeanNumberOfSteps](figure/Q2_MeanNumberOfSteps.png) 

Here We calculate mean and median

```r
mean(step_sum$Steps)
```

```
## [1] 10766
```

```r
median(step_sum$Steps)
```

```
## [1] 10765
```


### This section works on Daily Activity Pattern

```r
step_5min_avg <-aggregate(Steps ~ Interval,data=activity_frame[bad_frame,],FUN="mean")
plot(step_5min_avg$Interval,step_5min_avg$Steps,type="l")
```

![plot of chunk Q3_AverageDailyActivityTypePattern](figure/Q3_AverageDailyActivityTypePattern.png) 

```r
dev.copy(png,'figures/step_5min_avg.png')
```

```
## png 
##   3
```

```r
dev.off()
```

```
## pdf 
##   2
```

The interval where we have max average no. of steps

```r
step_5min_avg[which(max(step_5min_avg$Steps)==step_5min_avg$Steps),"Interval"]
```

```
## [1] 835
```


### This section imputes missing values and perform calculations

#### In Question 4 to impute missing values we have replaced NA with mean of the interval i.e. If we say 0 interval so all the NAs at 0 interval have been replaced with mean of 0 interval

head shows current dataset top10 rows for clarity 
to see more data comment head line
rug is used in histogram for clarity

### No. of missing values

```r
length(which(bad_frame==FALSE))
```

```
## [1] 2304
```


```r
activity_var<-1
while(activity_var < length(Steps))
{
  
  if(bad_frame[activity_var]==FALSE)
  {
    activity_frame[activity_var,1] <- step_5min_avg[which(activity_frame[activity_var,3]==step_5min_avg$Interval),"Steps"]
  }
activity_var <- activity_var+1
}
head(activity_frame,n=10)
```

```
##      steps       date interval
## 1  1.71698 2012-10-01        0
## 2  0.33962 2012-10-01        5
## 3  0.13208 2012-10-01       10
## 4  0.15094 2012-10-01       15
## 5  0.07547 2012-10-01       20
## 6  2.09434 2012-10-01       25
## 7  0.52830 2012-10-01       30
## 8  0.86792 2012-10-01       35
## 9  0.00000 2012-10-01       40
## 10 1.47170 2012-10-01       45
```

```r
bad_frame <- complete.cases(activity_frame)
Interval <-activity_frame[,3]
Date <- activity_frame[,2]
Steps <- activity_frame[,1]
step_sum <- aggregate(Steps ~ Date,data=activity_frame[bad_frame,],FUN="sum")
hist(step_sum$Steps,breaks=15,xlab="Total Steps per day",ylab="Frequency",main="Histogram of Steps per day")
dev.copy(png,'figures/AfterImputingTotalStepsPerDay.png')
```

```
## png 
##   3
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r
rug(step_sum$Steps)
```

![plot of chunk Q4_ImputingMissingValues](figure/Q4_ImputingMissingValues.png) 


### New Dataset data to view is available in PA1_old else uncomment this block

```r
#activity_frame
```

#### Here we calculate mean and median of the newdataset after imputing missing values with mean of interval

```r
mean(step_sum$Steps)
```

```
## [1] 10766
```

```r
median(step_sum$Steps)
```

```
## [1] 10766
```

### We Compare Activity in weekends and Weekdays over different intervals weekdays() function has been used here

```r
date_col <- weekdays(as.Date(activity_frame[,2]))
head(activity_frame,n=5)
```

```
##     steps       date interval
## 1 1.71698 2012-10-01        0
## 2 0.33962 2012-10-01        5
## 3 0.13208 2012-10-01       10
## 4 0.15094 2012-10-01       15
## 5 0.07547 2012-10-01       20
```

```r
activity_frame <- cbind(activity_frame,date_col)
activity_var <- 1
week_vector <- NA
week_logic <- which(activity_frame[,"date_col"] %in% c("Saturday","Sunday"))
while(activity_var < (length(Steps)+1))
{
  if(activity_var %in% week_logic)
  {
    week_vector <- c(week_vector,"weekend")
  }
  else {
    week_vector <- c(week_vector,"weekday")
  }
activity_var <- activity_var+1
}
week_vector_na <- is.na(week_vector)
week_vector <- week_vector[!week_vector_na]
activity_frame <- cbind(activity_frame,week_vector)
activity_frame$week_vector <- factor(activity_frame$week_vector)
levels(activity_frame$week_vector)
```

```
## [1] "weekday" "weekend"
```

```r
step_5min_avg <-aggregate(Steps ~ Interval + week_vector,data=activity_frame,FUN="mean")
qplot(Interval,Steps,data=step_5min_avg,facets=week_vector~.,main="Average Steps Comparison at different intervals over Weekend and Weekdays")+geom_line()
```

![plot of chunk Q5_ActivityPatternWeekendAndWeekdays](figure/Q5_ActivityPatternWeekendAndWeekdays.png) 

```r
dev.copy(png,'figures/WeekdayWeekendComparison.png')
```

```
## png 
##   3
```

```r
dev.off()
```

```
## pdf 
##   2
```
