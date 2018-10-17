---
title: "Reproducible Research Assignment 1"
output: html_document
keep_md:True
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r}
library(dplyr)
library(ggplot2)
summary(cars)
```

##Loading and preprocessing the data

```{r}
activity <- read.csv("activity.csv")
str(activity)
head(activity)
summary(activity)
```


```{r}
##Removing the missing values.
act.complete <- na.omit(activity)
```

##What is the mean total number of steps taken per day?
```{r}
act.day <- group_by(act.complete, date)
act.day <- summarize(act.day, steps=sum(steps))
summary(act.day)
```

####Plotting a histogram for the above data
```{r}
qplot(steps, data=act.day)
```

####Calculate and report the mean and median of the total number of steps taken per day
```{r}
mean(act.day$steps)
median(act.day$steps)
```

##What is the average daily activity pattern?
```{r}
act.int <- group_by(act.complete, interval)
act.int <- summarize(act.int, steps=mean(steps))
```

####Plotting the graph
```{r}
ggplot(act.int, aes(interval, steps)) + geom_line()
```

####Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
act.int[act.int$steps==max(act.int$steps),]
```

##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
nrow(activity)-nrow(act.complete)
#Filling in all of the missing values in the dataset
names(act.int)[2] <- "mean.steps"
act.impute <- merge(activity, act.int)
#New dataset with filled in missing values
act.impute$steps[is.na(act.impute$steps)] <- act.impute$mean.steps[is.na(act.impute$steps)]
```
####Plotting the graph
```{r}
act.day.imp <- group_by(act.impute, date)
act.day.imp <- summarize(act.day.imp, steps=sum(steps))
qplot(steps, data=act.day.imp)
```
####Calculating mean and median with the new dataset
```{r}
mean(act.day.imp$steps)
median(act.day.imp$steps)
```
##Analysing the activity pattern differences on weekdays and weekends.
```{r}
#Creating a weekday and weekend dataset
act.impute$dayofweek <- weekdays(as.Date(act.impute$date))
act.impute$weekend <-as.factor(act.impute$dayofweek=="Saturday"|act.impute$dayofweek=="Sunday")
levels(act.impute$weekend) <- c("Weekday", "Weekend")
#separate data frames for weekends and weekdays:
act.weekday <- act.impute[act.impute$weekend=="Weekday",]
act.weekend <- act.impute[act.impute$weekend=="Weekend",]
```
####Calculating mean number of steps across days for each 5 minute interval for weekday and weekend
```{r}
act.int.weekday <- group_by(act.weekday, interval)
act.int.weekday <- summarize(act.int.weekday, steps=mean(steps))
act.int.weekday$weekend <- "Weekday"
act.int.weekend <- group_by(act.weekend, interval)
act.int.weekend <- summarize(act.int.weekend, steps=mean(steps))
act.int.weekend$weekend <- "Weekend"

act.int <- rbind(act.int.weekday, act.int.weekend)
act.int$weekend <- as.factor(act.int$weekend)
ggplot(act.int, aes(interval, steps)) + geom_line() + facet_grid(weekend ~ .)
```