library(tidyverse)
library(magrittr)
library(lubridate)
library(gridExtra)
library(mice)
library(VIM)
options(digits = 0, scipen = 999)
Sys.setenv(LANG = "en") ###didn't change language 
setwd(dir = "C:\\Users\\yousri.hajri\\Documents\\DATA_SCIENCE\\5_REPRODUCIBLE_RESEARCH\\WEEK2\\COURSE_PROJECT")


#Loading and preprocessing the data
###################################

##Getting tha data
file = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(file, destfile = "activity.zip")
cols <- c('steps' = "numeric",
         'date' = "character",
         'interval' = "integer")
activity <- read.csv(unzip ("activity.zip"), colClasses = cols)
str(activity)

##Preparing the data
activity$time <- sprintf("%04d", activity$interval) %>%
    strptime("%H%M") %>%
    format("%H:%M")
activity$datetime <- paste(activity$date, activity$time, sep = " ") %>% as.POSIXct()
str(activity)
summary(activity)


#What is mean total number of steps taken per day?
##################################################



## Study of steps per day (sum, mean & median)
### For this part of the assignment, you can ignore the missing values in the dataset
activity_sum <- activity %>%
    .[complete.cases(.),] %>%
    aggregate(steps~date,., FUN=sum) %>%
    rename(sum=steps)
activity_sum$date %<>% as.Date()
str(activity_sum)

### Histogram of the total number of steps taken each day
plot_hist <- c()
for (i in 1:9) {
plot_hist[[i]] <- ggplot (activity_sum, aes(sum)) +
    geom_histogram(bins = i*5) +
    ggtitle(paste("bins=",i*5))
}
gridofplots <- grid.arrange(grobs=plot_hist, ncol=3, top="Histograms")

### The mean
mean_day <- mean(activity_sum$sum)
print(paste("Mean of of the total number of steps taken per day ", round(mean_day)))

### The median
median_day <- median (activity_sum$sum)
print(paste("Median of of the total number of steps taken per day ", round(median_day)))

### The distribution
plot_final <- ggplot (activity_sum, aes(sum)) +
    geom_density(fill="grey") +
    ggtitle("Frequency of number of steps per day") +
    geom_vline(aes(xintercept=mean_day), color="red") +
    geom_vline(aes(xintercept=median_day), color="red")
plot_final

#What is the average daily activity pattern?
############################################

## Study of number of steps by the 5mn interval
activity_int <- activity %>%
    .[complete.cases(.),] %>%
    aggregate(steps~time,., FUN=mean) %>%
    rename(mean_steps=steps)

str(activity_int)

breaks <- activity_int$time %>% substr(1,2) %>% unique() %>% paste(":00",sep="")

ggplot (data = activity_int, aes(x=time, y=mean_steps, group=1)) +
    geom_line() + 
    scale_x_discrete(name="intervals", breaks=breaks) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

## max of mean_steps time
subset (activity_int, mean_steps==max(mean_steps), select=time)

#Imputing missing values
########################

## NA's detection
activity_miss = aggr(activity, col=mdc(1:2),
                   numbers=TRUE, sortVars=TRUE,
                   labels=names(activity),
                   cex.axis=.7,
                   gap=3,
                   ylab=c("Proportion of missingness","Missingness Pattern"))
table(is.na(activity$steps))
####13% of col steps are missing (2304/17568)

## NA's imputation
activity_new <- merge(activity, activity_int, by="time")
activity_new$steps_new <- ifelse(test = (is.na(activity_new$steps)),
                                 activity_new$mean_steps,
                                 activity_new$steps)
activity_new <- select(activity_new, date, interval, steps_new) %>% arrange(date)
sum(is.na(activity_new))
str(activity_new)
summary(activity_new)

### Repeat activity_sum with the new dataset activity_new
activity_sum_new <- activity_new %>%
    .[complete.cases(.),] %>%
    aggregate(steps_new~date,., FUN=sum) %>%
    rename(sum=steps_new)
activity_sum_new$date %<>% as.Date()
str(activity_sum_new)

### Histogram of the total number of steps taken each day on activity_new
plot_hist_new <- c()
for (i in 1:9) {
    plot_hist_new[[i]] <- ggplot (activity_sum_new, aes(sum)) +
        geom_histogram(bins = i*5) +
        ggtitle(paste("bins=",i*5))
}

gridofplots <- grid.arrange(grobs=plot_hist_new, ncol=3, top="Histograms")

### The mean
mean_day <- mean(activity_sum_new$sum)
print(paste("Mean of of the total number of steps taken per day ", round(mean_day)))

### The median
median_day <- median (activity_sum_new$sum)
print(paste("Median of of the total number of steps taken per day ", round(median_day)))

### The distribution and the evolution after imputation
plot_final_new <- ggplot (activity_sum_new, aes(sum)) +
    geom_density(fill="grey") +
    ggtitle("Frequency of number of steps per day") +
    geom_vline(aes(xintercept=mean_day), color="red") +
    geom_vline(aes(xintercept=median_day), color="red")
gridofplots <- grid.arrange(plot_final,plot_final_new, ncol=2, top="Difference")
gridofplots <- grid.arrange(plot_hist[[2]],plot_hist_new[[2]], ncol=2, top="Difference")

#Are there differences in activity patterns between weekdays and weekends?
##########################################################################


## Preparing the data
activity_new$time <- sprintf("%04d", activity_new$interval) %>%
    strptime("%H%M") %>%
    format("%H:%M")
activity_new$datetime <- paste(activity_new$date, activity_new$time, sep = " ") %>% as.POSIXct()
activity_new$weekday <- weekdays(activity_new$datetime)
activity_new$type_of_day <- ifelse(test = activity_new$weekday %in% c("samedi", "dimanche"),
                                   yes = "weekend",
                                   no = "weekday" ) %>% as.factor()

str(activity_new)
summary(activity_new)

## Study of number of steps by the 5mn interval
activity_int <- activity_new %>%
    aggregate(steps_new~time+type_of_day,., FUN=mean) %>%
    rename(mean_steps=steps_new)

str(activity_int)

breaks <- activity_int$time %>% substr(1,2) %>% unique() %>% paste(":00",sep="")

ggplot (data = activity_int, aes(x=time, y=mean_steps, group=1)) +
    geom_line() + 
    scale_x_discrete(name="intervals", breaks=breaks) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_grid(type_of_day~.)

