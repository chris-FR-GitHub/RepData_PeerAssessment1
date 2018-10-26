
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(readr)
library(lubridate, warn.conflicts = FALSE)
library(scales, warn.conflicts = FALSE)

print(paste("R", getRversion()))
paste('dplyr package : ', packageVersion('dplyr'))
paste('ggplot2 package : ', packageVersion('ggplot2'))
paste('readr package : ', packageVersion('readr'))


print(paste("R", getRversion()))
print("-------------")
for (package_name in sort(loadedNamespaces())) {
    print(paste(package_name, packageVersion(package_name)))
}
################################################################################
# read the file
activity <- read_csv('activity.csv',
                col_names=TRUE,
                col_types = list(
                    col_integer(),
                    col_date(format="%Y-%m-%d"), 
                    col_integer()),
                na = "NA")

activity$intervaltime <- as.POSIXct(paste(activity$interval %/% 100, 
                                          activity$interval %% 100, 
                                          "00", 
                                          sep=":"),
                            format="%H:%M:%S", tz="")


unique(activity$interval)

length(unique(activity$intervallab))

class(activity$interval )
head(activity)


head(activity)

################################################################################
# Q1
stepsbydaynas <- activity %>% 
    group_by(date) %>% 
    summarize(steps = sum(steps, na.rm = TRUE))

# hist(stepsbydaynas$steps, col="blue")
ggplot(data=stepsbydaynas, aes(x=steps)) + 
    geom_histogram(bins = 15, 
                   col="blue",
                   fill="blue",
                   alpha = .2) +
    theme_bw() +
    labs(
        x = 'Steps',
        title = 'Histogram - Nb. Steps by day' ) +
    geom_vline(aes(xintercept=mean(steps)),
               color="red", linetype="dashed", size=1) +
    geom_vline(aes(xintercept=median(steps)),
               color="orange", linetype="dashed", size=1) +
    annotate("text", x = 20000, y = 12, label = "mean", color="red") +
    annotate("text", x = 20000, y = 11, label = "median", color="orange")


# we ghave 10 days with NO data
stepsbyday <- activity %>% 
    filter(!is.na(steps)) %>% 
    group_by(date) %>% 
    summarize(steps = sum(steps, na.rm = TRUE))
    
ggplot(data=stepsbyday, aes(x=steps)) + 
    geom_histogram(bins = 15, 
                   col="blue",
                   fill="blue",
                   alpha = .2) +
    theme_bw() +
    labs(
        x = 'Steps',
        title = 'Histogram - Nb. Steps by day' ) +
    geom_vline(aes(xintercept=mean(steps)),
               color="red", linetype="dashed", size=1) +
    geom_vline(aes(xintercept=median(steps)),
               color="orange", linetype="dashed", size=1) +
    annotate("text", x = 20000, y = 12, label = "mean", color="red") +
    annotate("text", x = 20000, y = 11, label = "median", color="orange")

max(stepsbyday2$steps)

mean(stepsbyday$steps)
median(stepsbyday$steps)


#####################################################
# Q2
# What is the average daily activity pattern?
#     Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") 
# of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
    
names(activity)

stepsbyint <- activity %>% 
    filter(!is.na(steps)) %>% 
    group_by(intervaltime) %>% 
    summarize(steps = mean(steps, na.rm = TRUE))

stepsbyintctl <- activity %>% 
  filter(!is.na(steps)) %>% 
  group_by(interval) %>% 
  summarize(steps = mean(steps, na.rm = TRUE))

# add a Time column
# stepsbyint$intervaltime <- as.POSIXct(paste(stepsbyint$interval %/% 100, stepsbyint$interval %% 100, "00", sep=":"),
#                                       format="%H:%M:%S")
head(stepsbyint)
unique (stepsbyint$intervaltime)
class (stepsbyint$intervaltime)

ggplot(data=stepsbyint, aes(x=intervaltime, y=steps)) + 
    geom_line(col="blue") +
    theme_bw() +
    labs(
        x = 'Hour interval',
        y = 'Average number of steps',
        title = 'Average daily activity pattern' ) +
  scale_x_datetime(labels = function(x) format(x, format = "%H:%M"))

ggplot(data=stepsbyint, aes(x=intervaltime, y=steps)) + 
  geom_line(col="blue") +
  theme_bw() +
  labs(
    x = 'Time interval',
    y = 'Average number of steps',
    title = 'Average daily activity pattern' ) + 
  scale_x_datetime(date_breaks = "2 hour", date_label = "%H:%M", timezone=NULL) +
  geom_vline(
    aes(xintercept=stepsbyint[which.max(stepsbyint$steps),]$intervaltime),
    color="red", 
    linetype="dashed", 
    size=.5)


format(stepsbyint[[which.max(stepsbyint$steps),'intervaltime']], '%H:%M')


#####################################################
# Q3

# > Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

sum(is.na(activity$steps))

nrow(activity)

round(sum(is.na(activity$steps)) / nrow(activity) * 100, 1)



# 
# 
# 
# > Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# 
# 
# > Create a new dataset that is equal to the original dataset but with the missing data filled in.


activityNoNAs <- activity %>% 
  group_by(interval) %>% 
  mutate( myintervalmean = mean(steps, na.rm = TRUE) ) %>%
  mutate(steps = ifelse(is.na(steps), myintervalmean, steps)) %>%
  ungroup() %>%
  select (-myintervalmean)

paste('Number of NAs : ', sum(is.na(activityNoNAs$steps)))

head(activityNoNAs)


> Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

  
stepsbydayreplaced <- activityNoNAs %>% 
  group_by(date) %>% 
  summarize(steps = sum(steps, na.rm = TRUE))

ggplot(data=stepsbydayreplaced, aes(x=steps)) + 
  geom_histogram(bins = 15, 
                 col="blue",
                 fill="blue",
                 alpha = .2) +
  theme_bw() +
  labs(
    x = 'Steps',
    title = 'Histogram - Nb. Steps by day',
    subtitle='NAs have been replaced') +
  geom_vline(aes(xintercept=mean(steps)),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(steps)),
             color="orange", linetype="dashed", size=1) +
  annotate("text", x = 20000, y = 12, label = "mean", color="red") +
  annotate("text", x = 20000, y = 11, label = "median", color="orange")
  
  
  

#####################################################
# Q4


oldlocale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "English")
names(activityNoNAs)

activityNoNAs$isweekday <- factor(ifelse(weekdays(activity$date) %in% c("Saturday", "Sunday"), "weekend", "weekday"))

table(activityNoNAs$isweekday )

str(activityNoNAs$isweekday)

stepsbyintNoNAs <- activityNoNAs %>% 
  group_by(intervaltime, isweekday) %>% 
  summarize(steps = mean(steps, na.rm = TRUE))

stepsbyintNoNAsglobal <- activityNoNAs %>% 
  group_by(isweekday) %>% 
  summarize(steps = mean(steps, na.rm = TRUE))

ggplot(data=stepsbyintNoNAs, aes(x=intervaltime, y=steps)) + 
  geom_line(col="blue") +
  theme_bw() +
  facet_grid(isweekday~.) +
  labs(
    x = 'Time interval',
    y = 'Average number of steps',
    title = 'Average daily activity pattern' ) + 
  scale_x_datetime(date_breaks = "2 hour", date_label = "%H:%M", timezone=NULL) +
  geom_hline(data = stepsbyintNoNAsglobal, 
             aes(yintercept = steps),
             color="red", 
             linetype="dashed", 
             size=.5)


    aes(yintercept=mean(steps)),
    color="red", 
    linetype="dashed", 
    size=.5)



Sys.setlocale("LC_TIME", oldlocale)


    