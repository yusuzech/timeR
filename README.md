# timeR

[![Travis build status](https://travis-ci.org/yusuzech/timeR.svg?branch=master)](https://travis-ci.org/yusuzech/timeR)
![](https://cranlogs.r-pkg.org/badges/grand-total/timeR)
![](https://cranlogs.r-pkg.org/badges/timeR)
![](https://cranlogs.r-pkg.org/badges/last-day/timeR)
## A simple package for timing your code.

`timeR` package allows you to create a *timer* object
to easily time your codes. Meanwhile, all records are saved to a data frame, so it's easy to retrieve all the records for later use.

Timing codes is not difficult but can be very tedious. With `timeR`, you can save your energy on timing and put more effort on 
your analysis. You can use `timeR` to time training time for machine learning models, record speed for requests when running web-scraping scripts or other situations that you need to keep records of time.

## How to install

```r
install.packages("timeR")
# or install from github for the newest version,recommened as it fixes all bugs currently known.
devtools::install_github("yusuzech/timeR")
```

## Basic Usage

```r
library(timeR)
# Create a timer object,precision default to s(second)
my_timer <- createTimer()

# start timing for an event
my_timer$start("event one")

#start timing for another event
my_timer$start("event two")

# stop timing for the events
my_timer$stop("event one")
my_timer$stop("event two", comment = "my comment") # comment is optional

# retrieve the table for all recordings
getTimer(my_timer)

# or create a timer object and setting verbose to false and use other precision
# s(second), ms(millisecond), us(microsecond)
my_timer2 <- createTimer(verbose = F,precision = "ms")

# toggle on/off verbose
my_timer$toggleVerbose()

# warnings will still be shown when verbose is turned off
my_timer$stop("event one")

# get attributes of a selected event
my_timer$getEvent("event one")
my_timer$getStartTime("event two")
```
