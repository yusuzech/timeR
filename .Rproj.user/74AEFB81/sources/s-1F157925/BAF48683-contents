# timeR

[![Travis build status](https://travis-ci.org/yusuzech/timeR.svg?branch=master)](https://travis-ci.org/yusuzech/timeR)
## A simple package for timing your code.

`timeR` package creates a R6 class object, which allows you to create a timer object
to easily time your codes. Meanwhile, all records are saved to a data frame, so it's easy to retrieve all the records for later use.

Timing codes is not difficult but can be very tedious. With `timeR`, you can save your energy on timing and put more effort on 
your analysis. You can use `timeR` to time training time for machine learning models, record speed for requests when running web-scraping scripts or other situations that you need to keep records of time.

## How to install

```r
# current this package is not on CRAN, please install from github
devtools::install_github("yusuzech/timeR")
```

## Basic Usage

```r
library(timeR)
# Create a timer object
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

# or create a timer object and setting verbose to false
my_timer2 <- createTimer(verbose = F)

# toggle on/off verbose
my_timer$toggleVerbose()

# warnings will still be shown when verbose is turned off
my_timer$stop("event one")
```
