[Release v1.0.1](https://github.com/yusuzech/timeR/releases/tag/v1.0.1)

* Fixed bug where elapsed time will be automatically converted to seconds, minutes or hours. Now, it only uses seconds.

[Release v1.1.0]
* Fixed a bus where timeElapsed could be in other units instead of in seconds.
* Added methods that will return attributes from selected event
* Refactored codes, removed redundent portion.
* switch to use lubridate for time calculation
* use string to represent datetime in dataframe which prevents errors from POSIXlt
