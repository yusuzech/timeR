Release v1.0.1

* Fixed bug where elapsed time will be automatically converted to seconds, minutes or hours. Now, it only uses seconds.

Release v1.1.0

* Fixed a bug where timeElapsed could be in other units instead of in seconds.
* Added methods that will return attributes from selected event
* Refactored codes, removed redundant portion.
* switch to use lubridate for time calculation
* use string to represent datetime in dataframe which prevents errors from POSIXlt

Release v1.2.0

* Now Added precision when creating a timeR object, valid values are s(second), ms(millisecond), us(microsecond)

