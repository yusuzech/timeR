#' Create a timer object
#'
#' @param verbose A parameter to control whether to print messages while using
#' methods. Default to \code{TRUE}.
#' @param precision Precision for time, default to s, valid values are: s,ms and us
#' @return a timer object.
#' @examples
#' timer1 <- createTimer() # print is enabled
#' timer1 <- createTimer(FALSE) # print is disabled
#' timer1$start("event1") # start timing for event 1
#' timer1$stop("event1", comment = "event 1 stopped") # stop timing for event 1(comment is optional)
#' getTimer(timer1) # get all records in a data frame
#' @export
createTimer <- function(verbose = T,precision = "s"){
    return(timeR$new(verbose = verbose,precision = precision))
}


#' Get the data frame in timer object
#'
#' timer object has a built-in data frame that contains all timings. run this
#' function to extract the data frame.
#'
#' @param object The name for timer object.
#' @return A data frame containing all records of a timer object.
#' @examples
#' timer1 <- createTimer()
#' timer1$start("event1")
#' Sys.sleep(1)
#' timer1$stop("event1")
#' getTimer(timer1)
#' @export
getTimer <- function(object){
    stopifnot(any(class(object) == "timeR"))
    return(object$getTimer())
}
