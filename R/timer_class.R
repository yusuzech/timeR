#' A R6 Class to represent a timer.
#'
#' timer is a R6 Class that represent a timer.
#'
#' @docType class
#' @field time A POSIXct/POSIXlt value of your latest timing.
#' @field event A string of your latest timing.
#' @field eventTable A data frame that stores all timings.
#' @field verbose A printing setting that controls whether to print messages.
#' @section Public Methods:
#' \describe{
#'   \item{\code{initialize(time,event,verbose,eventTable)}
#'   }{Initialize a timer object. You can also use \code{createTimer()}
#'   function to initialize a timer object.}
#'   \item{\code{start(eventName)}
#'   }{Start timing for a event, \code{eventName} should be a string}
#'   \item{\code{stop(eventName)}
#'   }{Stop timing for a event.}
#'   \item{\code{getTimer()}
#'   }{Get a data frame that stores all recordings.You can also use
#'   \code{getTimer()} function to get the data frame.}
#'   \item{\code{removeEvent(eventName)}
#'   }{Remove an given row in the eventTable.}
#'   \item{\code{toggleVerbose()}
#'   }{Toggle between \code{TRUE} and \code{FALSE} for \code{verbose}}
#'   \item{\code{getStartTime()}
#'   }{Get start time for a selected event.}
#'   \item{\code{getStopTime()}
#'   }{Get stop time for a selected event.}
#'   \item{\code{getTimeElapsed()}
#'   }{Get time elapsed for a selected event.}
#'   \item{\code{getComment()}
#'   }{Get comment for a selected event.}
#'   \item{\code{getEventf()}
#'   }{Get entire row for a selected event.}
#'   \item{\code{print()}
#'   }{Custom print method for timer class. However, you don't need to use this
#'   function to generate custom printing.
#'   Custom printing is triggered by default.}
#'   }
#' @section Private Methods:
#' \describe{
#' \item{\code{slprint(msg,flag = self$verbose)}
#' }{A function that controls whether to print extra message.}
#' }
#' @examples
#' timer <- createTimer()
#' timer$start("event1")
#' # put some codes in between
#' timer$stop("event1")
#'
#' timer$start("event2")
#' # put some codes in between
#' timer$stop("event2",comment = "event 2 completed")
#'
#' table1 <- getTimer(timer)
#' timer$toggleVerbose() # set verbose to FALSE as default is TRUE
#'
#' table1 # print all records in a tibble(data frame)
#'
#' # get attributes for selected events
#' timer$getStartTime("event1")
#' timer$getStopTime("event1")
#' timer$getTimeElapsed("event1")
#' timer$getComment("event1")
#' timer$getEvent("event1")
#' @importFrom R6 R6Class
#' @export
timeR <- R6::R6Class(
    classname = "timeR",
    public = list(
        #values
        eventTable = data.frame(
            event = character(),
            start = character(),
            end = character(),
            timeElapsed = numeric(),
            stringsAsFactors = FALSE,
            comment = character()),
        verbose = logical(),
        #initialize timeR
        initialize = function(verbose=TRUE){
            stopifnot(is.logical(verbose),!is.na(verbose))
            self$verbose = verbose
        },
        #start a timeR for event
        start = function(eventName){
            if(!exists("eventName")){
                stop("Please create a name for the event.")
            }
            theTable <- self$eventTable
            verbose <- self$verbose
            current_time <- as.character(lubridate::now())
            #create that record/row
            newRow <- data.frame(event = eventName,
                                 start = current_time,
                                 end = character(1),
                                 timeElapsed = numeric(1),
                                 stringsAsFactors = FALSE,
                                 comment = NA_character_)
            #detect ifevent already exist
            if (any(theTable$event %in% eventName) ){
                out_msg <- paste0("Event: '",
                                  eventName,
                                  "' already exists.",
                                  " Overwriting previous 'start'.\n")
                private$slprint(out_msg)
                #replace the row in dataframe with new row
                boolTargetRow <- theTable$event == eventName
                self$eventTable[boolTargetRow, ] <- newRow
            } else {
                #append new row
                self$eventTable <- rbind(theTable,newRow)
            }
            invisible(self)
        },
        #stop timeR
        stop = function(eventName,comment=NA_character_){
            theTable <- self$eventTable
            verbose <- self$verbose
            current_time <- as.character(lubridate::now())
            #detect if event already exists
            if (any(theTable$event %in% eventName)){
                #detect if end time for event already exist
                end_exist <- !is.na(as.character(
                    theTable[theTable$event ==eventName,][["end"]]))
                if (end_exist){
                    out_msg <- paste0("Event: '",eventName,
                                      "' already has a record.",
                                      " Overwriting previous one.\n")
                    private$slprint(out_msg)
                }
                #modify the end anyway
                isEventRow <- theTable$event == eventName
                startTime <- self$eventTable[isEventRow, ][["start"]]
                timeElapsed <- difftime(
                    lubridate::ymd_hms(current_time,tz = Sys.timezone()),
                    lubridate::ymd_hms(startTime,tz = Sys.timezone()),
                    units = "secs"
                    )
                self$eventTable[isEventRow, ][["end"]] <- current_time
                self$eventTable[isEventRow, ][["timeElapsed"]] <- timeElapsed
                self$eventTable[isEventRow, ][["comment"]] <- comment
            } else {
                stop("Event: '",eventName,"'",
                     " doesn't exist. Record won't be created.\n")
            }
            out_msg <- paste0("For event: '",eventName,
                              "', ",round(timeElapsed,2),
                              " seconds elapsed.\n")
            private$slprint(out_msg)
            invisible(self)
        },
        getTimer = function(...){
            return(self$eventTable)
        },
        removeEvent = function(eventName){
            theTable <- self$eventTable
            boolRow <- !(theTable$event == eventName)
            if(all(boolRow)) {
                out_msg <- paste0("Event '",eventName,
                                  "' doesn't exist. No record is deleted.\n")
                private$slprint(out_msg)
            } else {
                self$eventTable <- theTable[boolRow,]
            }
            invisible(self)
        },
        toggleVerbose = function(...){
            self$verbose = !self$verbose
            out_msg <- paste0("Verbose set to: ",as.character(self$verbose),
                              ".\n")
            writeLines(out_msg)
            invisible(self)
        },
        getStartTime = function(eventName){
            rowIndex <- which(eventName == self$eventTable$event)
            if (length(rowIndex) == 0) {
                stop("event doesn't exist.")
            }
            result <- self$eventTable[rowIndex,"start"]
            return(result)
        },
        getStopTime = function(eventName){
            rowIndex <- which(eventName == self$eventTable$event)
            if (length(rowIndex) == 0) {
                stop("event doesn't exist.")
            }
            result <- self$eventTable[rowIndex,"stop"]
            return(result)
        },
        getTimeElapsed = function(eventName){
            rowIndex <- which(eventName == self$eventTable$event)
            if (length(rowIndex) == 0) {
                stop("event doesn't exist.")
            }
            result <- self$eventTable[rowIndex,"timeElapsed"]
            return(result)
        },
        getComment = function(eventName){
            rowIndex <- which(eventName == self$eventTable$event)
            if (length(rowIndex) == 0) {
                stop("event doesn't exist.")
            }
            result <- self$eventTable[rowIndex,"comment"]
            return(result)
        },
        getEvent = function(eventName){
            rowIndex <- which(eventName == self$eventTable$event)
            if (length(rowIndex) == 0) {
                stop("event doesn't exist.")
            }
            result <- self$eventTable[rowIndex,]
            return(result)
        },
        print = function(...){
            writeLines("Your Table is:")
            print(self$eventTable)
        }
    ),
    #function used to determine whether message is printed
    private = list(
        slprint = function(msg,flag = self$verbose){
            if(flag) writeLines(msg)
        }
    ),
    active = list(now = function(){
        as.POSIXct(Sys.time(), format = "%Y-%m-%d %H:%M")
    })
)
