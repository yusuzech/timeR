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
#' timer1 <- createTimer()
#' timer1$start("event1")
#' # put some codes in between
#' timer1$stop("event1")
#'
#' timer1$start("event2")
#' # put some codes in between
#' timer1$stop("event2",comment = "event 2 completed")
#'
#' table1 <- getTimer(timer1)
#' timer1$toggleVerbose() # set verbose to FALSE as default is TRUE
#'
#' table1 # print all records in a tibble(data frame)
#' @importFrom R6 R6Class
#' @export
timer <- R6::R6Class(
    classname = "timer",
    public = list(
        #values
        time = .POSIXct(character()),
        event = character(),
        eventTable = data.frame(),
        verbose = logical(),
        #initialize timer
        initialize = function(time=.POSIXct(character(1)),
                              event=character(1),
                              verbose=TRUE,
                              eventTable=data.frame(
                                  event = character(),
                                  start = .POSIXct(character()),
                                  end = .POSIXct(character()),
                                  timeElapsed = numeric(),
                                  stringsAsFactors = FALSE,
                                  comment = character())){
            #check if input values are correct
            stopifnot(any(is(time,"POSIXt"),
                          is(time,"POSIXct")),
                      length(time) == 1)
            stopifnot(is.character(event),
                      length(event) == 1)
            stopifnot(is.data.frame(eventTable))
            stopifnot(!is.na(verbose),
                      is.logical(verbose))
            #initialize values
            self$time = time
            self$event = event
            self$verbose = verbose
            self$eventTable = eventTable},
        #start a timer for event
        start = function(eventName){
            theTable <- self$eventTable
            verbose <- self$verbose
            current_time <- Sys.time()
            #create that record/row
            newRow <- data.frame(event = eventName,
                                 start = current_time,
                                 end = .POSIXct(character(1)),
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
            self$time <- current_time
            self$event <- eventName
            invisible(self)
        },
        #stop timer
        stop = function(eventName,comment=NA_character_){
            theTable <- self$eventTable
            verbose <- self$verbose
            current_time <- Sys.time()
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
                timeElapsed <- as.numeric(current_time - startTime)
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
            self$time <- current_time
            self$event <- eventName
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
        print = function(...){
            current_table <- self$eventTable
            out_msg1 <- paste0("Current event: '",self$event,
                               "' at ",as.character(self$time),".\n",
                               "--------------------------------\n",
                               "Current eventTable has ",
                               nrow(current_table)," records:")
            out_msg2 <- capture.output(print(current_table))
            out_msg <- c(out_msg1,out_msg2)
            writeLines(out_msg)
            invisible(self)
        }
    ),
    #function used to determine whether message is printed
    private = list(
        slprint = function(msg,flag = self$verbose){
            if(flag) writeLines(msg)
        }
    ),
    active = list(now = Sys.time)
)
