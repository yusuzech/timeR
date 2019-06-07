timeR <- R6::R6Class(
    classname = "timeR",
    public = list(
        #values
        eventTable = data.frame(
            event = character(),
            start = .POSIXct(character()),
            end = .POSIXct(character()),
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
            invisible(self)
        },
        #stop timeR
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
                timeElapsed <- as.numeric(difftime(current_time,startTime,units = "secs"))
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
            result <- self$eventTable[rowIndex,"start"]
            return(result)
        },
        getStopTime = function(eventName){
            rowIndex <- which(eventName == self$eventTable$event)
            result <- self$eventTable[rowIndex,"stop"]
            return(result)
        },
        getTimeElapsed = function(eventName){
            rowIndex <- which(eventName == self$eventTable$event)
            result <- self$eventTable[rowIndex,"timeElapsed"]
            return(result)
        },
        getComment = function(eventName){
            rowIndex <- which(eventName == self$eventTable$event)
            result <- self$eventTable[rowIndex,"comment"]
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
    active = list(now = Sys.time)
)
