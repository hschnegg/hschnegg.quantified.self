#' Garmin Activity Class
#'
#' This class offers the framework to deal with Garmin Connect data
#'
#' @export
#' @import RCurl
#' @import rjson
#' @import XML
#' 
#' @keywords XXX
#' @examples {
#' act <- garminActivities$new()
#' act$connectGC()
#' }
garminActivities <- setRefClass(Class = "garminActivities",
                                fields = list(username = "character",
                                              password = "character",
                                              act = "integer",
                                              lap = "integer"))

garminActivities$methods(
    initialize = function(...) {
        username <<- "stats290_test"
        password <<- "stats290_test"
        act <<- 0L
        lap <<- 0L
        callSuper(...)
    })

garminActivities$methods(
    connectGC = function() {
        "Login to Garmin Connect (website)"

        # Take CURL handle
        curlHandle <- getCurlHandle()

        curlSetOpt(curl = curlHandle,
                   cookiefile = system.file(package = .global.constants()$packageName, "inst", "extdate", "gc_cookies.txt"),
                   followlocation = FALSE,
                   verbose = FALSE,
                   header = TRUE)
        
        # Initial query to retrieve session and "flow execution key"
        answer <- getURL(url = .garmin.constants()$urlGCsignin, curl = curlHandle)

        # Extract "flow execution key" from answer
        flowExecKey <- gsub("^.*<!-- flowExecutionKey: \\[(.+)\\] -->.*$", replacement="\\1", x = answer[1])

        # Error
        if (flowExecKey == answer[1])
            stop("Connection to Garmin Connect failed - flowExecKey")

        # Submit login form
        answer <- postForm(uri = .garmin.constants()$urlGCsignin,
                             .params = list(
                                 "_eventId" = "submit",
                                 "displayNameRequired" = "false",
                                 "embed" = "true",
                                 "lt" = flowExecKey,
                                 "password" = password,
                                 "username" = username),
                             curl = curlHandle,
                             style = "POST",
                             .checkParams = TRUE
                             )

        # Retrieve post auth URL
        url <- gsub("^.*response_url += +'(.*?)';.*$", replacement="\\1", x = answer[1])

        # Error
        if (url == answer[1])
            stop("Connection to Garmin Connect failed - post auth url")
        
        # Finish auth process
        answer <- getURL(url = url, curl = curlHandle, .opts = list(followlocation = TRUE))

        # Test login
        testLogin <- getURLContent(url = .garmin.constants()$urlGCvalidate, curl = curlHandle)

        # Status report
        if (identical(grep(pattern = username, x = testLogin), character(0))) {
            stop("Connection to Garmin Connect failed - validation")
        } else {
            cat("User", username, "successfully connected to Garmin Connect", "\n")
            return(curlHandle)
        }
    })
 
garminActivities$methods(
    retrieveGCactivityList = function() {
        "Retrieve a list of the most recent activities posted to Garmin Connect"
        
        # Retrieve latest activities from GC REST API
        curlHandle <- connectGC()
        jsonAct <- getURLContent(url = .garmin.constants()$urlGCactivityList, curl = curlHandle)
        listAct <- fromJSON(jsonAct)

        # Format activities into a data frame
        activityList <- sapply(X=listAct$results$activities, FUN=function(l) {
            c(l$activity$activityId,
              l$activity$beginTimestamp$value,
              l$activity$activityName$value)
        })
        activityList <- as.data.frame(t(activityList))
        names(activityList) <- c("id", "date", "name")
        
        return(activityList)
    })

garminActivities$methods(
    downloadTCX = function(activityId) {
        "Download the TCX file for a given activity from Garmin Connect. The file is saved in /inst/extdata"
        
        curlHandle <- connectGC()

        fileName <- paste0(system.file(package = .global.constants()$packageName, "inst", "extdata"), paste0("/activity_", activityId, ".tcx"))
        urlTcxFile <- sub(pattern = "XXX", replacement = activityId, x = .garmin.constants()$urlGCtcxFile)

        activity <- getURLContent(url = urlTcxFile, curl = curlHandle)
        write(x = activity, file = fileName)
        cat("File", fileName, "available", "\n")
    })

garminActivities$methods(
    readTcx = function(activityId) {
        fileName <- paste0(system.file(package = .global.constants()$packageName, "inst", "extdata"), paste0("/activity_", activityId, ".tcx"))

        return(0)
    })

garminActivities$methods(
    saxHandler = function() {
        Activity <- function(node) {
            #activityId <- xmlValue(node[["Id"]])
            act <<- act + 1L
        }
        #c(Activity=xmlParserContextFunction(Activity))
        Lap <- function(node) {
            #LapId <- xmlValue(node[["Calories"]])
            lap <<- lap + 1L
        }
        list(Activity = Activity, Lap = Lap)
    })
            
