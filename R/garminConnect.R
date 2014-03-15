#' Garmin Activity Connect Class
#'
#' This class offers the framework to deal with accessing Garmin Connect
#' (connect.garmin.com)
#'
#' username field:
#' Your Garmin Connect username
#'
#' password field:
#' Your Garmin Connect password
#'
#' login method:
#' Open a connection to the Garmin Connect web site using the username and password fields to log you in. Returns
#' the CURL handle that can be used to issue queries against the Garmin Connect REST API.
#'
#' retrieveActivityList method:
#' Retrieve a list of the latest activities stored in Garmin Connect
#'
#' downloadTcx method:
#' Download the TCX file for a given activity from Garmin Connect. The file is saved in /inst/extdata. The
#' method expects an Activity Id as a parameter.
#' 
#'
#' @export
#' @import RCurl
#' @import rjson
#' 
#' @keywords fitness, data
#' @examples {
#' gc <- garminConnect$new()
#' gc$login()
#' gc$retrieveActivityList()
#' gc$download(454818889)
#' }
garminConnect <- setRefClass(Class = "garminConnect",
                                fields = list(username = "character",
                                              password = "character",
                                              act = "integer",
                                              lap = "integer",
                                              lapId = "integer",
                                              trackpointId = "integer",
                                              vec = "character"
                                    ))

garminConnect$methods(
    initialize = function(...) {
        username <<- "stats290_test"
        password <<- "stats290_test"
        act <<- 0L
        lap <<- 0L
        lapId <<- 0L
        trackpointId <<- 0L
        callSuper(...)
    })

garminConnect$methods(
    login = function() {
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
 
garminConnect$methods(
    retrieveActivityList = function() {
        "Retrieve a list of the most recent activities posted to Garmin Connect"
        
        # Retrieve latest activities from GC REST API
        curlHandle <- login()
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

garminConnect$methods(
    downloadTcx = function(activityId) {
        "Download the TCX file for a given activity from Garmin Connect. The file is saved in /inst/extdata"
        
        curlHandle <- login()

        fileName <- paste0(system.file(package = .global.constants()$packageName, "inst", "extdata"), paste0("/activity_", activityId, ".tcx"))
        urlTcxFile <- sub(pattern = "XXX", replacement = activityId, x = .garmin.constants()$urlGCtcxFile)

        activity <- getURLContent(url = urlTcxFile, curl = curlHandle)
        write(x = activity, file = fileName)
        cat("File", fileName, "available", "\n")
    })
