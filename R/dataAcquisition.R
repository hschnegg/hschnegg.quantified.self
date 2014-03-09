garminActivities <- setRefClass(Class = "garminActivities",
                                fields = list(username = "character",
                                              password = "character"))

garminActivities$methods(
    initialize = function(...) {
        username <<- "stats290_test"
        password <<- "stats290_test"
        callSuper(...)
    })

#' Login to Garmin Connect
#'
#' This method is called to login to Garmin Connect so that calls to the REST API are possible
#'
#' @param username Garmin Connect user name
#' @param password Garmin Connect password
#'  
#' @keywords XXX
#' @export
#' @examples {
#' act <- garminActivities$new()
#' act$connectGC()
#' }
garminActivities$methods(
    connectGC = function() {

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

#' Retrieve latest activities from Garmin Connect as a data.frame
#'  
#' @keywords XXX
#' @export
#' @examples {
#' act <- garminActivities$new()
#' act$retrieveGCactivityList()
#' }
garminActivities$methods(
    retrieveGCactivityList = function() {
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
    
