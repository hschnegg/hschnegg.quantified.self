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
                                              lap = "integer",
                                              lapId = "integer",
                                              trackpointId = "integer",
                                              vec = "character"
                                    ))

garminActivities$methods(
    initialize = function(...) {
        username <<- "stats290_test"
        password <<- "stats290_test"
        act <<- 0L
        lap <<- 0L
        lapId <<- 0L
        trackpointId <<- 0L
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

        doc <- xmlTreeParse(file = fileName, useInternalNodes = TRUE)

        # iterate over activities
        xpathApply(doc = doc, path="//ns:Activity", namespaces="ns", function(act) {
            actSport <- unlist(getNodeSet(doc = act,
                                          path = "//@Sport",
                                          namespaces="ns"))
            actTimestamp <- unlist(getNodeSet(doc = act,
                                              path = "//ns:Id",
                                              namespaces="ns",
                                              fun=xmlValue))
            # iterate over laps
            xpathApply(doc = act, path="//ns:Lap", namespaces="ns", function(lp) {
                #lapId <<- lapId + 1L
                lapTimestamp <- unlist(getNodeSet(doc = lp,
                                                  path = "//@StartTime",
                                                  namespaces="ns"))
                lapTime <- unlist(getNodeSet(doc = lp,
                                             path = "//ns:TotalTimeSeconds",
                                             namespaces="ns",
                                             fun=xmlValue))
                lapDistance <- unlist(getNodeSet(doc = lp,
                                                 path = "//ns:DistanceMeters",
                                                 namespaces="ns",
                                                 fun=xmlValue))
                lapCalories <- unlist(getNodeSet(doc = lp,
                                                 path = "//ns:Calories",
                                                 namespaces="ns",
                                                 fun=xmlValue))
                lapAvgHr <- unlist(getNodeSet(doc = lp,
                                              path = "//ns:AverageHeartRateBpm",
                                              namespaces="ns",
                                              fun=xmlValue))
                lapMaxHr <- unlist(getNodeSet(doc = lp,
                                              path = "//ns:MaximumHeartRateBpm",
                                              namespaces="ns",
                                              fun=xmlValue))
                lapIntensity <- unlist(getNodeSet(doc = lp,
                                                  path = "//ns:Intensity",
                                                  namespaces="ns",
                                                  fun=xmlValue))
                lapTrigger <- unlist(getNodeSet(doc = lp,
                                                path = "//ns:TriggerMethod",
                                                namespaces="ns",
                                                fun=xmlValue))
                # iterate over tracks
                xpathApply(doc = lp, path="//ns:Track", namespaces="ns", function(t) {
                    # iterate over trackpoints
                    xpathApply(doc = t, path="//ns:Trackpoint", namespaces="ns", function(tp) {
                        tpTimestamp <- unlist(getNodeSet(doc = tp,
                                                         path = "//ns:Time",
                                                         namespaces="ns",
                                                         fun=xmlValue))
                        tpLat <- unlist(getNodeSet(doc = tp,
                                                   path = "//ns:Position/ns:LatitudeDegrees",
                                                   namespaces="ns",
                                                   fun=xmlValue))
                        tpLong <- unlist(getNodeSet(doc = tp,
                                                    path = "//ns:Position/ns:LongitudeDegrees",
                                                    namespaces="ns",
                                                    fun=xmlValue))
                        tpAlt <- unlist(getNodeSet(doc = tp,
                                                   path = "//ns:AltitudeMeters",
                                                   namespaces="ns",
                                                   fun=xmlValue))
                        tpDist <- unlist(getNodeSet(doc = tp,
                                                    path = "//ns:DistanceMeters",
                                                    namespaces="ns",
                                                    fun=xmlValue))
                        tpSpeed <- unlist(getNodeSet(doc = tp,
                                                     path = "//ns:Extensions/ex:TPX/ex:Speed",
                                                     namespaces=c(
                                                         ns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2",
                                                         ex="http://www.garmin.com/xmlschemas/ActivityExtension/v2"),
                                                     fun=xmlValue))
                        tpCadence <- unlist(getNodeSet(doc = tp,
                                                       path = "//ns:Extensions/ex:TPX/ex:RunCadence",
                                                       namespaces=c(
                                                           ns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2",
                                                           ex="http://www.garmin.com/xmlschemas/ActivityExtension/v2"),
                                                       fun=xmlValue))
                        #df <<- rbind(df, c(lapId, tpTimestamp, tpLat, tpLong, tpAlt, tpDist, tpSpeed, tpCadence))
                        tp <- c(tpTimestamp, tpLat, tpLong, tpAlt, tpDist, tpSpeed, tpCadence)
                        assign(x="vec", value=tp, inherits=TRUE)
                        #.self$trackpointId <<- 5L
                        #return(tp)
                    })
                })
            })
        })
        return(0)
    })
            
