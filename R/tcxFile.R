#' TCX File Class
#'
#' Class used to manage a typycal Garmin Connect file (.tcx). The class has 3 fields used to store the main
#' components stored in a tcx file:
#' - Activity: as recorded by a Garmin fitness device
#' - Lap: the higher level of activity breakdown. A lap is either based on a fixed distance (1 Mile, 1 KM) or
#'   defined by when the user is hitting the lap button on her device
#' - Trackpoint: log of all the points recorded by the device
#'
#' The 3 components are stored in 3 data frames which are fields of the class.
#'
#' read method:
#' The read method is used to read and parse a local .tcx file. The method expects the Garmin Connect identifier
#' of an activity or a file name as a parameter. The file should be stored in the extdata folder.
#'
#' saveToDb method:
#' Save the 3 activity data frames stored in the class fields to the package database.
#'
#' readFromDb method:
#' Retrieve the activity data frames from the package database.
#' - Parameter activityId: controls which activity to retrieve (missing is all).
#' - Parameter dataFrames: controls which data frame to retrieve (missing or 'all' would retrieve all of them).
#' The data frames are stored in the class fields
#' 
#' @import XML
#' @import RSQLite
#' 
#' @keywords fitness, data
#' @examples \dontrun{
#' f <- tcxFile$new()
#' f$read(439915418)
#' f$saveToDb()
#' f$readFromDb()
#' }
tcxFile <- setRefClass(Class = "tcxFile",
                       fields = list(activity = "data.frame",
                                     lap = "data.frame",
                                     trackpoint = "data.frame"))

tcxFile$methods(
    initialize = function(...) {
        activity <<- data.frame(activity_id = character(),
                                sport = character(),
                                timestamp = as.POSIXct(character(), format = "%Y-%m-%dT%H:%M:%S."),
                                stringsAsFactors = FALSE)
        
        lap <<- data.frame(activity_id = character(),
                           lap_id = integer(),
                           timestamp = as.POSIXct(character(), format = "%Y-%m-%dT%H:%M:%S."),
                           time = numeric(),
                           distance = numeric(),
                           avg_speed = numeric(),
                           max_speed = numeric(),
                           calories = integer(),
                           avg_hr = integer(),
                           max_hr = integer(),
                           intensity = character(),
                           trigger = character(),
                           max_cadence = integer(),
                           avg_cadence = integer(),
                           steps = integer(),
                           stringsAsFactors = FALSE)
        
        trackpoint <<- data.frame(activity_id = character(),
                                  lap_id = integer(),
                                  tp_id = integer(),
                                  timestamp  = as.POSIXct(character(), format = "%Y-%m-%dT%H:%M:%S."),
                                  latitude = numeric(),
                                  longitude = numeric(),
                                  altitude = numeric(),
                                  distance = numeric(),
                                  speed = numeric(),
                                  cadence = integer(),
                                  stringsAsFactors = FALSE)
        callSuper(...)
    })

tcxFile$methods(
    read = function(activityId = "", fileName = "") {
        "The read method is used to read and parse a local .tcx file. The method expects the Garmin Connect identifier of an activity or a file name as a parameter. The file should be stored in the extdata folder."
        
        if (activityId != "") {
            fileName <- system.file(package = .global.constants()$packageName, "extdata", paste0("activity_", activityId, ".tcx"))
        } else if (fileName != "") {
            fileName <- system.file(package = .global.constants()$packageName, "extdata", fileName)
        }

        if (fileName == "")
            stop("File not found.")

        doc <- xmlTreeParse(file = fileName, useInternalNodes = TRUE)

        testMissing <- function(v) {
            if (length(v) == 0) {
                NA
            } else {
                v
            }
        }

        # Retrieve activitie details
        activity[1, "activity_id"] <<- activityId 
        activity[1, "sport"] <<- unlist(getNodeSet(doc = doc,
                                                   path = "//@Sport",
                                                   namespaces="ns"))
        activity[1, "timestamp"] <<- as.POSIXct(unlist(getNodeSet(doc = doc,
                                                                  path = "//ns:Id",
                                                                  namespaces="ns",
                                                                  fun=xmlValue)), format = "%Y-%m-%dT%H:%M:%S.")

        #activity[1, ] <<- data.frame(activityId, actSport, actTimestamp, stringsAsFactors = FALSE)

        lapCount <- getNodeSet(doc = doc,
                               path = "count(//ns:Activity[1]/ns:Lap)",
                               namespaces = "ns",
                               fun = xmlValue)
                                          
        for (l in 1:lapCount) {
            # Retrieve laps
            lap[l, "activity_id"] <<- activityId
            lap[l, "lap_id"] <<- l
            lap[l, "timestamp"] <<- as.POSIXct(testMissing(unlist(getNodeSet(doc = doc,
                                                                 path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/@StartTime"),
                                                                 namespaces = "ns"))), format = "%Y-%m-%dT%H:%M:%S.")
            lap[l, "time"] <<- as.numeric(testMissing(unlist(getNodeSet(doc = doc,
                                                            path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:TotalTimeSeconds"),
                                                            namespaces="ns",
                                                            fun=xmlValue))))
            lap[l, "distance"] <<- as.numeric(testMissing(unlist(getNodeSet(doc = doc,
                                                                path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:DistanceMeters"),
                                                                namespaces="ns",
                                                                fun=xmlValue))))
            lap[l, "max_speed"] <<- as.numeric(testMissing(unlist(getNodeSet(doc = doc,
                                                                 path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:MaximumSpeed"),
                                                                 namespaces="ns",
                                                                 fun=xmlValue))))
            lap[l, "calories"] <<- as.integer(testMissing(unlist(getNodeSet(doc = doc,
                                                                path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Calories"),
                                                                namespaces="ns",
                                                                fun=xmlValue))))
            lap[l, "avg_hr"] <<- as.numeric(testMissing(unlist(getNodeSet(doc = doc,
                                                              path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:AverageHeartRateBpm"),
                                                              namespaces="ns",
                                                              fun=xmlValue))))
            
            lap[l, "max_hr"] <<- as.numeric(testMissing(unlist(getNodeSet(doc = doc,
                                                              path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:MaximumHeartRateBpm"),
                                                              namespaces="ns",
                                                              fun=xmlValue))))
            lap[l, "intensity"] <<- as.character(testMissing(unlist(getNodeSet(doc = doc,
                                                                path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Intensity"),
                                                                namespaces="ns",
                                                                fun=xmlValue))))
            lap[l, "trigger"] <<- as.character(testMissing(unlist(getNodeSet(doc = doc,
                                                                path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:TriggerMethod"),
                                                                namespaces="ns",
                                                                fun=xmlValue))))
            lap[l, "max_cadence"] <<- as.integer(testMissing(unlist(getNodeSet(doc = doc,
                                                                    path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Extensions/ex:LX/ex:MaxRunCadence"),
                                                                    namespaces=c(
                                                                        ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2",
                                                                        ex = "http://www.garmin.com/xmlschemas/ActivityExtension/v2"),
                                                                    fun=xmlValue))))
            lap[l, "avg_cadence"] <<- as.integer(testMissing(unlist(getNodeSet(doc = doc,
                                                                   path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Extensions/ex:LX/ex:AvgRunCadence"),
                                                                   namespaces=c(
                                                                       ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2",
                                                                       ex = "http://www.garmin.com/xmlschemas/ActivityExtension/v2"),
                                                                   fun=xmlValue))))
            lap[l, "avg_speed"] <<- as.numeric(testMissing(unlist(getNodeSet(doc = doc,
                                                                 path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Extensions/ex:LX/ex:AvgSpeed"),
                                                                 namespaces=c(
                                                                     ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2",
                                                                     ex = "http://www.garmin.com/xmlschemas/ActivityExtension/v2"),
                                                                 fun=xmlValue))))
            lap[l, "steps"] <<- as.integer(testMissing(unlist(getNodeSet(doc = doc,
                                                             path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Extensions/ex:LX/ex:Steps"),
                                                             namespaces=c(
                                                                 ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2",
                                                                 ex = "http://www.garmin.com/xmlschemas/ActivityExtension/v2"),
                                                             fun=xmlValue))))
        }
        
        # Retrieve Trackpoints
        tpId <- 0

        # Loop on laps
        ignore <- lapply(lap$lap_id, function(l) {

            tpCount <- getNodeSet(doc = doc,
                                  path = paste0("count(//ns:Activity[1]/ns:Lap[", l, "]/ns:Track/ns:Trackpoint)"),
                                  namespaces = "ns",
                                  fun = xmlValue)

            # Loop on trackpoints
            for (tp in 1:tpCount) {
                assign(x = "tpId", value = tpId + 1, inherits = TRUE) 
                trackpoint[tpId, "activity_id"] <<- activityId
                trackpoint[tpId, "lap_id"] <<- l
                trackpoint[tpId, "tp_id"] <<- tpId
                trackpoint[tpId, "timestamp"] <<- as.POSIXct(testMissing(unlist(getNodeSet(doc = doc,
                                                                               path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Track/ns:Trackpoint[", tp, "]/ns:Time"),
                                                                               namespaces="ns",
                                                                               fun=xmlValue))), format = "%Y-%m-%dT%H:%M:%S.")
                trackpoint[tpId, "latitude"] <<- as.numeric(testMissing(unlist(getNodeSet(doc = doc,
                                                                              path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Track/ns:Trackpoint[", tp, "]/ns:Position/ns:LatitudeDegrees"),
                                                                              namespaces="ns",
                                                                              fun=xmlValue))))
                trackpoint[tpId, "longitude"] <<- as.numeric(testMissing(unlist(getNodeSet(doc = doc,
                                                                               path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Track/ns:Trackpoint[", tp, "]/ns:Position/ns:LongitudeDegrees"),
                                                                               namespaces="ns",
                                                                               fun=xmlValue))))
                trackpoint[tpId, "altitude"] <<- as.numeric(testMissing(unlist(getNodeSet(doc = doc,
                                                                              path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Track/ns:Trackpoint[", tp, "]/ns:AltitudeMeters"),
                                                                              namespaces="ns",
                                                                              fun=xmlValue))))
                trackpoint[tpId, "distance"] <<- as.numeric(testMissing(unlist(getNodeSet(doc = doc,
                                                                             path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Track/ns:Trackpoint[", tp, "]/ns:DistanceMeters"),
                                                                             namespaces="ns",
                                                                             fun=xmlValue))))
                trackpoint[tpId, "speed"] <<- as.numeric(testMissing(unlist(getNodeSet(doc = doc,
                                                                           path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Track/ns:Trackpoint[", tp, "]/ns:Extensions/ex:TPX/ex:Speed"),
                                                                           namespaces=c(
                                                                               ns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2",
                                                        ex="http://www.garmin.com/xmlschemas/ActivityExtension/v2"),
                                                                           fun=xmlValue))))
                trackpoint[tpId, "cadence"] <<- as.integer(testMissing(unlist(getNodeSet(doc = doc,
                                                                             path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Track/ns:Trackpoint[", tp, "]/ns:Extensions/ex:TPX/ex:RunCadence"),
                                                                             namespaces=c(
                                                                                 ns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2",
                                                                                 ex="http://www.garmin.com/xmlschemas/ActivityExtension/v2"),
                                                                             fun=xmlValue))))
            }
            NULL
        })
    })

tcxFile$methods(
    saveToDb = function() {
        "Save the 3 activity data frames (see fields) to the package database."
        
        db <- .database.constants()$db
        con <- dbConnect(SQLite(), dbname = db)

        activity_ <- transform(activity, timestamp = format(timestamp, format = "%Y-%m-%dT%H:%M:%S.", usetz = TRUE))
        lap_ <- transform(lap, timestamp = format(timestamp, format = "%Y-%m-%dT%H:%M:%S.", usetz = TRUE))
        trackpoint_ <- transform(trackpoint, timestamp = format(timestamp, format = "%Y-%m-%dT%H:%M:%S.", usetz = TRUE))
        
        listOfTables <- c("activity", "lap", "trackpoint")

        ignore <- lapply(listOfTables, function(t) {
            t_ <- paste0(t, "_")
            status <- dbWriteTable(conn = con, name = t, value = get(t_), row.names = FALSE, append = TRUE)
            if (status == TRUE) {
                cat(t, "data written to database.", "\n")
            } else {
                stop("Failed to write ", t, " data to database!")
            }
        })
    })

tcxFile$methods(
    readFromDb = function(activityId = "", dataFrames = "all") {
        "Retrieve the activity data frames from the package database. Parameter activityId controls which activity to retrieve (missing is all). Parameter dataFrames controls which data frame to retrieve (missing or 'all' would retrieve all of them). The data frames are stored in the class fields."
        
        dataFrames <- match.arg(arg=dataFrames, choices=c("all", "activity", "lap", "trackpoint"), several.ok=TRUE)
        if ("all" %in% dataFrames)
            dataFrames <- c("activity", "lap", "trackpoint")
        
        if (activityId == "")
            activityId <- "activity_id"
        else
            activityId <- paste0("'", activityId, "'")
    
        db <- .database.constants()$db
        con <- dbConnect(SQLite(), dbname = db)

        ignore <- lapply(dataFrames, function(t) {
            sql <- paste0("select * from ", t, " where activity_id = ", activityId)
            res <- dbGetQuery(con, sql)
            res <- transform(res, timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%S."))
            if (nrow(res) == 0) {
                cat("No", t, "data for activity", activityId, "\n")
            } else {
                assign(x = t, value = res, inherits = TRUE)
                cat("Retrieved", t, "data", "\n")
            }
        })
    })
