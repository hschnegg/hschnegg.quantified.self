#' TCX File  Class
#'
#' 
#'
#' @export
#' @import XML
#' 
#' @keywords XXX
#' @examples {
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
                                  cadence = numeric(),
                                  stringsAsFactors = FALSE)
        callSuper(...)
    })

tcxFile$methods(
    readTcx = function(activityId) {
        
        fileName <- paste0(system.file(package = .global.constants()$packageName, "inst", "extdata"), paste0("/activity_", activityId, ".tcx"))

        doc <- xmlTreeParse(file = fileName, useInternalNodes = TRUE)

        # Retrieve activitie details
        actSport <- unlist(getNodeSet(doc = doc,
                                      path = "//@Sport",
                                      namespaces="ns"))
        actTimestamp <- unlist(getNodeSet(doc = doc,
                                          path = "//ns:Id",
                                          namespaces="ns",
                                          fun=xmlValue))

        activity[1, ] <<- data.frame(activityId, actSport, actTimestamp)

        # Retrieve laps
        lapTimestamp <- unlist(getNodeSet(doc = doc,
                                          path = "//ns:Activity[1]/ns:Lap/@StartTime",
                                          namespaces = "ns"))
        lapTime <- as.numeric(unlist(getNodeSet(doc = doc,
                                                path = "//ns:Activity[1]/ns:Lap/ns:TotalTimeSeconds",
                                                namespaces="ns",
                                                fun=xmlValue)))
        lapDistance <- as.numeric(unlist(getNodeSet(doc = doc,
                                                    path = "//ns:Activity[1]/ns:Lap/ns:DistanceMeters",
                                                    namespaces="ns",
                                                    fun=xmlValue)))
        lapMaxSpeed <- as.numeric(unlist(getNodeSet(doc = doc,
                                                    path = "//ns:Activity[1]/ns:Lap/ns:MaximumSpeed",
                                                    namespaces="ns",
                                                    fun=xmlValue)))
        lapCalories <- as.integer(unlist(getNodeSet(doc = doc,
                                                    path = "//ns:Activity[1]/ns:Lap/ns:Calories",
                                                    namespaces="ns",
                                                    fun=xmlValue)))
        lapAvgHr <- as.numeric(unlist(getNodeSet(doc = doc,
                                                 path = "//ns:Activity[1]/ns:Lap/ns:AverageHeartRateBpm",
                                                 namespaces="ns",
                                                 fun=xmlValue)))
        lapMaxHr <- as.numeric(unlist(getNodeSet(doc = doc,
                                                 path = "//ns:Activity[1]/ns:Lap/ns:MaximumHeartRateBpm",
                                                 namespaces="ns",
                                                 fun=xmlValue)))
        lapIntensity <- as.character(unlist(getNodeSet(doc = doc,
                                                       path = "//ns:Activity[1]/ns:Lap/ns:Intensity",
                                                       namespaces="ns",
                                                       fun=xmlValue)))
        lapTrigger <- as.character(unlist(getNodeSet(doc = doc,
                                                     path = "//ns:Activity[1]/ns:Lap/ns:TriggerMethod",
                                                     namespaces="ns",
                                                     fun=xmlValue)))
        lapMaxCadence <- as.integer(unlist(getNodeSet(doc = doc,
                                                      path = "//ns:Activity[1]/ns:Lap/ns:Extensions/ex:LX/ex:MaxRunCadence",
                                                      namespaces=c(
                                                          ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2",
                                                          ex = "http://www.garmin.com/xmlschemas/ActivityExtension/v2"),
                                                      fun=xmlValue)))
        lapAvgCadence <- as.integer(unlist(getNodeSet(doc = doc,
                                                      path = "//ns:Activity[1]/ns:Lap/ns:Extensions/ex:LX/ex:AvgRunCadence",
                                                      namespaces=c(
                                                          ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2",
                                                          ex = "http://www.garmin.com/xmlschemas/ActivityExtension/v2"),
                                                      fun=xmlValue)))
        lapAvgSpeed <- as.numeric(unlist(getNodeSet(doc = doc,
                                                    path = "//ns:Activity[1]/ns:Lap/ns:Extensions/ex:LX/ex:AvgSpeed",
                                                    namespaces=c(
                                                        ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2",
                                                        ex = "http://www.garmin.com/xmlschemas/ActivityExtension/v2"),
                                                    fun=xmlValue)))
        lapSteps <- as.integer(unlist(getNodeSet(doc = doc,
                                                 path = "//ns:Activity[1]/ns:Lap/ns:Extensions/ex:LX/ex:Steps",
                                                 namespaces=c(
                                                     ns = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2",
                                                     ex = "http://www.garmin.com/xmlschemas/ActivityExtension/v2"),
                                                 fun=xmlValue)))

        lapCount <- length(lapTimestamp)
        lapId <- seq(from = 1, to = lapCount) 

        lap <<- data.frame(activityId, lapId, lapTimestamp, lapTime, lapDistance, lapAvgSpeed, lapMaxSpeed, lapCalories, lapAvgHr, lapMaxHr, lapIntensity, lapTrigger, lapMaxCadence, lapAvgCadence, lapSteps)
        
        # Retrieve Trackpoints
        ignore <- lapply(lapId, function(l) {
            tpTimestamp <- unlist(getNodeSet(doc = doc,
                                             path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Track/ns:Trackpoint/ns:Time"),
                                             namespaces="ns",
                                             fun=xmlValue))
            tpLat <- as.numeric(unlist(getNodeSet(doc = doc,
                                                  path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Track/ns:Trackpoint/ns:Position/ns:LatitudeDegrees"),
                                                  namespaces="ns",
                                                  fun=xmlValue)))
            tpLong <- as.numeric(unlist(getNodeSet(doc = doc,
                                                   path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Track/ns:Trackpoint/ns:Position/ns:LongitudeDegrees"),
                                                   namespaces="ns",
                                                   fun=xmlValue)))
            tpAlt <- as.numeric(unlist(getNodeSet(doc = doc,
                                                  path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Track/ns:Trackpoint/ns:AltitudeMeters"),
                                                  namespaces="ns",
                                                  fun=xmlValue)))
            tpDist <- as.numeric(unlist(getNodeSet(doc = doc,
                                                   path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Track/ns:Trackpoint/ns:DistanceMeters"),
                                                   namespaces="ns",
                                                   fun=xmlValue)))
            tpSpeed <- as.numeric(unlist(getNodeSet(doc = doc,
                                                    path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Track/ns:Trackpoint/ns:Extensions/ex:TPX/ex:Speed"),
                                                    namespaces=c(
                                                        ns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2",
                                                        ex="http://www.garmin.com/xmlschemas/ActivityExtension/v2"),
                                                    fun=xmlValue)))
            tpCadence <- as.integer(unlist(getNodeSet(doc = doc,
                                                      path = paste0("//ns:Activity[1]/ns:Lap[", l, "]/ns:Track/ns:Trackpoint/ns:Extensions/ex:TPX/ex:RunCadence"),
                                                      namespaces=c(
                                                          ns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2",
                                                          ex="http://www.garmin.com/xmlschemas/ActivityExtension/v2"),
                                                      fun=xmlValue)))

            lastId <- 0
            if (nrow(trackpoint) > 0)
                lastId <- max(trackpoint$tp_id)
            tpCount <- length(tpTimestamp)           
            tpId <- seq(from = (lastId + 1), to = (lastId + tpCount))
            
            trackpoint[tpId, ] <<- data.frame(rep(activityId, tpCount), rep(l, tpCount), tpId, tpTimestamp, tpLat, tpLong, tpAlt, tpDist, tpSpeed, tpCadence)

            NULL
        })
    })
