library(XML)

doc <- xmlTreeParse(file="inst/extdata/activity_439915418.tcx", useInternalNodes = TRUE)

activityId <- 0L
lapId <- 0L
trackId <- 0L
trackpointId <- 0L

activityNodes <- getNodeSet(doc = doc,
                            path = "//ns:Activity",
                            namespaces = "ns")

# iterate over activities
sapply(activityNodes, function(act) {
    #activityId <<- activityId + 1L
    actSport <<- unlist(getNodeSet(doc = act,
                                   path = "//ns:Activity/@Sport",
                                   namespaces="ns"))
    actTimestamp <<- unlist(getNodeSet(doc = act,
                                  path = "//ns:Activity/ns:Id",
                                  namespaces="ns",
                                  fun=xmlValue))
    lapNodes <- getNodeSet(doc = act,
                           path = "//ns:Lap",
                           namespaces = "ns")
    # iterate over laps
    sapply(lapNodes, function(lp) {
        lapId <<- lapId + 1L
        lapTimestamp <<- unlist(getNodeSet(doc = lp,
                                           path = "//ns:Lap/@StartTime",
                                           namespaces="ns"))
        lapTime <<- unlist(getNodeSet(doc = lp,
                                      path = "//ns:Lap/ns:TotalTimeSeconds",
                                      namespaces="ns",
                                      fun=xmlValue))
        lapDistance <<- unlist(getNodeSet(doc = lp,
                                          path = "//ns:Lap/ns:DistanceMeters",
                                          namespaces="ns",
                                          fun=xmlValue))
        lapCalories <<- unlist(getNodeSet(doc = lp,
                                          path = "//ns:Lap/ns:Calories",
                                          namespaces="ns",
                                          fun=xmlValue))
        lapAvgHr <<- unlist(getNodeSet(doc = lp,
                                       path = "//ns:Lap/ns:AverageHeartRateBpm",
                                       namespaces="ns",
                                       fun=xmlValue))
        lapMaxHr <<- unlist(getNodeSet(doc = lp,
                                       path = "//ns:Lap/ns:MaximumHeartRateBpm",
                                       namespaces="ns",
                                       fun=xmlValue))
        lapIntensity <<- unlist(getNodeSet(doc = lp,
                                           path = "//ns:Lap/ns:Intensity",
                                           namespaces="ns",
                                           fun=xmlValue))
        lapTrigger <<- unlist(getNodeSet(doc = lp,
                                         path = "//ns:Lap/ns:TriggerMethod",
                                         namespaces="ns",
                                         fun=xmlValue))
        trckNodes <- getNodeSet(doc = lp,
                                path = "//ns:Track",
                                namespaces = "ns")
        # iterate over tracks
        sapply(trckNodes, function(t) {
            #trackId <<- trackId + 1L
            trckptNodes <- getNodeSet(doc = t, path = "//ns:Trackpoint", namespaces = "ns")
            # iterate over trackpoints
            sapply(trckptNodes, function(tp) {
                #trackpointId <<- trackpointId + 1L
                tpTimestamp <- unlist(getNodeSet(doc = tp,
                                                 path = "/ns:Time",
                                                 namespaces="ns",
                                                 fun=xmlValue))
                tpLat <- unlist(getNodeSet(doc = tp,
                                           path = "//ns:Trackpoint/ns:Position/ns:LatitudeDegrees",
                                           namespaces="ns",
                                           fun=xmlValue))
                tpLong <- unlist(getNodeSet(doc = tp,
                                            path = "//ns:Trackpoint/ns:Position/ns:LongitudeDegrees",
                                            namespaces="ns",
                                            fun=xmlValue))
                tpAlt <- unlist(getNodeSet(doc = tp,
                                           path = "//ns:Trackpoint/ns:AltitudeMeters",
                                           namespaces="ns",
                                           fun=xmlValue))
                tpDist <- unlist(getNodeSet(doc = tp,
                                            path = "//ns:Trackpoint/ns:DistanceMeters",
                                            namespaces="ns",
                                            fun=xmlValue))
                tpSpeed <- unlist(getNodeSet(doc = tp,
                                             path = "//ns:Trackpoint/ns:Extensions/ex:TPX/ex:Speed",
                                             namespaces=c(
                                                   ns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2",
                                                   ex="http://www.garmin.com/xmlschemas/ActivityExtension/v2"),,
                                             fun=xmlValue))
                tpCadence <- unlist(getNodeSet(doc = tp,
                                               path = "//ns:Trackpoint/ns:Extensions/ex:TPX/ex:RunCadence",
                                               namespaces=c(
                                                   ns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2",
                                                   ex="http://www.garmin.com/xmlschemas/ActivityExtension/v2"),
                                               fun=xmlValue))
                print(cbind(lapId, tpTimestamp, tpLat, tpLong, tpAlt, tpDist, tpSpeed, tpCadence))
            })
        })
    })
})

