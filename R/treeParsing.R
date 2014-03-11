library(XML)

doc <- xmlTreeParse(file="../inst/extdata/activity_439915418.tcx", useInternalNodes = TRUE)

activity <- getNodeSet(doc = doc, path = "//ns:Activity", namespaces="ns")


# Activity
getNodeSet(doc, path = "//ns:Activity/@Sport", namespaces="ns") # OK
unlist(getNodeSet(doc, path = "//ns:Activity/ns:Id", namespaces="ns", fun=xmlValue)) # OK
# Lap
#getNodeSet(doc, path = "//ns:Activity/ns:Lap", namespaces="ns") # OK
unlist(getNodeSet(doc, path = "//ns:Activity/ns:Lap/@StartTime", namespaces="ns")) # OK
getNodeSet(doc, path = "//ns:Activity/ns:Lap/ns:TotalTimeSeconds", namespaces="ns", fun=xmlValue)
getNodeSet(doc, path = "//ns:Activity/ns:Lap/ns:DistanceMeters", namespaces="ns", fun=xmlValue)
getNodeSet(doc, path = "//ns:Activity/ns:Lap/ns:Calories", namespaces="ns", fun=xmlValue)
getNodeSet(doc, path = "//ns:Activity/ns:Lap/ns:AverageHeartRateBpm", namespaces="ns", fun=xmlValue)
getNodeSet(doc, path = "//ns:Activity/ns:Lap/ns:MaximumHeartRateBpm", namespaces="ns", fun=xmlValue)
getNodeSet(doc, path = "//ns:Activity/ns:Lap/ns:Intensity", namespaces="ns", fun=xmlValue)
getNodeSet(doc, path = "//ns:Activity/ns:Lap/ns:TriggerMethod", namespaces="ns", fun=xmlValue)
# Track
getNodeSet(doc, path = "//ns:Track/ns:Trackpoint/ns:Time", namespaces="ns", fun=xmlValue)
getNodeSet(doc, path = "//ns:Track/ns:Trackpoint/ns:Position/ns:LatitudeDegrees", namespaces="ns", fun=xmlValue)


activity <- getNodeSet(doc = doc, path = "//ns:Activity", namespaces = "ns")
# iterate over activities
sapply(activity, function(act) {
    lap <- getNodeSet(doc = act, path = "//ns:Lap", namespaces = "ns")
    # iterate over laps
    sapply(lap, function(l) {
        #trackpt <- getNodeSet(doc = l, path = "//ns:Track/ns:Trackpoint", namespaces = "ns")
        time <- getNodeSet(doc = l, path = "//ns:Track/ns:Trackpoint/ns:Time", namespaces="ns", fun=xmlValue)
        print(time)
        })
    })



library(XML)

doc <- xmlTreeParse(file="../inst/extdata/activity_439915418.tcx", useInternalNodes = TRUE)

activityId <- 0L
lapId <- 0L
trackId <- 0L
trackpointId <- 0L

activityNodes <- getNodeSet(doc = doc, path = "//ns:Activity", namespaces = "ns")
# iterate over activities
sapply(activityNodes, function(act) {
    #activityId <<- activityId + 1L
    sport <<- unlist(getNodeSet(doc = act, path = "//ns:Activity/@Sport", namespaces="ns"))
    id <<-  unlist(getNodeSet(doc = act, path = "//ns:Activity/ns:Id", namespaces="ns", fun=xmlValue))
    lapNodes <- getNodeSet(doc = act, path = "//ns:Lap", namespaces = "ns")
    # iterate over laps
    sapply(lapNodes, function(lp) {
        lapId <<- lapId + 1L
        trckNodes <- getNodeSet(doc = lp, path = "//ns:Track", namespaces = "ns")
        # iterate over tracks
        sapply(trckNodes, function(t) {
            #trackId <<- trackId + 1L
            trckptNodes <- getNodeSet(doc = t, path = "//ns:Trackpoint", namespaces = "ns")
            # iterate over trackpoints
            sapply(trckptNodes, function(tp) {
                #trackpointId <<- trackpointId + 1L
                print(sport)
            })
        })
    })
})

