library(XML)

doc <- xmlTreeParse(file="../inst/extdata/activity_439915418.tcx")

activity <- getNodeSet(doc = doc, path = "//ns:Activity", namespaces="ns")


# Activity
getNodeSet(doc, path = "//ns:Activity/@Sport", namespaces="ns") # OK
getNodeSet(doc, path = "//ns:Activity/ns:Id", namespaces="ns", fun=xmlValue) # OK
# Lap
getNodeSet(doc, path = "//ns:Activity/ns:Lap", namespaces="ns") # OK
getNodeSet(doc, path = "//ns:Activity/ns:Lap/@StartTime", namespaces="ns") # OK
getNodeSet(doc, path = "//ns:Activity/ns:Lap/ns:TotalTimeSeconds", namespaces="ns", fun=xmlValue)
getNodeSet(doc, path = "//ns:Activity/ns:Lap/ns:DistanceMeters", namespaces="ns", fun=xmlValue)
getNodeSet(doc, path = "//ns:Activity/ns:Lap/ns:Calories", namespaces="ns", fun=xmlValue)
getNodeSet(doc, path = "//ns:Activity/ns:Lap/ns:AverageHeartRateBpm", namespaces="ns", fun=xmlValue)
getNodeSet(doc, path = "//ns:Activity/ns:Lap/ns:MaximumHeartRateBpm", namespaces="ns", fun=xmlValue)
getNodeSet(doc, path = "//ns:Activity/ns:Lap/ns:Intensity", namespaces="ns", fun=xmlValue)
getNodeSet(doc, path = "//ns:Activity/ns:Lap/ns:TriggerMethod", namespaces="ns", fun=xmlValue)




