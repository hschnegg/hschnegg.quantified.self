# Install package
install.packages("hschnegg.quantified.self_0.1.tar.gz", repos=NULL, type="source")

# Load package
library(hschnegg.quantified.self)

# List all activities stored in the local database
listLoadedActivity()

# List activities stored in Garmin Connect (user stats290_test)
listGarminConnectActivity()

# List activity available in Garmin Connect, not yet loaded in the local database
listNewActivity()

# Create instance of class tcxFile
f <- tcxFile$new()

# Read local file activity_439915418.tcx
f$read(439915418)

# Check that we have data in the three data frames (Fields)
f$activity
head(f$lap)
head(f$trackpoint)

# Save the data to the local database
f$saveToDb()

# Test that the saved data can be read from the database
f$readFromDb(439915402)

# Create instance of garminConnect class
gc <- garminConnect$new()

# Test login to Garmin Connect
gc$login()

# Retrieve a list of the activity stored in Garmin Connect
gc$retrieveActivityList()

# Download a Garmin Connect activity file (.tcx)
gc$downloadTcx(454818889)

# Download an activity from Garmin Connect and save it to the local database
retrieveActivity(454818889)

# Delete the previously loaded activity from the database
deleteActivity(454818889)

# Plot an activity available in the database on a Google map
mapActivity(439915402)
