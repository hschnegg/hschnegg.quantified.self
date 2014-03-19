context("Garmin Connect")

test_that("Test garminConnect class", {
    
    gc <- garminConnect$new()

    # Test login to Garmin Connect
    expect_that(gc$login(), prints_text("successfully connected"))
    # Test that a list of activities is retrieved
    expect_that(gc$retrieveActivityList(), is_a("data.frame"))
    # Test that download from Garmin Connect is working
    expect_that(gc$downloadTcx(454818889), prints_text("available"))
})
