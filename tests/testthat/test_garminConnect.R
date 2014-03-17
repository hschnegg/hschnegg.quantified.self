context("Garmin Connect")

test_that("Test garminConnect class", {
    
    gc <- garminConnect$new()

    expect_that(gc$login(), prints_text("successfully connected"))
    expect_that(gc$retrieveActivityList(), is_a("data.frame"))
    expect_that(gc$downloadTcx(454818889), prints_text("available"))
})
