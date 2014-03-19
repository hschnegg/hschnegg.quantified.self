context("TCX FILE")

test_that("Test tcxFile class", {

    # Create an instance of the tcxFile class and read one of the .tcx
    # test file
    f <- tcxFile$new()
    f$read(439915418)

    # Check that all the data frames contain data
    expect_that(nrow(f$activity), equals(1))
    expect_that(nrow(f$lap), is_more_than(1))
    expect_that(nrow(f$trackpoint), is_more_than(1))

    # Check that readFromDb retrieves expected data frames
    expect_that(f$readFromDb(433485172), prints_text("activity"))
    expect_that(f$readFromDb(433485172), prints_text("lap"))
    expect_that(f$readFromDb(433485172), prints_text("trackpoint"))
})
