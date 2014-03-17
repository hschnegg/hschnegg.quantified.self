context("TCX FILE")

test_that("Test tcxFile class", {
    
    f <- tcxFile$new()
    f$read(439915418)

    expect_that(nrow(f$activity), equals(1))
    expect_that(nrow(f$lap), is_more_than(1))
    expect_that(nrow(f$trackpoint), is_more_than(1))

    expect_that(f$readFromDb(439915418), prints_text("activity"))
    expect_that(f$readFromDb(439915418), prints_text("lap"))
    expect_that(f$readFromDb(439915418), prints_text("trackpoint"))
})
