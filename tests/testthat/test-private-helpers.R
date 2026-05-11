test_that("internal .pluck handles nested extraction and defaults", {
  x <- list(a = list(b = list(c = 42)))

  expect_equal(OdysseusPathwayModule:::.pluck(x, "a", "b", "c"), 42)
  expect_equal(OdysseusPathwayModule:::.pluck(x, "a", "missing", .default = "x"), "x")
})

test_that("internal .map_dfc combines outputs by column", {
  cols <- OdysseusPathwayModule:::.map_dfc(
    c("u", "v"),
    function(tag) data.frame(value = paste0(tag, 1:2), stringsAsFactors = FALSE)
  )

  expect_s3_class(cols, "data.frame")
  expect_equal(ncol(cols), 2)
  expect_equal(unname(cols[[1]]), c("u1", "u2"))
  expect_equal(unname(cols[[2]]), c("v1", "v2"))
})

test_that(".pluck with function accessor", {
  x <- list(a = 1:5)
  expect_equal(OdysseusPathwayModule:::.pluck(x, "a", length), 5)
})

test_that(".pluck returns .default for NULL element", {
  x <- list(a = NULL)
  expect_equal(OdysseusPathwayModule:::.pluck(x, "a", .default = "fb"), "fb")
})

test_that(".pluck returns .default for out-of-bounds numeric accessor", {
  expect_equal(OdysseusPathwayModule:::.pluck(list(1, 2), 10, .default = "oob"), "oob")
  expect_equal(OdysseusPathwayModule:::.pluck(list(1, 2), 0, .default = "oob"), "oob")
})

test_that(".pluck errors on invalid accessor type", {
  expect_error(
    OdysseusPathwayModule:::.pluck(list(a = 1), TRUE),
    "Accessor must be an integer, character, or function"
  )
})

test_that("extractBitSum decomposes powers-of-two combinations", {
  expect_equal(OdysseusPathwayModule:::extractBitSum(2), 1)
  expect_equal(OdysseusPathwayModule:::extractBitSum(6), c(2, 1))
  expect_equal(OdysseusPathwayModule:::extractBitSum(10), c(3, 1))
})
