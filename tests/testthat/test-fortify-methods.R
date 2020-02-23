# Vector class ------------------------------------------------------------

test_that("fortify on Vector converts them to data.frame", {
  ctrl <- Rle(1:5)
  test <- fortify(ctrl)

  expect_identical(ctrl, test$.Rle)
})

test_that("fortify on Vectors column-ises element metadata", {
  ctrl <- Rle(1:5)
  mcols(ctrl) <- DataFrame(y = LETTERS[1:5])
  test <- fortify(ctrl)

  expect_identical(mcols(ctrl)$y, test$y)
})

# DataFrame class ---------------------------------------------------------

test_that("fortify on DataFrames converts them to data.frame", {
  ctrl <- DataFrame(x = 1:5, y = 5:1)
  test <- fortify(ctrl)
  expect_is(ctrl, "DFrame")
  expect_is(test, "data.frame")
})

test_that("fortify on DataFrames preserves S4 classes", {
  ctrl <- DataFrame(x = Rle(1:5), y = Factor(LETTERS[1:5]))
  test <- fortify(ctrl)

  expect_identical(ctrl$x, test$x)
  expect_identical(ctrl$y, test$y)
})

test_that("fortified DataFrames preserve S4 classes on subsetting", {
  ctrl <- DataFrame(x = Rle(1:5), y = Factor(LETTERS[1:5]))
  test <- fortify(ctrl)

  ctrl <- ctrl[2:4,]
  test <- test[2:4,]

  expect_identical(ctrl$x, test$x)
  expect_identical(ctrl$y, test$y)
})
