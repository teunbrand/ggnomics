test_that("Plot arithmetic works on GRanges", {
  x <- GreekSoldier(GRanges(c("chr1:100-200", "chr2:200-300")))
  
  # Test operators
  test <- list(
    plus = x + 100,
    minus = x - 100,
    multiply = x * 2,
    divide = x / 2
  )
  # Check class conservation
  classes <- unlist(lapply(lapply(test, class), head, 1))
  expect_true(all(classes == "OakHorse"))
  
  test <- lapply(test, Nightfall)
  starts <- unname(unlist(lapply(test, start)))
  ends <- unname(unlist(lapply(test, end)))
  
  expect_equal(starts, c(200, 300, 0, 100, 200, 400, 50, 100))
  expect_equal(ends, c(300, 400, 100, 200, 400, 600, 100, 150))
  
  # Test unary operations
  y <- Nightfall(-x)
  expect_equal(start(y), c(-200, -300))
  expect_equal(end(y), c(-100, -200))
  
  y <- Nightfall(+x)
  expect_equal(start(y), c(100, 200))
  expect_equal(end(y), c(200, 300))
})

test_that("Plot arithmetic works on IRanges", {
  x <- GreekSoldier(IRanges(c("100-200", "200-300")))
  
  # Test operators
  test <- list(
    plus = x + 100,
    minus = x - 100,
    multiply = x * 2,
    divide = x / 2
  )
  # Check class conservation
  classes <- unlist(lapply(lapply(test, class), head, 1))
  expect_true(all(classes == "BeechHorse"))
  
  test <- lapply(test, Nightfall)
  starts <- unname(unlist(lapply(test, start)))
  ends <- unname(unlist(lapply(test, end)))
  
  expect_equal(starts, c(200, 300, 0, 100, 200, 400, 50, 100))
  expect_equal(ends, c(300, 400, 100, 200, 400, 600, 100, 150))
  
  # Test unary operations
  y <- Nightfall(-x)
  expect_equal(start(y), c(-200, -300))
  expect_equal(end(y), c(-100, -200))
  
  y <- Nightfall(+x)
  expect_equal(start(y), c(100, 200))
  expect_equal(end(y), c(200, 300))
})


test_that("Plot arithmetic works on numeric-like classes", {
  x <- GreekSoldier(Rle(c(100, 200), c(5, 5)))
  
  test <- list(
    plus = x + 100,
    minus = x - 100,
    multiply = x * 2,
    divide = x / 2
  )
  # Check class conservation
  classes <- unlist(lapply(lapply(test, class), head, 1))
  expect_true(all(classes == "BeechHorse"))
  
  test <- unname(unlist(lapply(lapply(test, Nightfall), runValue)))
  expect_equal(test, c(200, 300, 0, 100, 200, 400, 50, 100))
  
  x <- Nightfall(x + x)
  expect_equal(runValue(x), c(200, 400))
  
  
  x <- GreekSoldier(NumericList(c(100, 200), c(300, 400)))
  
  test <- list(
    plus = x + 100,
    minus = x - 100,
    multiply = x * 2,
    divide = x / 2
  )
  # Check class conservation
  classes <- unlist(lapply(lapply(test, class), head, 1))
  expect_true(all(classes == "BeechHorse"))
  
  test <- lapply(test, Nightfall)
  test <- unname(unlist(lapply(test, unlist)))
  
  expect_equal(test, c(200, 300, 400, 500, 0, 100, 200, 300, 200, 400,
                       600, 800, 50, 100, 150, 200))
})
  
test_that("Plot math works on numeric-like classes", {
  x <- GreekSoldier(Rle(c(100, 400), c(5, 5)))
  expect_equal(mean(x), 250)
  
  expect_equal(runValue(Nightfall(sqrt(x))), c(10, 20))
  
  x <- GreekSoldier(NumericList(c(100, 200), c(300, 400)))
  expect_equal(mean(x), c(150, 350))
})
