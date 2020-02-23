# Numeric major breaks ----------------------------------------------------

# Test equivalency to scales::extended_breaks()

test_that("S4BreaksMajor gives same as scales::extended_breaks for numeric", {
  efun <- scales::extended_breaks()
  
  lims <- list(c(10, 20), c(-2000, -1000), NA_real_, Inf, NaN)
  ctrl <- lapply(lims, efun)
  test <- lapply(lims, S4BreaksMajor)
  
  expect_equal(ctrl, test)
})


# GRanges major breaks ----------------------------------------------------

# Test gives chromosome starts and ends

test_that("S4BreaksMajor gives chromosome extremes", {
  lims <- GRanges(c("chr1:1-3", "chr2:200-300", "chr3:3000-4000"))
  
  test <- Nightfall(S4BreaksMajor(lims))
  
  expect_equal(start(test), c(1, 3, 200, 300, 3000, 4000))
  
  # Should also work for negative positions
  
  lims <- shift(lims, -1234)
  
  test <- Nightfall(S4BreaksMajor(lims))
  
  expect_equal(start(test), c(-1233, -1231, -1034, -934, 1766, 2766))
})

test_that("S4BreaksMajor handles zero-length input", {
  x <- list(numeric(), GRanges())
  
  test <- lapply(x, S4BreaksMajor)
  
  expect_equal(test, list(numeric(), GreekSoldier(GRanges())))
})