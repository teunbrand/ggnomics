cases <- list(
  GRanges(c("chr1:100-200", "chr2:200-300")),
  IRanges(c(10, 20), width = 5),
  Rle(c(1, 10), c(2, 2)),
  c(15, 20)
)

limits <- list(
  GRanges(c("chr1:50-250", "chr2:100-150")),
  c(8, 18),
  c(0, 5),
  c(8, 18)
)

should_oob <- list(
  c(FALSE, TRUE),
  c(FALSE, TRUE),
  c(FALSE, FALSE, TRUE, TRUE),
  c(FALSE, TRUE)
)

# is_oob ------------------------------------------------------------------

test_that("is_oob works as inteded", {
  test <- mapply(is_oob, x = cases, range = limits)
  expect_identical(test, should_oob)
})

# censorThis --------------------------------------------------------------

test_that("censorThis works as intended", {
  # Note that S4 Vectors are not meant to be exposed to censorThis
  wh <- lapply(cases, GreekSoldier)
  
  test <- mapply(censorThis, x = wh, range = limits)
  
  # Censored values should be detected as NAs
  nas <- lapply(test, is.na)
  
  expect_identical(nas, should_oob)
  
  # Should retain classes
  classes <- unlist(lapply(lapply(test, class), head, 1))
  expect_identical(classes, c("WoodenHorse", "WoodenHorse", 
                              "WoodenHorse", "numeric"))
  
  implicit <- unlist(lapply(test, HelenOfTroy))
  expect_identical(implicit, c("GRanges", "IRanges", "Rle", "numeric"))
})

# squishThis --------------------------------------------------------------

test_that("squishThis works as intended", {
  wh <- lapply(cases, GreekSoldier)
  test <- mapply(squishThis, x = wh, range = limits)
  test <- lapply(test, Nightfall)
  
  expect_equal(
    test,
    list(
      GRanges(c("chr1:100-200", "chr2:200-199")),
      IRanges(c(10, 18), c(14, 17)),
      Rle(c(1, 5), c(2, 2)),
      c(15, 18)
    )
  )
})

test_that("squish_infiniteThis works as intended", {
  # I don't think other classes than Rle support infinite values
  newcases <- list(
    c(Rle(c(-Inf, -1, 0, 1, 2, Inf))),
    c(-Inf, -1, 0, 1, 2, Inf)
  )
  newcases[[3]] <- GreekSoldier(newcases[[1]])
  
  test <- lapply(newcases, squish_infiniteThis, range = c(-10, 10))

  expect_identical(test[[2]], c(-10, -1, 0, 1, 2, 10))
  expect_identical(as.vector(test[[1]]), test[[2]])
  expect_identical(Nightfall(test[[3]]), test[[1]])
})

# discardOob --------------------------------------------------------------

test_that("discardOob works as intended", {
  wh <- lapply(cases, GreekSoldier)
  test <- mapply(discardOob, x = wh, range = limits)
  test <- lapply(test, Nightfall)
  ctrl <- mapply(`[`, x = cases, i = lapply(should_oob, `!`))
  
  expect_equal(test, ctrl)
  
  # edge cases
  expect_null(discardOob(NULL))
  expect_equal(discardOob(c(NA, 0, 1, 2)), c(0, 1))
})

