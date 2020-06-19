xp <- example_HiC(version = "new")

xr <- GenomicRanges::GRanges("chr1", IRanges::IRanges(10e6, 20e6))

# Basic tests -------------------------------------------------------------

test_that("hic_extractor works as intended", {
  res <- hic_extractor(xp, xrange = xr)
  expect_equal(dim(res), c(66, 9))
})

# Argument tests ----------------------------------------------------------

test_that("hic_extractor can work with two experiments", {
  ctrl <- hic_extractor(xp, NULL, xr)
  test <- hic_extractor(xp, xp, xr)
  expect_equal(nrow(ctrl) * 2, nrow(test))
  
  ctrl <- length(unique(ctrl$exp))
  test <- length(unique(test$exp))
  expect_equal(ctrl + 1, test)
})

test_that("hic_extractor can work with two ranges", {
  yr <- xr
  yr@ranges@start <- yr@ranges@start + 20e6L
  
  ctrl <- hic_extractor(xp, NULL, xr, NULL)
  test <- hic_extractor(xp, NULL, xr, yr)
  
  expect_true(all(ctrl$xidx %in% ctrl$yidx))
  expect_false(all(test$xidx %in% test$yidx))
})

test_that("hic_extractor can extract multiple xranges", {
  xr2 <- xr
  xr2@ranges@start <- xr@ranges@start + 20e6L
  xr2 <- c(xr, xr2)
  
  ctrl <- hic_extractor(xp, NULL, xr)
  test <- hic_extractor(xp, NULL, xr2)
  
  expect_equal(length(unique(ctrl$entry)), 1)
  expect_equal(length(unique(test$entry)), 2)
})

test_that("hic_extractor can extract parallel entries", {
  xr2 <- xr
  xr2@ranges@start <- xr@ranges@start + 20e6L
  xr2 <- c(xr, xr2)
  
  yr <- xr2
  yr@ranges@start <- yr@ranges@start + 60e6L
  
  res <- hic_extractor(xp, NULL, xr2, yr)
  
  expect_equal(length(unique(res$entry)), 2)
  expect_false(all(res$xidx %in% res$yidx))
})

test_that("hic_extractor drops yranges when incompatible", {
  yr <- yr2 <- xr
  yr2@ranges@start <- yr2@ranges@start + 10e6L
  yr <- c(yr, yr2)
  
  ctrl <- hic_extractor(xp, NULL, yr, yr)
  expect_message({test <- hic_extractor(xp, NULL, xr, yr)},
                 "incompatible dimensions.")
  expect_equal(length(unique(test$entry)), 1)
  expect_equal(length(unique(ctrl$entry)), 2)
})

# Error tests -------------------------------------------------------------

test_that("hic_extractor fails if exp1 is not a contacts object", {
  expect_error({hic_extractor("nonsense")},
               "Please supply a GENOVA contacts object")
})

test_that("hic_extractor fails if exp2 is not a contacts object", {
  expect_error({hic_extractor(xp, "nonsense")},
               "Please supply a GENOVA contacts object")
})

test_that("hic_extractor cannot combine different resolutions", {
  xp2 <- xp
  attr(xp2, "resolution") <- 1e5
  expect_error({hic_extractor(xp, xp2)},
               "Resolutions of experiments are incompatible") 
})

test_that("hic_extractor cannot combine experiments with different indices", {
  xp2 <- xp
  xp2$IDX$V1 <- "chr2"
  expect_error({hic_extractor(xp, xp2)},
               "Cannot extract two experiments with differences in indices")
})

test_that("hic_extractor gives informative error upon invalid xrange", {
  expect_error({hic_extractor(xp, xrange = NULL)},
               "Please supply a valid `xrange` argument")
})
