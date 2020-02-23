# Normal minor breaks -----------------------------------------------------

# Tests equivalent to the scales package tests for minor breaks

l1 <- c(0, 9)
l2 <- -l1
b1 <- scales::extended_breaks()(l1)
b2 <- scales::extended_breaks()(l2)
m1 <- S4BreaksMinor(b1, l1, n = 2)
m2 <- S4BreaksMinor(b2, l2, n = 2)

test_that("minor breaks are calculated correctly", {
  expect_equal(m1, seq(b1[1], b1[length(b1)], by = 1.25))
  expect_equal(m2, seq(b2[1], b2[length(b2)], by = 1.25))
})

test_that("minor breaks for reversed scales are comparable to non-reversed", {
  expect_equal(m1, sort(-m2))
})

# GRanges minor breaks ----------------------------------------------------

test_that("Genomic minor breaks are like extended breaks", {
  Q <- c(1, 5, 2, 4, 3)
  
  # Simple case
  lims <- GRanges("chr1:100-200")
  
  test <- Nightfall(S4BreaksMinor(NA, lims, 5))
  test <- start(test)
  
  ctrl <- scales::extended_breaks(Q = Q)(c(100, 200))
  
  expect_equal(ctrl, test)
  
  # Ranges adjacent in no-seqlevel space
  lims <- GRanges(c("chr1:100-200", "chr2:200-300"))
  
  test <- Nightfall(S4BreaksMinor(NA, lims, 5))
  test <- unique(start(test))
  
  ctrl <- scales::extended_breaks(Q = Q)(c(100, 300))
  
  expect_equal(ctrl, test)
  
  # Choose largest range to do extendedbreaks on
  lims <- GRanges(c("chr1:100-200", "chr2:300-600"))
  
  test <- Nightfall(S4BreaksMinor(NA, lims, 5))
  test <- split(start(test), decode(seqnames(test)))
  
  ctrl <- scales::extended_breaks(Q = Q)(c(300, 600))
  
  expect_equal(ctrl, test$chr2)
  expect_equal(c(100, 200), test$chr1)
  
})
