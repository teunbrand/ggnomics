cases <- list(
  GreekSoldier(Rle(1:10)),
  c(15:20),
  Rle(5:10, 10:5),
  IRanges(1:5, width = 10),
  GRanges(c("chr1:100-200", "chr2:100-200"))
)

limits <- list(
  c(0, 11),
  c(10, 25),
  c(5, 10),
  c(1, 14),
  GRanges(c("chr1:100-300", "chr2:50-200"))
)

test_that("S4ForceFlat works as intended", {

  test <- mapply(S4ForceFlat, x = cases, limits = limits)
  
  ctrl <- list(
    seq(0, 1, length.out = 12)[2:11],
    seq(0, 1, length.out = 16)[6:11],
    rep(seq(0, 1, by = 0.2), 10:5),
    seq(0, 1, length.out = 27)[c(10, 12, 14, 16, 18)],
    c(50.5, 101.5 + 201) / c(353)
  )
  
  expect_equal(test, ctrl)
  
  
})

test_that("S4ForceFlat has contextual behaviour", {
  x <- GRanges("chr1:1-2")
  y <- IRanges(5, 10)
  
  expect_equal(S4ForceFlat(x, GRanges(c("chr1:1-4")), "xmax"), 0.5)
  expect_equal(S4ForceFlat(y, c(0, 10.5), "xmin"), 4.5/10.5)
})
