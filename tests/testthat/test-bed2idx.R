abs <- data.frame(
  V1 = rep(paste0("chr", LETTERS[1:5]), c(30, 20, 15, 10, 5)),
  V2 = as.integer(c(0:29, 0:19, 0:14, 0:9, 0:4) * 1e6)
)
abs$V3 <- as.integer(abs$V2 + 1e6)
abs$V4 <- seq_along(abs$V1)

# Entry 2 is out of order
# Entry 3 spans three bins
# Entry 5 is out of bounds
# Entry 6 is on non-existing chromosome
bed <- data.frame(
  V1 = paste0("chr", LETTERS[c(1,2,1,4,4,6)]),
  V2 = as.integer(c(1.5, 1.5, 5.5, 2.5, 12.5, 1.5) * 1e6),
  V3 = as.integer(c(1.6, 1.6, 7.5, 2.6, 12.6, 1.6) * 1e6)
)

test_that("bed2idx works", {
  exp <- c(2L, 32L, 7L, 68L, 75L, NA_integer_)
  test <- bed2idx(abs, bed)
  expect_identical(exp, test)
})

test_that("bed2idx respects mode", {
  # Only bed entry 3 spans multiple
  
  # Test start
  test <- bed2idx(abs, bed, "start")
  expect_identical(test,
                   c(2L, 32L, 6L, 68L, 75L, NA_integer_))
  
  # Test end
  test <- bed2idx(abs, bed, "end")
  expect_identical(test,
                   c(2L, 32L, 8L, 68L, 75L, NA_integer_))
  
  # Test center
  test <- bed2idx(abs, bed, "centre")
  expect_identical(test,
                   c(2L, 32L, 7L, 68L, 75L, NA_integer_))
})

test_that("bed2idx handles out of bounds values", {
  newbed <- bed
  newbed[2, 2:3] <- -1 * newbed[2, 2:3]
  test <- bed2idx(abs, newbed)
  expect_identical(test,
                   c(2L, 31L, 7L, 68L, 75L, NA_integer_))
})

test_that("bed2idx accepts GRanges", {
  gr <- with(bed, GenomicRanges::GRanges(V1, IRanges::IRanges(V2, V3)))
  exp <- c(2L, 32L, 7L, 68L, 75L, NA_integer_)
  test <- bed2idx(abs, gr)
  expect_identical(exp, test)
})