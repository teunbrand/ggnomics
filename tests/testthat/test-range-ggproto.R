# S4Train tests -----------------------------------------------------------

news <- list(
  GreekSoldier(Rle(1:3)),
  1:10,
  factor(LETTERS[1:3]),
  IRanges(1:3, width = 10),
  GRanges(c("chr1:100-200", "chr2:100-200"))
)

olds <- list(
  c(-3, 3),
  c(15, 20),
  factor(LETTERS[3:5]),
  c(-5, 1),
  GRanges(c("chr1:130-170", "chr2:50-250"))
)

test_that("S4Train reports correct ranges", {
  test <- lapply(news, S4Train)
  expect_equal(
    test,
    list(
      c(1L, 3L),
      c(1L, 10L),
      LETTERS[1:3],
      c(1L, 12L),
      news[[5]]
    )
  )
})

test_that("S4Train can correctly update ranges", {
  test <- mapply(S4Train, new = news, existing = olds)
  expect_equal(
    test,
    list(
      c(-3, 3),
      c(1, 20),
      LETTERS[1:5],
      c(-5, 12),
      GRanges(c("chr1:100-200", "chr2:50-250"))
    )
  )
})

test_that("S4Train can deal with edge cases", {
  expect_equal(S4Train(c(Inf, -Inf, NA, NaN, 20), c(NA, 10)), c(10, 20))
  
  f <- factor(LETTERS[1:3], LETTERS)
  expect_equal(S4Train(f, drop = FALSE), LETTERS)
  expect_equal(S4Train(f, drop = TRUE), LETTERS[1:3])
  expect_null(S4Train(NULL))
})


# ggproto tests -----------------------------------------------------------

test_that("ggproto creation works", {
  dis <- new_S4_discrete_range("a")
  expect_is(dis, "RangeS4Discrete")
  expect_is(dis, "RangeS4")
  dis$train(LETTERS[1:3])
  expect_equal(dis$range, LETTERS[1:3])
  
  con <- new_S4_continuous_range("b")
  expect_is(con, "RangeS4Continuous")
  expect_is(con, "RangeS4")
  con$train(1:10)
  expect_equal(con$range, c(1L, 10L))
})
