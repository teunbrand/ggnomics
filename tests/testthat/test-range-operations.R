cases1 <- list(
  1:10,
  IRanges(1:2, 11:12),
  GRanges(c("chr1:100-200", "chr2:200-300")),
  GreekSoldier(Rle(1:3, 3:1))
)

cases2 <- list(
  20:1,
  IRanges(20:21, width = 10),
  GRanges(c("chr1:300-400", "chr2:500-600")),
  GreekSoldier(Rle(1:5, 5:1))
)

# S4Range -----------------------------------------------------------------

test_that("S4Range produces correct ranges with 1 input", {
  test <- lapply(cases1, S4Range)
  expect_identical(
    test,
    list(
      c(1L, 10L),
      c(1L, 12L),
      GRanges(c("chr1:100-200", "chr2:200-300")),
      c(1L, 3L)
    )
  )
})

test_that("S4Ranges produces correct ranges with multiple inputs", {
  test <- mapply(S4Range, cases1, cases2)
  expect_identical(
    test,
    list(
      c(1L, 20L),
      c(1L, 30L),
      GRanges(c("chr1:100-400", "chr2:200-600")),
      c(1L, 5L)
    )
  )
})


# S4ExpandRange -----------------------------------------------------------

test_that("S4ExpandRange works", {
  exps1 <- expand.grid(c(0, 1), c(0, 1), c(0, 1), c(0, 1))
  exps <- lapply(seq_len(nrow(exps1)), 
                 function(i){unname(unlist(exps1[i,]))})
  
  cases <- list(c(10, 20), GRanges(c("chr1:10-20", "chr2:20-30")),
                GRanges(c("chr1:2-1")))
  
  expect_null(S4ExpandRange(NULL))
  
  # Test simple case
  test <- mapply(S4ExpandRange, limits = rep(cases[1], length(exps)), 
                 expand = exps, SIMPLIFY = FALSE)
  ctrl <- mapply(ggplot2:::expand_range4, limits = rep(cases[1], length(exps)),
                 expand = exps, SIMPLIFY = FALSE)
  expect_equal(test, ctrl)
  
  # Test GRanges case
  test <- mapply(S4ExpandRange, limits = rep(cases[2], length(exps)), 
                 expand = exps, SIMPLIFY = FALSE)
  test <- unlist(as(test, "GRangesList"))
  
  ctrl <- rep(start(cases[[2]]), nrow(exps1))
  ctrl <- ctrl - rep(exps1$Var1, each = 2) * 10 - rep(exps1$Var2, each = 2)
  
  expect_equal(start(test), ctrl)
  
  ctrl <- rep(end(cases[[2]]), nrow(exps1))
  ctrl <- ctrl + rep(exps1$Var3, each = 2) * 10 + rep(exps1$Var4, each = 2)
  
  expect_equal(end(test), ctrl)
  
  # Test zero-width GRanges case
  test <- mapply(S4ExpandRange, limits = rep(cases[3], length(exps)),
                 expand = exps, SIMPLIFY = FALSE)
  test <- unlist(as(test, "GRangesList"))
  
  ctrl1 <- rep(start(cases[[3]]), nrow(exps1))
  ctrl1 <- ctrl1 - exps1$Var1 * 1 - exps1$Var2
  
  ctrl <- rep(end(cases[[3]]), nrow(exps1))
  ctrl <- ctrl + exps1$Var3 * 1 + exps1$Var4
  
  expect_equal(start(test), pmin(ctrl, ctrl1))
  expect_equal(end(test), pmax(ctrl, ctrl1))
})


# S4ZeroRange ---------------------------------------------------------------

test_that("S4ZeroRange works on numeric", {
  eps <- .Machine$double.eps
  cases <- lapply(c(1, 800, 1600), function(x)(c(1, x)))
  
  test <- vapply(cases, S4ZeroRange, logical(1))
  ctrl <- vapply(cases, scales::zero_range, logical(1))
  
  expect_equal(test, ctrl)
})

test_that("S4ZeroRange works on Ranges", {
  cases <- list(
    GRanges(c("chr1:10-9")), GRanges(c("chr1:10-9", "chr2:10-20")),
    IRanges(10, 9), IRanges(10, 10)
  )
  
  test <- vapply(cases, S4ZeroRange, logical(1))
  expect_equal(test, c(TRUE, FALSE, TRUE, FALSE))
})