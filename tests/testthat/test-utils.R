context("test-utils")

test_that("try_require returns error when package is absent", {
  expect_error(try_require("nonsense", "test"),
               "Please install and try again")
})

test_that("try_require loads package namespace", {

  unloadNamespace("rtracklayer")
  pkgs <- loadedNamespaces()
  expect_false("rtracklayer" %in% pkgs)
  expect_false("package:rtracklayer" %in% search())

  try_require("rtracklayer", "test")
  pkgs <- loadedNamespaces()
  expect_true("rtracklayer" %in% pkgs)
  expect_false("package:rtracklayer" %in% search())
})

test_that("%||% does what it is supposed to do", {
  x <- NULL
  y <- 10
  z <- 5
  expect_equal(x %||% y, 10)
  expect_equal(z %||% y, 5)
})

test_that("function grabber grabs functions", {
  x <- .grab_ggplot_internals()
  classes <- table(sapply(x, class))
  expect_gt(classes["function"], 10)
})
