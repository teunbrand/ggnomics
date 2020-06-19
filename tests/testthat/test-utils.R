test_that(".grab_ggplot_internals works", {
  x <- ggnomics:::.grab_ggplot_internals()
  test <- vapply(x, is.function, logical(1))
  expect_true(all(test))
})
