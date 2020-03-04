test_that("scale_type recognises S4 objects", {
  expect_identical(scale_type(Rle(1:3)), "S4_continuous")
})

test_that("S4Vector scale can be found", {
  fun <- getFromNamespace("find_scale", "ggplot2")

  x <- fun("x", Rle(1:3))
  expect_is(x, "ScaleS4Continuous")

  y <- fun("y", Rle(1:3))
  expect_is(y, "ScaleS4Continuous")
})

test_that("S4Vector scale can be added to a plot", {
  ctrl <- ggplot(iris)
  testx <- ctrl + scale_x_S4_continuous()
  testy <- ctrl + scale_y_S4_continuous()

  expect_is(testx$scales$scales[[1]], "ScaleS4Continuous")
  expect_is(testy$scales$scales[[1]], "ScaleS4Continuous")
})

test_that("S4Vector scale is recognised if type is Vector", {
  ctrl <- data.frame(x = 1:5, y = 5:1)
  test <- DataFrame(x = Rle(1:5), y = 5:1)

  ctrl <- ggplot(ctrl, aes(x, y)) + geom_point()
  test <- ggplot(test, aes(x, y)) + geom_point()

  ctrl <- layer_scales(ctrl)
  test <- layer_scales(test)

  expect_equal(ctrl$y, test$y)
  expect_is(ctrl$x, "ScaleContinuousPosition")
  expect_is(test$x, "ScaleS4ContinuousPosition")
})

test_that("S4Vector scale can be used with base R column types", {
  ctrl <- ggplot(iris, aes(Sepal.Width, Sepal.Length)) + geom_point()
  test <- ctrl + scale_x_S4_continuous() + scale_y_S4_continuous()

  ctrl_ld <- layer_data(ctrl)
  test_ld <- layer_data(test)

  # Data transformation should be identical
  expect_identical(ctrl_ld, test_ld)

  ctrl_sc <- layer_scales(ctrl)
  test_sc <- layer_scales(test)

  # Limits should be identical
  expect_identical(ctrl_sc$x$get_limits(),
                   test_sc$x$get_limits())
  # Breaks should be identical
  expect_identical(ctrl_sc$x$get_breaks(),
                   test_sc$x$get_breaks())
  # Minor breaks should be identical
  expect_identical(ctrl_sc$x$get_breaks_minor(),
                   test_sc$x$get_breaks_minor())
  # Labels should be identical
  expect_identical(ctrl_sc$x$get_labels(),
                   test_sc$x$get_labels())
})

test_that("scale_S4Vector can work with Vectors", {
  g <- ggplot(DataFrame(x = Rle(1:3), y = 3:1), aes(x, y)) + geom_point()

  # Test scale training is done properly
  sc <- layer_scales(g)
  expect_identical(sc$x$get_limits(), c(1L, 3L))
  expect_identical(sc$x$get_breaks(), c(1, 1.5, 2, 2.5, 3))
  expect_identical(sc$x$get_breaks_minor(), seq(1, 3, by = 0.25))
  expect_identical(sc$x$get_labels(), format(seq(1, 3, by = 0.5)))

  # Test plot can become ggplotGrob
  expres <- substitute(ggplotGrob(g))
  expect_silent(eval(expres))
})

test_that("scale_S4Vector can work with IRanges", {
  df <- DataFrame(x = IRanges::IRanges(1:3, 4:6), y = 1:3)
  g <- ggplot(df, aes(x = x, y = y)) +
    geom_tile(width = 0)

  # Test scale training is done properly
  sc <- layer_scales(g)
  expect_identical(sc$x$get_limits(), c(0.5, 6.5))
  expect_identical(sc$x$get_breaks(), c(NA, 2, 4, 6))
  expect_identical(sc$x$get_breaks_minor(), seq(1, 6, by = 1))
  expect_identical(sc$x$get_labels(), c(NA, format(seq(2, 6, by = 2))))

  # Test plot can become ggplotGrob
  expres <- substitute(ggplotGrob(g))
  expect_silent(eval(expres))

  # Test grob if satisfactory
  gr <- layer_grob(g)[[1]]
  expect_is(gr, "rect")
  expect_length(gr$x, 3L)
})

test_that("Continuous S4 scale can work with secondary axis", {
  df <- DataFrame(x = Rle(1:3), y = 3:1)
  ctrl <- ggplot(df, aes(x = x, y = y)) +
    geom_point()
  test <- ctrl + scale_x_S4_continuous(
    sec.axis = sec_axis(trans = ~ . + 1)
  )
  ctrl <- ctrl + scale_x_S4_continuous()
  
  test1 <- layer_scales(test)
  ctrl1 <- layer_scales(ctrl)
  
  expect_is(test1$x$secondary.axis, "AxisSecondary")
  expect_is(ctrl1$x$secondary.axis, "waiver")
  
  test <- ggplotGrob(test)
  ctrl <- ggplotGrob(ctrl)
  
  test <- test$grobs[grepl("axis-t", test$layout$name)][[1]]
  ctrl <- ctrl$grobs[grepl("axis-t", ctrl$layout$name)][[1]]
  
  expect_is(test, "absoluteGrob")
  expect_is(ctrl, "zeroGrob")
})
