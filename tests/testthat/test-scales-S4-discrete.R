test_that("Discrete S4 classes are recognised as such", {

  expect_identical(scale_type(Rle(letters[1:3])),
                   "S4_discrete")
  expect_identical(scale_type(Rle(1:3)),
                   "S4_continuous")
  expect_identical(scale_type(Factor(letters[1:3])),
                   "S4_discrete")
  expect_identical(scale_type(Factor(1:3)),
                   "S4_continuous")
})

test_that("Discrete S4 scale can be found", {
  fun <- getFromNamespace("find_scale", "ggplot2")

  x <- fun("x", Rle(letters[1:3]))
  expect_is(x, "ScaleS4Discrete")

  y <- fun("y", Rle(letters[1:3]))
  expect_is(y, "ScaleS4Discrete")
})

test_that("Discrete S4 scale can be added to a plot", {
  ctrl <- ggplot(iris)
  testx <- ctrl + scale_x_S4_discrete()
  testy <- ctrl + scale_y_S4_discrete()

  expect_is(testx$scales$scales[[1]], "ScaleS4Discrete")
  expect_is(testy$scales$scales[[1]], "ScaleS4Discrete")
})

test_that("Discrete S4 scale is recognised if type is Vector", {
  ctrl <- data.frame(x = factor(1:5), y = 5:1)
  test <- DataFrame(x = Rle(letters[1:5]), y = 5:1)

  ctrl <- ggplot(ctrl, aes(x, y)) + geom_point()
  test <- ggplot(test, aes(x, y)) + geom_point()

  ctrl <- layer_scales(ctrl)
  test <- layer_scales(test)

  expect_equal(ctrl$y, test$y)
  expect_is(ctrl$x, "ScaleDiscretePosition")
  expect_is(test$x, "ScaleS4DiscretePosition")
})

test_that("Discrete S4 scale can be used with base R column types", {
  ctrl <- ggplot(mpg, aes(manufacturer, class)) + geom_point()
  test <- ctrl + scale_x_S4_discrete() + scale_y_S4_discrete()

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


test_that("Discrete S4 scale can work with Vectors", {
  g <- ggplot(DataFrame(x = Factor(letters[1:3]), y = 3:1), aes(x, y)) + geom_point()

  # Test scale training is done properly
  sc <- layer_scales(g)
  expect_identical(sc$x$get_limits(), letters[1:3])
  expect_identical(sc$x$get_breaks(), structure(letters[1:3], pos = 1:3))
  expect_null(sc$x$get_breaks_minor())
  expect_identical(sc$x$get_labels(), letters[1:3])

  # Test plot can become ggplotGrob
  expres <- substitute(ggplotGrob(g))
  expect_silent(eval(expres))
})
