df <- DataFrame(x = 1:2, y = 1:2, col = Rle(1:2))
base <- ggplot(df, aes(x, y))

test_that("continuous colour scales can be added to a plot", {
  # Test default/gradient scale
  test <- base + geom_tile(aes(colour = col), size = 1)
  sc <- ggplot_build(test)
  sc <- sc$plot$scales$scales[[1]]
  expect_is(sc, "ScaleS4Continuous")
  grb <- layer_grob(test)[[1]]
  expect_equal(grb$gp$col, c("#132B43", "#56B1F7"))

  # Test viridis scale
  test <- test + scale_colour_S4_viridis_c()
  sc <- ggplot_build(test)
  sc <- sc$plot$scales$scales[[1]]
  expect_is(sc, "ScaleS4Continuous")
  grb <- layer_grob(test)[[1]]
  expect_equal(grb$gp$col, c("#440154", "#FDE725"))
})

test_that("continuous fill scales can be added to a plot", {
  # Test default/gradient scale
  test <- base + geom_tile(aes(fill = col), size = 1)
  sc <- ggplot_build(test)
  sc <- sc$plot$scales$scales[[1]]
  expect_is(sc, "ScaleS4Continuous")
  grb <- layer_grob(test)[[1]]
  expect_equal(grb$gp$fill, c("#132B43", "#56B1F7"))

  # Test viridis scale
  test <- test + scale_fill_S4_viridis_c()
  sc <- ggplot_build(test)
  sc <- sc$plot$scales$scales[[1]]
  expect_is(sc, "ScaleS4Continuous")
  grb <- layer_grob(test)[[1]]
  expect_equal(grb$gp$fill, c("#440154", "#FDE725"))
})

df <- DataFrame(x = 1:2, y = 1:2, col = Rle(letters[1:2]))
base <- ggplot(df, aes(x, y))

test_that("discrete colour scales can be added to a plot", {
  # Test default/hue scale
  test <- base + geom_tile(aes(colour = col), size = 1)
  sc <- ggplot_build(test)
  sc <- sc$plot$scales$scales[[1]]
  expect_is(sc, "ScaleS4Discrete")
  grb <- layer_grob(test)[[1]]
  expect_equal(grb$gp$col, c("#F8766D", "#00BFC4"))
})

test_that("discrete fill scales can be added to a plot", {
  # Test default/hue scale
  test <- base + geom_tile(aes(fill = col), size = 1)
  sc <- ggplot_build(test)
  sc <- sc$plot$scales$scales[[1]]
  expect_is(sc, "ScaleS4Discrete")
  grb <- layer_grob(test)[[1]]
  expect_equal(grb$gp$fill, c("#F8766D", "#00BFC4"))
})
