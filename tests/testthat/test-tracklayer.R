
# Basic tests -------------------------------------------------------------

test_that("tracklayer returns an instance of TrackLayer", {
  l <- tracklayer()
  expect_is(l, "TrackLayerInstance")
  expect_is(l, "TrackLayer")
  expect_is(l, "Layer")
})

test_that("tracklayer can be added to a plot", {
  g <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
    tracklayer()
  
  expect_is(g$layers[[1]], "TrackLayerInstance")
})

# Geom, stat and position matching ----------------------------------------

test_that("tracklayer finds geoms", {
  l <- tracklayer(geom = "line")
  expect_is(l$geom, "GeomLine")
})

test_that("tracklayer finds stats", {
  l <- tracklayer(stat = "density")
  expect_is(l$stat, "StatDensity")
})

test_that("tracklayer finds positions", {
  l <- tracklayer(position = "dodge")
  expect_is(l$position, "PositionDodge")
})

# Distributing Parameters -------------------------------------------------

test_that("tracklayer aesthetics go in aes_params", {
  l <- tracklayer(size = 3)
  expect_identical(l$aes_params, list(size = 3))
})

test_that("tracklayers correctly puts aes in mapping", {
  l <- tracklayer(mapping = aes(Sepal.Width))
  expect_identical(names(l$mapping), "x")
  expect_identical(l$mapping$x, quo(Sepal.Width))
})

test_that("tracklayers puts geom params in geom_params", {
  l <- tracklayer(geom = "raster", hjust = 1)
  expect_true("hjust" %in% names(l$geom_params))
  expect_false("vjust" %in% names(l$geom_params))
  expect_identical(l$geom_params$hjust, 1)
})

test_that("tracklayers put stat params in stat_params", {
  l <- tracklayer(stat = "bin", binwidth = 10)
  expect_true("binwidth" %in% names(l$stat_params))
  expect_false("bin" %in% names(l$stat_params))
  expect_identical(l$stat_params$binwidth, 10)
})

# Essence tests -----------------------------------------------------------

test_that("tracklayer adds FacetTrack to a plot", {
  ctrl <- ggplot(iris, aes(Sepal.Length, Sepal.Width))
  test <- ctrl + tracklayer()
  expect_is(ctrl$facet, "FacetNull")
  expect_is(test$facet, "FacetTrack")
})

test_that("tracklayer does not override other facets", {
  ctrl <- ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
    facet_wrap(~ Species)
  test <- ctrl + tracklayer()
  expect_identical(ctrl$facet, test$facet)
})

test_that("tracklayer puts in extra columns through the mapping", {
  data <- iris
  l <- tracklayer(data = data, 
                  mapping = aes(Sepal.Width, Sepal.Length, track_y = Species))
  out <- l$setup_layer(data = data, plot = ggplot())
  expect_true(all(names(data) %in% names(out)))
  expect_true(all(c("track_y", "track_x") %in% names(out)))
  expect_identical(out$track_x[1], "Default")
  expect_identical(unique(out$track_y), unique(iris$Species))
})

test_that("tracklayer punts in extra columns through the track_* arguments", {
  data <- iris
  x <- "an_x_variable"
  y <- "an_y_variable"
  l <- tracklayer(data = data,
                  track_y = y, track_x = x)
  out <- l$setup_layer(data = data, plot = ggplot())
  expect_true(all(names(data) %in% names(out)))
  expect_true(all(c("track_y", "track_x") %in% names(out)))
  expect_identical(out$track_x[1], x)
  expect_identical(out$track_y[1], y)
})

# Warnings and errors -----------------------------------------------------

test_that("tracklayer errors about missing geoms, stats and position", {
  expect_error(tracklayer(geom = NULL), "no geom")
  expect_error(tracklayer(stat = NULL), "no stat")
  expect_error(tracklayer(position = NULL), "no position")
})

test_that("tracklayers throws appropriate warnings", {
  expect_warning(tracklayer(show.legend = "blah"), "logical vector")
  expect_warning(tracklayer(blah = "green"), "unknown parameters")
  expect_warning(tracklayer(mapping = aes(blah = "green")), 
                 "unknown aesthetics")
})
