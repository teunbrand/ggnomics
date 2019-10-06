# Basic Tests -------------------------------------------------------------

test_that("facet_track has appropriate classes", {
  f <- facet_track()
  expect_is(f, "FacetTrack")
  expect_is(f, "FacetGrid")
  expect_is(f, "Facet")
})

test_that("facet_track outputs correct quosures", {
  f <- facet_track()
  expect_true(all(c("rows", "cols") %in% names(f$params)))
  expect_true("track_y" %in% names(f$params$rows))
  expect_true("track_x" %in% names(f$params$cols))
  expect_equal(f$params$rows$track_y, ~track_y)
  expect_equal(f$params$cols$track_x, ~track_x)
})

test_that("facet_track converts numeric width/height to null units", {
  f <- facet_track(widths = 10, heights = 2)
  expect_identical(f$params$widths, unit(10, "null"))
  expect_identical(f$params$heights, unit(2, "null"))
})


# Essence tests -----------------------------------------------------------

test_that("facet_track recognises 'track_x/y' columns", {
  df <- cbind(expand.grid(track_x = LETTERS[1:2], track_y = letters[1:2]), 
              x = 1:4)
  g <- ggplot(df, aes(x, x)) +
    geom_point() +
    facet_track()
  ld <- layer_data(g)
  expect_equal(length(unique(ld$PANEL)), 4)
  gt <- ggplotGrob(g)
  expect_equivalent(panel_rows(gt), data.frame(t = c(8, 10), b = c(8, 10)))
  expect_equivalent(panel_cols(gt), data.frame(l = c(6, 8), r = c(6, 8)))
})

test_that("facet_track drops unused 'track_x/y' variables", {
  df1 <- data.frame(x = 1:4)
  df2 <- data.frame(track_y = c(1, 2), x = 1:2)
  df3 <- data.frame(track_x = c(1, 2), x = 1:2)
  df4 <- data.frame(track_x = c(1,2,1,2), track_y = c(1,1,2,2), x = 1:4)
  
  f <- facet_track()
  ctrl <- f$setup_params(list(df1), f$params)
  expect_equal(ctrl$rows, quos())
  expect_equal(ctrl$cols, quos())
  
  test1 <- f$setup_params(list(df2), f$params)
  expect_equivalent(test1$rows$track_y, quo(track_y))
  expect_equal(test1$cols, quos())
  
  test2 <- f$setup_params(list(df3), f$params)
  expect_equal(test2$rows, quos())
  expect_equivalent(test2$cols$track_x, quo(track_x))
  
  test3 <- f$setup_params(list(df4), f$params)
  expect_equivalent(test1$rows$track_y, quo(track_y))
  expect_equivalent(test2$cols$track_x, quo(track_x))
})

test_that("facet_track can set widths and heights", {
  df <- cbind(expand.grid(track_x = 1:2, track_y = 1:2), x = 1:4)
  g <- ggplot(df, aes(x, x)) + geom_point()
  h <- grid::unit.c(unit(2, "cm"), unit(1, "inch"))
  w <- grid::unit.c(unit(10, "pt"), unit(1.5, "null"))
  
  ctrl <- g + facet_track(heights = NULL, widths = NULL)
  test <- g + facet_track(heights = h, widths = w)
  
  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)
  
  panels_x <- unique(ctrl$layout$l[grepl("panel", ctrl$layout$name)])
  panels_y <- unique(ctrl$layout$t[grepl("panel", ctrl$layout$name)])
  
  expect_true(all(startsWith(test[panels_y, panels_x]$layout$name, "panel")))
  
  expect_equivalent(ctrl$widths[panels_x], rep(unit(1, "null", NULL), 2))
  expect_equivalent(ctrl$heights[panels_y], rep(unit(1, "null", NULL), 2))
  
  expect_equivalent(test$widths[panels_x], list(w[1], w[2]))
  expect_equivalent(test$heights[panels_y], list(h[1], h[2]))
  
})

# Warnings and errors -----------------------------------------------------

test_that("facet_track warns about nonsense switch arguments", {
  expect_error(facet_track(switch = "blah"), "switch must be either")
})
