test_that(
  "guide_axis_genomic returns proper object",
  {
    a <- guide_genomic_axis()
    expect_is(a, "guide")
    expect_is(a, "axis")
    expect_is(a, "genomic_axis")
  }
)

# Assuming test above went ok, we can pre-make the axis
untrained <- guide_genomic_axis()

test_that("guide_axis_genomic can be trained", {

  sc <- scale_x_genomic(limits = GRanges(c("chr1:100-200", "chr2:100-200")))
  sc$train(sc$limits)
  sc <- ggnomics:::view_scales_from_scale_S4(sc)

  b <- guide_train(untrained, sc$x, "x")
  expect_true(all(c("key", "key_minor") %in% names(b)))

  # Major key assumptions
  expect_true(all(c("chr1", "chr2") %in% b$key$.label))
  expect_is(b$key$x, "WoodenHorse")
  expect_is(b$key$.value, "WoodenHorse")
  expect_true(HelenOfTroy(b$key$x) == "UnstitchedGPos")
  expect_true(HelenOfTroy(b$key$.value) == "UnstitchedGPos")

  # Minor key assumptions
  expect_true(all(c("120", "150", "180") %in% b$key_minor$.label))
  expect_is(b$key_minor$x, "WoodenHorse")
  expect_is(b$key_minor$.value, "WoodenHorse")
  expect_true(HelenOfTroy(b$key_minor$x) == "UnstitchedGPos")
  expect_true(HelenOfTroy(b$key_minor$.value) == "UnstitchedGPos")
})

test_that("guide_axis_genomic training yield axis if non-S4 input", {
  a <- guide_genomic_axis()

  sc <- scale_x_continuous(limits = c(0, 10))
  sc$train(sc$limits)
  sc <- ggnomics:::view_scales_from_scale_S4(sc)

  b <- guide_train(a, sc$x, "x")
  expect_is(b, "axis")
  expect_false(inherits(b, "genomic_axis"))

})

# Assuming test above went ok, we can pre-make the axis
sc <- scale_x_genomic(limits = GRanges(c("chr1:100-200", "chr2:100-200")))
sc$train(sc$limits)
sc <- ggnomics:::view_scales_from_scale_S4(sc)
trained <- guide_train(untrained, sc$x, "x")

test_that("guide_axis_genomic can be transformed", {
  df <- DataFrame(x = GRanges(c("chr1:100-200", "chr2:100-200")),
                  y = 1:2)
  g <- ggplot(df, aes(x, y)) + geom_point()
  gb <- ggplot_build(g)

  gb$layout$setup_panel_guides(gb$plot$guides, gb$plot$layers, gb$plot$mapping)

  co <- gb$layout$coord
  # abc <- co$setup_panel_guides(gb$panel_params, trained)[[1]]$x

  trained$position <- "bottom"

  transformed <- guide_transform(trained, co, gb$layout$panel_params[[1]])

  expect_is(trained$key$x, "WoodenHorse")
  expect_is(transformed$key$x, "numeric")
  expect_is(trained$key_minor$x, "WoodenHorse")
  expect_is(transformed$key_minor$x, "numeric")
  expect_false("y" %in% names(trained$key))
  expect_true("y" %in% names(transformed$key))
  expect_false("y" %in% names(trained$key_minor))
  expect_true("y" %in% names(transformed$key_minor))
})

test_that("guide_axis_genomic grob can be made", {
  df <- DataFrame(x = GRanges(c("chr1:100-200", "chr2:100-200")),
                  y = 1:2)
  g <- ggplot(df, aes(x, y)) + geom_point()
  gt <- ggplotGrob(g)
  grob <- gt$grobs[[grep("axis-b", gt$layout$name)]]
  grob <- grob$children$axis

  expect_equal(nrow(grob$layout), 3)
  grobclass <- unlist(lapply(lapply(grob$grobs, class), head, 1))

  # This should be the ticks
  expect_true("polyline" %in% grobclass)
  # Should have 2 'titleGrob's, 1 for chromosome, one for position
  expect_equal(sum(grepl("titleGrob", grobclass)), 2)

  labs <- lapply(grob$grobs[grobclass != "polyline"], function(x) {
    x$children[[1]]$label
  })

  # Order or length, longest should be position, other chromosome
  labs <- labs[order(lengths(labs))]
  expect_equal(labs[[1]], c("chr1", "chr2"))
  expect_equal(labs[[2]], as.character(c(120, 150, 180, 120, 150, 180)))


  # Test secondary axis position
  gt <- ggplotGrob(g + scale_x_genomic(position = "top"))
  grob <- gt$grobs[[grep("axis-t", gt$layout$name)]]
  grob <- grob$children$axis

  expect_equal(nrow(grob$layout), 3)
  grobclass <- unlist(lapply(lapply(grob$grobs, class), head, 1))

  # This should be the ticks
  expect_true("polyline" %in% grobclass)
  # Should have 2 'titleGrob's, 1 for chromosome, one for position
  expect_equal(sum(grepl("titleGrob", grobclass)), 2)

  labs <- lapply(grob$grobs[grobclass != "polyline"], function(x) {
    x$children[[1]]$label
  })

  # Order or length, longest should be position, other chromosome
  labs <- labs[order(lengths(labs))]
  expect_equal(labs[[1]], c("chr1", "chr2"))
  expect_equal(labs[[2]], as.character(c(120, 150, 180, 120, 150, 180)))
})

test_that("guide_axis_genomic can be vertical", {
  df <- DataFrame(x = GRanges(c("chr1:100-200", "chr2:100-200")),
                  y = 1:2)
  g <- ggplot(df, aes(y, x)) + geom_point()
  gt <- ggplotGrob(g)
  grob <- gt$grobs[[grep("axis-l", gt$layout$name)]]
  grob <- grob$children$axis

  expect_equal(nrow(grob$layout), 3)
  grobclass <- unlist(lapply(lapply(grob$grobs, class), head, 1))

  # This should be the ticks
  expect_true("polyline" %in% grobclass)
  # Should have 2 'titleGrob's, 1 for chromosome, one for position
  expect_equal(sum(grepl("titleGrob", grobclass)), 2)

  labs <- lapply(grob$grobs[grobclass != "polyline"], function(x) {
    x$children[[1]]$label
  })

  # Order or length, longest should be position, other chromosome
  labs <- labs[order(lengths(labs))]
  expect_equal(labs[[1]], c("chr1", "chr2"))
  expect_equal(labs[[2]], as.character(c(120, 150, 180, 120, 150, 180)))


  # Test secondary axis position
  gt <- ggplotGrob(g + scale_y_genomic(position = "right"))
  grob <- gt$grobs[[grep("axis-r", gt$layout$name)]]
  grob <- grob$children$axis

  expect_equal(nrow(grob$layout), 3)
  grobclass <- unlist(lapply(lapply(grob$grobs, class), head, 1))

  # This should be the ticks
  expect_true("polyline" %in% grobclass)
  # Should have 2 'titleGrob's, 1 for chromosome, one for position
  expect_equal(sum(grepl("titleGrob", grobclass)), 2)

  labs <- lapply(grob$grobs[grobclass != "polyline"], function(x) {
    x$children[[1]]$label
  })

  # Order or length, longest should be position, other chromosome
  labs <- labs[order(lengths(labs))]
  expect_equal(labs[[1]], c("chr1", "chr2"))
  expect_equal(labs[[2]], as.character(c(120, 150, 180, 120, 150, 180)))
})
