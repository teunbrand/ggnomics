context("test-geom_rectrug")

df <- data.frame(
  xmin = c(1, 5),
  xmax = c(2, 7),
  ymin = c(1, 2),
  ymax = c(2, 4),
  fill = c("A", "B")
)

base <- ggplot(df, aes(xmin = xmin, xmax = xmax,
                       ymin = ymin, ymax = ymax,
                       fill = fill)) +
  geom_rect()

test_that("geom_rectrug can be added to plots", {
  g <- base + geom_rectrug()
  expect_is(g$layers[[2]]$geom, "GeomRectRug")

  gt <- ggplotGrob(g)
  gt <- gt$grobs[grepl("panel", gt$layout$name)][[1]]
  gt <- gt$children[[4]]$children

  expect_is(gt[[1]], "rect")
  expect_is(gt[[2]], "rect")
})

test_that("geom_rectrug recognises sides argument", {
  t <- base + geom_rectrug(sides = "t")
  b <- base + geom_rectrug(sides = "b")
  l <- base + geom_rectrug(sides = "l")
  r <- base + geom_rectrug(sides = "r")

  t <- layer_grob(t, 2)[[1]]$children[[1]]
  b <- layer_grob(b, 2)[[1]]$children[[1]]
  l <- layer_grob(l, 2)[[1]]$children[[1]]
  r <- layer_grob(r, 2)[[1]]$children[[1]]

  expect_equal(as.numeric(t$y), 1)
  expect_equal(as.numeric(b$y), 0)
  expect_equal(as.numeric(l$x), 0)
  expect_equal(as.numeric(r$x), 1)

  sizes <- c(t$height, b$height,
             r$width, l$width)
  expect_equal(sizes, c(0.03, 0.03, 0.03, 0.03))
})

test_that("geom_rectrug size can be set", {
  a <- base + geom_rectrug(length = unit(1, "inch"))
  b <- base + geom_rectrug(length = unit(5, "mm"))
  a <- layer_grob(a, 2)[[1]]$children[[1]]$height
  b <- layer_grob(b, 2)[[1]]$children[[1]]$height
  expect_equal(attr(a, "unit"), "inch")
  expect_equal(attr(b, "unit"), "mm")
  expect_equal(as.numeric(a), 1)
  expect_equal(as.numeric(b), 5)
})

test_that("coord flip flips rectrugs", {
  a <- base + geom_rectrug(sides = "b")
  b <- a + coord_flip()
  a <- layer_grob(a, 2)[[1]]$children[[1]]
  b <- layer_grob(b, 2)[[1]]$children[[1]]

  expect_equal(as.numeric(a$width), as.numeric(b$height))
})


# geom_tilerug ------------------------------------------------------------

df <- data.frame(
  x = c(1, 4),
  y = c(1, 2),
  width = c(2, 1),
  height = c(1, 2),
  fill = c("A", "B")
)

base <- ggplot(df, aes(x, y,
                       width = width, height = height,
                       fill = fill)) +
  geom_tile()

test_that("geom_rectrug can be added to plots", {
  g <- base + geom_tilerug()
  expect_is(g$layers[[2]]$geom, "GeomTileRug")
  expect_is(g$layers[[2]]$geom, "GeomRectRug")

  gt <- ggplotGrob(g)
  gt <- gt$grobs[grepl("panel", gt$layout$name)][[1]]
  gt <- gt$children[[4]]$children

  expect_is(gt[[1]], "rect")
  expect_is(gt[[2]], "rect")
})

test_that("geom_tilerug recognises sides argument", {
  t <- base + geom_tilerug(sides = "t")
  b <- base + geom_tilerug(sides = "b")
  l <- base + geom_tilerug(sides = "l")
  r <- base + geom_tilerug(sides = "r")

  t <- layer_grob(t, 2)[[1]]$children[[1]]
  b <- layer_grob(b, 2)[[1]]$children[[1]]
  l <- layer_grob(l, 2)[[1]]$children[[1]]
  r <- layer_grob(r, 2)[[1]]$children[[1]]

  expect_equal(as.numeric(t$y), 1)
  expect_equal(as.numeric(b$y), 0)
  expect_equal(as.numeric(l$x), 0)
  expect_equal(as.numeric(r$x), 1)

  sizes <- c(t$height, b$height,
             r$width, l$width)
  expect_equal(sizes, c(0.03, 0.03, 0.03, 0.03))
})

test_that("geom_tilerug size can be set", {
  a <- base + geom_tilerug(length = unit(1, "inch"))
  b <- base + geom_tilerug(length = unit(5, "mm"))
  a <- layer_grob(a, 2)[[1]]$children[[1]]$height
  b <- layer_grob(b, 2)[[1]]$children[[1]]$height
  expect_equal(attr(a, "unit"), "inch")
  expect_equal(attr(b, "unit"), "mm")
  expect_equal(as.numeric(a), 1)
  expect_equal(as.numeric(b), 5)
})

test_that("coord flip flips tilerugs", {
  a <- base + geom_tilerug(sides = "b")
  b <- a + coord_flip()
  a <- layer_grob(a, 2)[[1]]$children[[1]]
  b <- layer_grob(b, 2)[[1]]$children[[1]]

  expect_equal(as.numeric(a$width), as.numeric(b$height))
})
