context("test-facet_ideowrap")

setup_cytobands(example_cytobands(),
                example_cytoband_colours())

df <- iris
df$chr <- ifelse(df$Species == "setosa", "chr1", "chr2")

base <- ggplot(df, aes(Sepal.Width, y = Sepal.Length)) +
  geom_point()

# Basic tests -------------------------------------------------------------

test_that("facet_ideowrap adds ggproto", {
  g <- base + facet_ideowrap("chr1" ~ .)
  expect_is(g$facet, "gg")
  expect_is(g$facet, "Facet")
  expect_is(g$facet, "FacetIdeo")
  expect_is(g$facet, "FacetIdeoWrap")
})

test_that("facet_ideowrap adds ideogram", {
  g <- base + facet_ideowrap("chr1" ~ .)
  gt <- ggplotGrob(g)
  expect_equal(sum(grepl("ideo", gt$layout$name)), 1)
  ideo <- gt$grobs[grepl("ideo", gt$layout$name)][[1]]$grobs[[1]]
  expect_equal(length(ideo$children), 3)
})

# Essence tests -----------------------------------------------------------

test_that("facet_ideowrap adds multiple ideograms horizontally", {
  g <- base + facet_ideowrap(chr ~ .)
  gt <- ggplotGrob(g)
  # Test ideograms have been added
  expect_equal(sum(grepl("ideo", gt$layout$name)), 2)

  # Test ideograms are different
  ideos <- gt$grobs[grepl("ideo", gt$layout$name)]
  pattern1 <- as.numeric(ideos[[1]]$grobs[[1]]$children[[2]]$y)
  pattern2 <- as.numeric(ideos[[2]]$grobs[[1]]$children[[2]]$y)
  expect_false(identical(pattern1, pattern2))
})

test_that("facet_ideowrap adds multiple ideograms vertically", {
  g <- base + facet_ideowrap(~ chr, ncol = 1)
  gt <- ggplotGrob(g)
  # Test ideograms have been added
  expect_equal(sum(grepl("ideo", gt$layout$name)), 2)

  # Test ideograms are different
  ideos <- gt$grobs[grepl("ideo", gt$layout$name)]
  ideos <- sapply(ideos, function(x){x$grobs})
  pattern1 <- as.numeric(ideos[[1]]$children[[2]]$y)
  pattern2 <- as.numeric(ideos[[2]]$children[[2]]$y)
  expect_false(identical(pattern1, pattern2))
})

test_that("facet_ideowrap can be nested with other variables", {
  first  <- base + facet_ideowrap(~ chr + Species)
  second <- base + facet_ideowrap(~ Species + chr)
  first  <- ggplotGrob(first)
  second <- ggplotGrob(second)
  first  <- first$grobs[grepl("ideo",  first$layout$name)]
  first  <- sapply(first, function(x) {x$grobs})
  second <- second$grobs[grepl("ideo", second$layout$name)]
  second <- sapply(second, function(x) {x$grobs})
  
  # Should give 3 ideograms
  expect_equal(length(first),  3)
  expect_equal(length(second), 3)
  
  # Ideograms should not be nullGrobs
  first  <- sapply(first,  inherits, "null")
  second <- sapply(second, inherits, "null")
  expect_equal(sum(first),  0)
  expect_equal(sum(second), 0)
  
  # Should not draw double ideograms when self-nested
  selfnest <- base + facet_ideowrap(~ chr + rev(chr))
  selfnest <- ggplotGrob(selfnest)
  selfnest <- selfnest$grobs[grepl("ideo", selfnest$layout$name)]
  selfnest <- sapply(selfnest, function(x) {x$grobs})
  expect_equal(length(selfnest), 3)
  selfnest <- sapply(selfnest, inherits, "null")
  expect_equal(sum(selfnest), 0)
})

test_that("facet_ideowrap highlighting works", {
  ctrl <- base + facet_ideowrap(~ chr, high.col = NA)
  test <- base + facet_ideowrap(~ chr, high.col = "blue")
  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)
  ctrl <- ctrl$grobs[grepl("ideo", ctrl$layout$name)]
  test <- test$grobs[grepl("ideo", test$layout$name)]
  ctrl <- sapply(ctrl, function(x) {x$grobs})[[1]]$children
  test <- sapply(test, function(x) {x$grobs})[[1]]$children
  
  # Test children lengths
  expect_equal(length(ctrl), 3)
  expect_equal(length(test), 5)
  
  # Check test has colours
  expect_equal(test[[1]]$gp$fill, "blue")
  expect_equal(test[[5]]$gp$col,  "blue")
  
  # Ctrl band height should be larger than test band height
  test <- unique(as.numeric(test[[3]]$height))
  ctrl <- unique(as.numeric(ctrl[[2]]$height))
  
  expect_gt(ctrl, test)
})

# Edge cases --------------------------------------------------------------

test_that("facet_ideowrap doesn't add ideograms when unnecessary", {
  # Behaves like facet_grid when facets aren't chromosomes
  ctrl <- base + facet_wrap(~ Species)
  test <- base + facet_ideowrap(~ Species)
  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)
  expect_identical(test$layout, ctrl$layout)
  expect_equal(sum(grepl("ideo", test$layout$name)), 0)

  # Places nullGrob when only some chromosomes are present
  g <- base + facet_ideowrap(~ chr)
  g$data$chr <- ifelse(g$data$Species == "virginica", "virginica", g$data$chr)
  gt <- ggplotGrob(g)
  expect_equal(sum(grepl("ideo", gt$layout$name)), 3)
  ideos <- gt$grobs[grepl("ideo", gt$layout$name)]
  ideos <- sapply(ideos, function(x){x$grobs})
  isnull <- sapply(ideos, inherits, "null")
  expect_equal(length(isnull), 3)
  expect_equal(sum(isnull), 1)
})

test_that("facet_ideowrap reverses ideogram along with axis", {
  ctrl <- base + facet_ideowrap(~ chr) +
    scale_x_continuous(trans = "identity")
  test <- base + facet_ideowrap(~ chr) +
    scale_x_continuous(trans = "reverse")
  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)
  ctrl <- ctrl$grobs[grepl("ideo", ctrl$layout$name)][[1]]$grobs[[1]]$children[[2]]
  test <- test$grobs[grepl("ideo", test$layout$name)][[1]]$grobs[[1]]$children[[2]]
  # Ctrl is 1 - test
  expect_equal(as.numeric(test$x), 1 - as.numeric(ctrl$x))
  # Check test is not symmetric
  expect_false(all(as.numeric(test$x) == 1 - as.numeric(test$x)))
})

test_that("facet_ideowrap draws ideograms in absence of strips", {
  ctrl <- base + facet_ideowrap(~ chr) + theme(strip.text = element_text())
  test <- base + facet_ideowrap(~ chr) + theme(strip.text = element_blank())
  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)
  ctrl <- ctrl$grobs[grepl("strip", ctrl$layout$name)][[1]]
  test <- test$grobs[grepl("strip", test$layout$name)][[1]]
  expect_is(ctrl, "gTree")
  expect_is(test, "zeroGrob")
})

test_that("facet_ideowrap draws ideograms at empty panels", {
  ctrl <- ggplot(df, aes(Sepal.Width, y = Sepal.Length)) +
    geom_point() +
    facet_ideowrap(~ chr, drop = FALSE)
  testdat <- df
  testdat$chr <- factor("chr1", levels = c("chr1", "chr2"))
  test <- ggplot(testdat, aes(Sepal.Width, y = Sepal.Length)) +
    geom_point() +
    facet_ideowrap(~ chr, drop = FALSE)
  
  ctrl_df <- layer_data(ctrl)
  test_df <- layer_data(test)
  
  # Test should have only one panel specified
  expect_equal(length(unique(ctrl_df$PANEL)), 2)
  expect_equal(length(unique(test_df$PANEL)), 1)
  
  # Check ideograms are filled
  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)
  ctrl <- ctrl$grobs[grepl("ideo", ctrl$layout$name)]
  test <- test$grobs[grepl("ideo", test$layout$name)]
  ctrl <- lapply(ctrl, function(x) {x$grobs[[1]]$children})
  test <- lapply(test, function(x) {x$grobs[[1]]$children})
  
  expect_identical(lengths(ctrl), lengths(test))
})

# Argument tests ---------------------------------------------------------

test_that("facet_ideowrap strip.position arguments works", {
  x_ctrl <- base + facet_ideowrap(~ chr, strip.position = "top")
  y_ctrl <- base + facet_ideowrap(~ chr, strip.position = "right")
  x_test <- base + facet_ideowrap(~ chr, strip.position = "bottom")
  y_test <- base + facet_ideowrap(~ chr, strip.position = "left")
  x_ctrl <- ggplotGrob(x_ctrl)$layout
  y_ctrl <- ggplotGrob(y_ctrl)$layout
  x_test <- ggplotGrob(x_test)$layout
  y_test <- ggplotGrob(y_test)$layout

  x_ctrl_ideo  <- unique(x_ctrl$t[grepl("ideo",  x_ctrl$name)])
  x_ctrl_panel <- unique(x_ctrl$t[grepl("panel", x_ctrl$name)])
  x_test_ideo  <- unique(x_test$t[grepl("ideo",  x_test$name)])
  x_test_panel <- unique(x_test$t[grepl("panel", x_test$name)])
  y_ctrl_ideo  <- unique(y_ctrl$l[grepl("ideo",  y_ctrl$name)])
  y_ctrl_panel <- unique(y_ctrl$l[grepl("panel", y_ctrl$name)])
  y_test_ideo  <- unique(y_test$l[grepl("ideo",  y_test$name)])
  y_test_panel <- unique(y_test$l[grepl("panel", y_test$name)])

  expect_equal(x_ctrl_panel - 1, x_ctrl_ideo)
  expect_equal(x_test_panel + 1, x_test_ideo)
  expect_equal(y_ctrl_panel + 1, rev(y_ctrl_ideo))
  expect_equal(y_test_panel - 1, rev(y_test_ideo))
})

test_that("facet_ideowrap follows strip.placement theme", {
  # Check bottom
  ctrl <- base + facet_ideowrap(~ chr, strip.position = "bottom") +
    theme(strip.placement = "inside")
  test <- base + facet_ideowrap(~ chr, strip.position = "bottom") +
    theme(strip.placement = "outside")
  ctrl <- ggplotGrob(ctrl)$layout
  test <- ggplotGrob(test)$layout
  ctrl_ideo <- unique(ctrl$t[grepl("ideo",   ctrl$name)])
  ctrl_axis <- unique(ctrl$t[grepl("axis-b", ctrl$name)])
  test_ideo <- unique(test$t[grepl("ideo",   test$name)])
  test_axis <- unique(test$t[grepl("axis-b", test$name)])
  expect_equal(ctrl_axis - 2, ctrl_ideo)
  expect_equal(test_axis + 2, test_ideo)
  
  # Check top
  ctrl <- base + facet_ideowrap(~ chr, strip.position = "top") +
    theme(strip.placement = "inside")
  test <- base + facet_ideowrap(~ chr, strip.position = "top") +
    theme(strip.placement = "outside")
  ctrl <- ggplotGrob(ctrl)$layout
  test <- ggplotGrob(test)$layout
  ctrl_ideo  <- unique(ctrl$t[grepl("ideo",  ctrl$name)])
  ctrl_panel <- unique(ctrl$t[grepl("panel", ctrl$name)])
  test_ideo  <- unique(test$t[grepl("ideo",  test$name)])
  test_panel <- unique(test$t[grepl("panel", test$name)])
  expect_equal(ctrl_panel - 1, ctrl_ideo)
  expect_equal(test_panel - 3, test_ideo)
  
  # Check left
  ctrl <- base + facet_ideowrap(~ chr, strip.position = "left") +
    theme(strip.placement = "inside")
  test <- base + facet_ideowrap(~ chr, strip.position = "left") +
    theme(strip.placement = "outside")
  ctrl <- ggplotGrob(ctrl)$layout
  test <- ggplotGrob(test)$layout
  ctrl_ideo <- unique(ctrl$l[grepl("ideo",   ctrl$name)])
  ctrl_axis <- unique(ctrl$l[grepl("axis-l", ctrl$name)])
  test_ideo <- unique(test$l[grepl("ideo",   test$name)])
  test_axis <- unique(test$l[grepl("axis-l", test$name)])
  expect_equal(ctrl_axis + 2, ctrl_ideo)
  expect_equal(test_axis - 2, test_ideo)
  
  # Check right
  ctrl <- base + facet_ideowrap(~ chr, strip.position = "right") +
    theme(strip.placement = "inside")
  test <- base + facet_ideowrap(~ chr, strip.position = "right") +
    theme(strip.placement = "outside")
  ctrl <- ggplotGrob(ctrl)$layout
  test <- ggplotGrob(test)$layout
  ctrl_ideo  <- unique(ctrl$l[grepl("ideo",  ctrl$name)])
  ctrl_panel <- unique(ctrl$l[grepl("panel", ctrl$name)])
  test_ideo  <- unique(test$l[grepl("ideo",  test$name)])
  test_panel <- unique(test$l[grepl("panel", test$name)])
  expect_equal(rev(ctrl_panel) + 1, ctrl_ideo)
  expect_equal(rev(test_panel) + 3, test_ideo)
})

test_that("facet_ideowrap handles ideo.size arguments", {
  g1 <- base + facet_ideowrap(~ chr, ideo.size = unit(1, "inch"))
  g2 <- base + facet_ideowrap(~ chr, ideo.size = unit(5, "mm"))
  g1 <- ggplotGrob(g1)
  g2 <- ggplotGrob(g2)
  pos1 <- unique(g1$layout$t[grepl("ideo", g1$layout$name)])
  pos2 <- unique(g2$layout$t[grepl("ideo", g2$layout$name)])
  expect_equal(pos1, pos2)
  height1 <- g1$heights[pos1]
  height2 <- g2$heights[pos2]
  expect_equal(as.numeric(height1), 1)
  expect_equal(as.numeric(height2), 5)
  expect_equal(attr(height1, "unit"), "inch")
  expect_equal(attr(height2, "unit"), "mm")
})


# Warning tests -----------------------------------------------------------

test_that("facet_ideowrap warns when ideo.size unit improper", {
  test <- substitute(base + facet_ideowrap(~ chr, ideo.size = 1))
  expect_error(eval(test), "grid::unit()")
})

test_that("facet_ideowrap warns when free scales and coord is not", {
  g <- base + facet_ideowrap(facets = rev(chr) ~ chr, scales = "free")
  ctrl <- g + coord_cartesian()
  test <- g + coord_fixed()
  expect_silent(ggplotGrob(ctrl))
  expect_error(ggplotGrob(test), "doesn't support free scales")
})

test_that("facet_ideowrap warns when switch is improper", {
  test <- substitute(base + facet_ideowrap(~ chr, strip.position = "nonsense"))
  expect_error(eval(test), "should be one of")
})

clear_cytoband_cache()
test_that("facet_ideowrap warns when no ideograms found", {
  clear_cytoband_cache()
  expect_error(base + facet_ideowrap(~ chr))
})