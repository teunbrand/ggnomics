context("test-facet_ideogrid")

setup_cytobands(example_cytobands(),
                example_cytoband_colours())

df <- iris
df$chr <- ifelse(df$Species == "setosa", "chr1", "chr2")

base <- ggplot(df, aes(Sepal.Width, y = Sepal.Length)) +
  geom_point()


# Basic tests -------------------------------------------------------------

test_that("facet_ideogrid adds ggproto", {
  g <- base + facet_ideogrid("chr1" ~ .)
  expect_is(g$facet, "gg")
  expect_is(g$facet, "Facet")
  expect_is(g$facet, "FacetIdeo")
  expect_is(g$facet, "FacetIdeoGrid")
})

test_that("facet_ideogrid adds ideogram", {
  g <- base + facet_ideogrid("chr1" ~ .)
  gt <- ggplotGrob(g)
  expect_equal(sum(grepl("ideo-r", gt$layout$name)), 1)
  ideo <- gt$grobs[grepl("ideo-r", gt$layout$name)][[1]]
  expect_equal(length(ideo$children), 3)
})

# Essence tests -----------------------------------------------------------

test_that("facet_ideogrid adds multiple horizontal ideograms", {
  g <- base + facet_ideogrid(chr ~ .)
  gt <- ggplotGrob(g)
  # Test ideograms have been added
  expect_equal(sum(grepl("ideo-r", gt$layout$name)), 2)

  # Test ideograms are different
  ideos <- gt$grobs[grepl("ideo-r", gt$layout$name)]
  pattern1 <- as.numeric(ideos[[1]]$children[[2]]$y)
  pattern2 <- as.numeric(ideos[[2]]$children[[2]]$y)
  expect_false(identical(pattern1, pattern2))
})

test_that("facet_ideogrid adds multiple vertical ideograms", {
  g <- base + facet_ideogrid(~ chr)
  gt <- ggplotGrob(g)
  # Test ideograms have been added
  expect_equal(sum(grepl("ideo-t", gt$layout$name)), 2)

  # Test ideograms are different
  ideos <- gt$grobs[grepl("ideo-t", gt$layout$name)]
  pattern1 <- as.numeric(ideos[[1]]$children[[2]]$y)
  pattern2 <- as.numeric(ideos[[2]]$children[[2]]$y)
  expect_false(identical(pattern1, pattern2))
})

test_that("facet_ideogrid works on both vars simultaneously", {
  g <- base + facet_ideogrid(chr ~ rev(chr))
  gt <- ggplotGrob(g)$layout
  expect_equal(sum(grepl("ideo", gt$name)), 4)
  expect_equal(sum(grepl("ideo-r", gt$name)), 2)
  expect_equal(sum(grepl("ideo-t", gt$name)), 2)
})

test_that("facet_ideogrid can be nested with other variables", {
  first  <- base + facet_ideogrid(~ chr + Species)
  second <- base + facet_ideogrid(~ Species + chr)
  first  <- ggplotGrob(first)
  second <- ggplotGrob(second)
  first  <- first$grobs[grepl("ideo",  first$layout$name)]
  second <- second$grobs[grepl("ideo", second$layout$name)]
  
  # Should give 3 ideograms
  expect_equal(length(first),  3)
  expect_equal(length(second), 3)
  
  # Ideograms should not be nullGrobs
  first  <- sapply(first,  inherits, "null")
  second <- sapply(second, inherits, "null")
  expect_equal(sum(first),  0)
  expect_equal(sum(second), 0)
  
  # Should not draw double ideograms when self-nested
  selfnest <- base + facet_ideogrid(~ chr + rev(chr))
  selfnest <- ggplotGrob(selfnest)
  selfnest <- selfnest$grobs[grepl("ideo", selfnest$layout$name)]
  expect_equal(length(selfnest), 3)
  selfnest <- sapply(selfnest, inherits, "null")
  expect_equal(sum(selfnest), 0)
})

test_that("facet_ideogrid highlighting works", {
  ctrl <- base + facet_ideogrid(~ chr, high.col = NA)
  test <- base + facet_ideogrid(~ chr, high.col = "blue")
  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)
  ctrl <- ctrl$grobs[grepl("ideo", ctrl$layout$name)][[1]]$children
  test <- test$grobs[grepl("ideo", test$layout$name)][[2]]$children
  
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

test_that("facet_ideogrid doesn't add ideograms when unnecessary", {
  # Behaves like facet_grid when facets aren't chromosomes
  ctrl <- base + facet_grid(~ Species)
  test <- base + facet_ideogrid(~ Species)
  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)
  expect_identical(test$layout, ctrl$layout)
  expect_equal(sum(grepl("ideo", test$layout$name)), 0)

  # Should only draw ideograms along chromosome vars
  g <- base + facet_ideogrid(Species ~ chr)
  gt <- ggplotGrob(g)
  expect_equal(sum(grepl("ideo", gt$layout$name)), 2)

  # Places nullGrob when only some chromosomes are present
  g <- base + facet_ideogrid(~ chr)
  g$data$chr <- ifelse(g$data$Species == "virginica", "virginica", g$data$chr)
  gt <- ggplotGrob(g)
  expect_equal(sum(grepl("ideo", gt$layout$name)), 3)
  ideos <- gt$grobs[grepl("ideo", gt$layout$name)]
  isnull <- sapply(ideos, inherits, "null")
  expect_equal(length(isnull), 3)
  expect_equal(sum(isnull), 1)
})

test_that("facet_ideogrid reverses ideogram along with axis", {
  ctrl <- base + facet_ideogrid(~ chr) +
    scale_x_continuous(trans = "identity")
  test <- base + facet_ideogrid(~ chr) +
    scale_x_continuous(trans = "reverse")
  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)
  ctrl <- ctrl$grobs[grepl("ideo", ctrl$layout$name)][[1]]$children[[2]]
  test <- test$grobs[grepl("ideo", test$layout$name)][[1]]$children[[2]]
  # Ctrl is 1 - test
  expect_equal(as.numeric(test$x), 1 - as.numeric(ctrl$x))
  # Check test is not symmetric
  expect_false(all(as.numeric(test$x) == 1 - as.numeric(test$x)))
})

test_that("facet_ideogrid draws ideograms in absence of strips", {
  ctrl <- base + facet_ideogrid(~ chr) + theme(strip.text = element_text())
  test <- base + facet_ideogrid(~ chr) + theme(strip.text = element_blank())
  ctrl <- ggplotGrob(ctrl)
  test <- ggplotGrob(test)
  ctrl <- ctrl$grobs[grepl("strip", ctrl$layout$name)][[1]]
  test <- test$grobs[grepl("strip", test$layout$name)][[1]]
  expect_is(ctrl, "gTree")
  expect_is(test, "zeroGrob")
})

# Argument tests ----------------------------------------------------------

test_that("facet_ideogrid switch arguments works", {
  x_ctrl <- base + facet_ideogrid(~ chr)
  y_ctrl <- base + facet_ideogrid(chr ~ .)
  x_test <- base + facet_ideogrid(~ chr, switch = "x")
  y_test <- base + facet_ideogrid(chr ~ ., switch = "y")
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
  expect_equal(y_ctrl_panel + 1, y_ctrl_ideo)
  expect_equal(y_test_panel - 1, y_test_ideo)
})

test_that("facet_ideogrid follows stip.placement theme witout switch", {
  x_ctrl <- base + facet_ideogrid(~ chr) +
    theme(strip.placement = "inside")
  y_ctrl <- base + facet_ideogrid(chr ~.) +
    theme(strip.placement = "inside")
  x_test <- base + facet_ideogrid(~ chr) +
    theme(strip.placement = "outside")
  y_test <- base + facet_ideogrid(chr ~ .) +
    theme(strip.placement = "outside")
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
  expect_equal(x_test_panel - 3, x_test_ideo)
  expect_equal(y_ctrl_panel + 1, y_ctrl_ideo)
  expect_equal(y_test_panel + 3, y_test_ideo)
})

test_that("facet_ideogrid follows strip.placement theme with switch", {
  x_ctrl <- base + facet_ideogrid(~ chr, switch = "x") +
    theme(strip.placement = "inside")
  y_ctrl <- base + facet_ideogrid(chr ~ ., switch = "y") +
    theme(strip.placement = "inside")
  x_test <- base + facet_ideogrid(~ chr, switch = "x") +
    theme(strip.placement = "outside")
  y_test <- base + facet_ideogrid(chr ~ ., switch = "y") +
    theme(strip.placement = "outside")
  x_ctrl <- ggplotGrob(x_ctrl)$layout
  y_ctrl <- ggplotGrob(y_ctrl)$layout
  x_test <- ggplotGrob(x_test)$layout
  y_test <- ggplotGrob(y_test)$layout

  x_ctrl_ideo <- unique(x_ctrl$t[grepl("ideo",   x_ctrl$name)])
  x_ctrl_axis <- unique(x_ctrl$t[grepl("axis-b", x_ctrl$name)])
  x_test_ideo <- unique(x_test$t[grepl("ideo",   x_test$name)])
  x_test_axis <- unique(x_test$t[grepl("axis-b", x_test$name)])
  y_ctrl_ideo <- unique(y_ctrl$l[grepl("ideo",   y_ctrl$name)])
  y_ctrl_axis <- unique(y_ctrl$l[grepl("axis-l", y_ctrl$name)])
  y_test_ideo <- unique(y_test$l[grepl("ideo",   y_test$name)])
  y_test_axis <- unique(y_test$l[grepl("axis-l", y_test$name)])

  expect_equal(x_ctrl_axis - 2, x_ctrl_ideo)
  expect_equal(x_test_axis + 2, x_test_ideo)
  expect_equal(y_ctrl_axis + 2, y_ctrl_ideo)
  expect_equal(y_test_axis - 2, y_test_ideo)
})

test_that("facet_ideogrid handles ideo.size arguments", {
  g1 <- base + facet_ideogrid(~ chr, ideo.size = unit(1, "inch"))
  g2 <- base + facet_ideogrid(~ chr, ideo.size = unit(5, "mm"))
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

test_that("facet_ideogrid warns when ideo.size unit improper", {
  test <- substitute(base + facet_ideogrid(~ chr, ideo.size = 1))
  expect_error(eval(test), "grid::unit()")
})

test_that("facet_ideogrid warns when free scales and coord is not", {
  g <- base + facet_ideogrid(facets = rev(chr) ~ chr, scales = "free")
  ctrl <- g + coord_cartesian()
  test <- g + coord_fixed()
  expect_silent(ggplotGrob(ctrl))
  expect_error(ggplotGrob(test), "doesn't support free scales")
})

test_that("facet_ideogrid warns when switch is improper", {
  test <- substitute(base + facet_ideogrid(~ chr, switch = "nonsense"))
  expect_error(eval(test), "switch must be either")
})


clear_cytoband_cache()
test_that("facet_ideogrid warns when no ideograms found", {
  clear_cytoband_cache()
  expect_error(base + facet_ideogrid(~ chr))
})
