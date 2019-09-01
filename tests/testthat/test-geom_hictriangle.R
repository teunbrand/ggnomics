context("test-geom_hictriangle")

library(GenomicRanges)

set.seed(0)
genova_exp <- example_HiC()


# Extractor tests ---------------------------------------------------------

test_that("Hi-C data extactor extracts triangle Hi-C data", {
  gr <- GRanges("chr1", IRanges(20e6, 100e6))
  res <- ggnomics:::extract_hicdata(genova_exp, xranges = gr, triangle = TRUE)
  expect_equal(max(res$x), end(gr) + genova_exp$RES/2)
  expect_equal(min(res$x), start(gr) - genova_exp$RES/2)
  expect_equal(max(res$y), end(gr) + genova_exp$RES/2)
  expect_equal(min(res$y), start(gr) - genova_exp$RES/2)
  expect_equal(sum(is.na(res$contacts)), 0)
  expect_true(all(res$y >= res$x))
  expect_is(attr(res, "xchr"), "Rle")
  expect_is(attr(res, "ychr"), "Rle")

  gr <- GRanges("chr1", IRanges(20e6, 100e6))
  res <- ggnomics:::extract_hicdata(genova_exp, xranges = gr, triangle = TRUE)
  expect_equal(max(res$x), end(gr) + genova_exp$RES/2)
  expect_equal(min(res$x), start(gr) - genova_exp$RES/2)
  expect_equal(max(res$y), end(gr) + genova_exp$RES/2)
  expect_equal(min(res$y), start(gr) - genova_exp$RES/2)
  expect_equal(sum(is.na(res$contacts)), 0)
  expect_true(all(res$y >= res$x))
  expect_is(attr(res, "xchr"), "Rle")
  expect_is(attr(res, "ychr"), "Rle")
})

# Basic tests -------------------------------------------------------------

test_that("geom_hictriangle adds layer to plot", {
  gr <- GRanges("chr1", IRanges(20e6, 100e6))
  g <- ggplot() +
    geom_hictriangle(genova_exp, gr)
  expect_is(g$layers[[1]]$geom, "GeomHicTriangle")
  expect_is(g$layers[[1]]$geom, "GeomPolygon")
})

test_that("geom_hictriangle can be build", {
  gr <- GRanges("chr1", IRanges(20e6, 100e6))
  g <- ggplot() +
    geom_hictriangle(genova_exp, gr)
  g <- ggplot_build(g)
  expect_is(g$plot$layers[[1]]$geom, "GeomHicTriangle")
  expect_is(g$plot$layers[[1]]$geom, "GeomPolygon")
})

test_that("geom_hictriangle can make gtable", {
  gr <- GRanges("chr1", IRanges(20e6, 100e6))
  g <- ggplot() +
    geom_hictriangle(genova_exp, gr)
  g <- ggplotGrob(g)
  expect_is(g, "gtable")
  grob <- g$grobs[g$layout$name == "panel"][[1]]
  expect_true(any(grepl("geom_polygon", names(grob$children))))
})


# Specific tests ----------------------------------------------------------

gr <- GRanges("chr1", IRanges(20e6 + 1, 100e6))
base <- ggplot() + geom_hictriangle(genova_exp, gr)

test_that("geom_hictriangle does not have sawtooth", {
  dat <- layer_data(base)
  expect_equal(sum(dat$y < -1e-15), 0)
})

test_that("geom_hictriangle has appropriate resolution", {
  dat <- layer_data(base)
  expect_equal(resolution(dat$x), genova_exp$RES/2)
  expect_equal(max(dat$y), width(gr) + genova_exp$RES)
  expect_equal(resolution(dat$y), genova_exp$RES)
  dat <- split(dat, dat$group)
  xranges <- vapply(dat, function(d){
    max(d$x) - min(d$x)
  }, numeric(1))
  expect_true(all(xranges == genova_exp$RES))
})

test_that("geom_hictriangle throws warning when invalid exp or range", {
  # Test range misses data
  gr <- GRanges("chr1", IRanges(20e6 + 1, 100e6))
  test <- substitute(ggplot() +
    geom_hictriangle(genova_exp, list("chr" = "chr1", start = start(gr))))
  expect_warning(expect_error(eval(test), "Invalid Hi-C Layer"), 
                 "Please supply")
  
  # Test chromosome is numeric
  test <- substitute(
    ggplot() +
      geom_hictriangle(
        genova_exp, 
        list("chr" = 1, start = start(gr), end = end(gr)))
  )
  expect_error(expect_warning(eval(test), "as characters of factors"),
                 "Invalid Hi-C Layer")
  
  test <- substitute(
    ggplot() +
      geom_hictriangle(
        genova_exp, 
        list("chr" = "chr1", start = "nonsense", end = end(gr)))
  )
  expect_warning(expect_error(eval(test), "Invalid Hi-C Layer"),
                 "coordinates as numeric")
})
