context("test-ideograms")

test_that("example_cytobands returns intended data.frame", {
  cyto <- example_cytobands()

  # Test names
  expect_equal(names(cyto),
               c("chrom", "chromStart", "chromEnd", "name", "gieStain"))

  # Test column classes
  expect_is(cyto$chrom, "factor")
  expect_is(cyto$chromStart, "numeric")
  expect_is(cyto$chromEnd, "numeric")
  expect_is(cyto$name, "character")
  expect_is(cyto$gieStain, "factor")

  # Test column content
  expect_equal(levels(cyto$chrom), c("chr1", "chr2"))
  expect_equal(levels(cyto$gieStain),
               c("acen", "gneg", "gpos100", "gpos25",
                 "gpos50", "gpos75", "gvar"))
})

test_that("example_cytoband_colours returns compatible colours", {
  cyto <- example_cytobands()
  cytocols <- example_cytoband_colours()

  # Are gieStain levels represented?
  expect_true(all(levels(cyto$gieStain) %in% names(cytocols)))

  # Are the colours valid?
  expect_is(col2rgb(cytocols), "matrix")
})

test_that("setup_cytobands sets up facet ggprotos", {
  # Check cache is empty by default
  cache <- getFromNamespace("tbcache", "ggnomics")
  expect_is(cache, "environment")
  expect_equal(ls(cache), character(0))

  setup_cytobands(example_cytobands(),
                  example_cytoband_colours())

  # Expect cache to be filled with ggprotos
  cache <- getFromNamespace("tbcache", "ggnomics")
  expect_is(cache, "environment")
  expect_equal(ls(cache), c("FacetIdeo", "FacetIdeoGrid", "FacetIdeoWrap"))

  # Test ggproto classes
  expect_is(cache$FacetIdeo, "FacetIdeo")
  expect_is(cache$FacetIdeo, "Facet")
  expect_is(cache$FacetIdeo, "ggproto")
  expect_is(cache$FacetIdeoGrid, "FacetIdeo")
  expect_is(cache$FacetIdeoGrid, "Facet")
  expect_is(cache$FacetIdeoGrid, "ggproto")
  expect_is(cache$FacetIdeoWrap, "FacetIdeo")
  expect_is(cache$FacetIdeoWrap, "Facet")
  expect_is(cache$FacetIdeoWrap, "ggproto")

  # Check FacetIdeo carries data
  dataclass <- rapply(cache$FacetIdeo$ideograms, class)
  expect_equivalent(dataclass, rep(c("numeric", "character", "numeric", "character"), c(6,1,6,1)))

  # Check FacetIdeo has rendering function
  expect_true(exists("render.ideo", cache$FacetIdeo))
})

setup_cytobands(example_cytobands(),
                example_cytoband_colours())

test_that("FacetIdeo can render ideograms along x", {
  cache <- getFromNamespace("tbcache", "ggnomics")
  proto <- cache$FacetIdeo

  ideo1 <- ggnomics:::render.ideo("chr1", "x", list(x.range = c(5300000L, 190800000L)),
                                  theme_get(), NA, proto$ideograms)
  ideo2 <- proto$render.ideo("chr1", "x", list(x.range = c(5300000L, 190800000L)),
                            theme_get(), NA, proto$ideograms)

  expect_is(ideo1, "grob")
  expect_is(ideo2, "grob")

  ideo1 <- ideo1$children
  ideo2 <- ideo2$children

  # Should be equivalent expect for name
  expect_equivalent(ideo1[[1]][-5], ideo2[[1]][-5])
  expect_equivalent(ideo1[[2]][-8], ideo2[[2]][-8])
  expect_equivalent(ideo1[[3]][-5], ideo2[[3]][-5])

  # Test reverse works
  ideo3 <- proto$render.ideo("chr1", "x", list(x.range = c(190800000L, 5300000L)),
                             theme_get(), NA, proto$ideograms)
  ideo3 <- ideo3$children
  expect_equivalent(as.numeric(ideo1[[2]]$x),
                    1 - as.numeric(ideo3[[2]]$x))

})

test_that("FacetIdeo can render ideograms along y", {
  cache <- getFromNamespace("tbcache", "ggnomics")
  proto <- cache$FacetIdeo

  ideo1 <- ggnomics:::render.ideo("chr1", "y", list(y.range = c(5300000L, 190800000L)),
                                  theme_get(), NA, proto$ideograms)
  ideo2 <- proto$render.ideo("chr1", "y", list(y.range = c(5300000L, 190800000L)),
                             theme_get(), NA, proto$ideograms)

  expect_is(ideo1, "grob")
  expect_is(ideo2, "grob")

  ideo1 <- ideo1$children
  ideo2 <- ideo2$children

  # Should be equivalent expect for name
  expect_equivalent(ideo1[[1]][-5], ideo2[[1]][-5])
  expect_equivalent(ideo1[[2]][-8], ideo2[[2]][-8])
  expect_equivalent(ideo1[[3]][-5], ideo2[[3]][-5])

  # Test reverse works
  ideo3 <- proto$render.ideo("chr1", "y", list(y.range = c(190800000L, 5300000L)),
                             theme_get(), NA, proto$ideograms)
  ideo3 <- ideo3$children
  expect_equivalent(as.numeric(ideo1[[2]]$x),
                    1 - as.numeric(ideo3[[2]]$x))
})

test_that("FacetIdeo can indicate range", {
  rawdat <- example_cytobands()
  cache <- getFromNamespace("tbcache", "ggnomics")
  proto <- cache$FacetIdeo
  xrange <- list(x.range = c(5300000L, 190800000L))

  ctrl <- proto$render.ideo("chr1", "x", xrange,
                             theme_get(), NA, proto$ideograms)
  test <- proto$render.ideo("chr1", "x", xrange,
                             theme_get(), "dodgerblue", proto$ideograms)
  ctrl <- ctrl$children
  test <- test$children

  expect_equal(length(ctrl), 3)
  expect_equal(length(test), 5)

  # X positions should match
  expect_equal(ctrl[[1]]$x, test[[2]]$x)
  expect_equal(ctrl[[2]]$x, test[[3]]$x)
  expect_equal(ctrl[[3]]$x, test[[4]]$x)

  # Y positions should not match
  expect_false(any(as.numeric(ctrl[[1]]$y) == as.numeric(test[[2]]$y)))
  expect_true(all(as.numeric(ctrl[[2]]$height) > as.numeric(test[[3]]$height)))
  expect_false(any(as.numeric(ctrl[[3]]$y) == as.numeric(test[[4]]$y)))

  # Test colour is inherited
  expect_equal(test[[1]]$gp$fill, "dodgerblue")
  expect_equal(test[[5]]$gp$col,  "dodgerblue")

  # Test region is appropriate
  range <- as.numeric(test[[1]]$x) + c(-0.5, 0.5) * as.numeric(test[[1]]$width)
  max <- max(rawdat[rawdat$chrom == "chr1", "chromEnd"])
  range <- range * max
  expect_equal(range, xrange[[1]])

  # Test bands are appropriate
  xpos <- as.numeric(test[[3]]$x)
  width <- as.numeric(test[[3]]$width)
  start <- (xpos - 0.5 * width) * max
  end <- (xpos + 0.5 * width) * max
  rawdat <- rawdat[rawdat$gieStain != "acen" & rawdat$chrom == "chr1",]
  expect_equal(rawdat$chromStart, start)
  expect_equal(rawdat$chromEnd, end)
})

test_that("setup_ideo can handle missing centromeres", {
  cytobands <- example_cytobands()
  cytobands <- cytobands[cytobands$gieStain != "acen",]
  setup_cytobands(cytobands, example_cytoband_colours())
  
  cache <- getFromNamespace("tbcache", "ggnomics")
  proto <- cache$FacetIdeo
  
  nrows <- sapply(proto$ideograms, function(ideo) {
    sapply(ideo, nrow)
  })
  
  expected <- matrix(c(4,4,61,60), nrow = 2, byrow = TRUE)
  expect_equivalent(nrows, expected)
})
