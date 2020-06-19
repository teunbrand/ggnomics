# General tests -----------------------------------------------------------

test_that("scale_type recognises GenomicRanges", {
  expect_identical(scale_type(GRanges()), "genomic")
  expect_identical(scale_type(GPos()), "genomic")
  expect_identical(scale_type(GRangesFactor()), "genomic")
})

test_that("scale_genomic can be found", {
  fun <- getFromNamespace("find_scale", "ggplot2")

  x <- fun("x", GRanges())
  expect_is(x, 'ScaleGenomic')

  x <- fun("y", GPos())
  expect_is(x, "ScaleGenomic")
})

test_that("scale_genomic can be added to a plot", {
  ctrl <- ggplot(iris)
  testx <- ctrl + scale_x_genomic()
  testy <- ctrl + scale_y_genomic()

  expect_is(testx$scales$scales[[1]], "ScaleGenomic")
  expect_is(testy$scales$scales[[1]], "ScaleGenomic")
})

test_that("scale_genomic is recognised if type is GenomicRanges", {
  ctrl <- DataFrame(x = IRanges(1:5), y = 5:1)
  test <- DataFrame(x = GRanges("chr1", IRanges(1:5)), y = 5:1)

  ctrl <- ggplot(ctrl, aes(x, y)) + geom_point()
  test <- ggplot(test, aes(x, y)) + geom_point()

  ctrl <- layer_scales(ctrl)
  test <- layer_scales(test)

  expect_equal(ctrl$y, test$y)
  expect_is(ctrl$x, "ScaleS4ContinuousPosition")
  expect_is(test$x, "ScaleGenomic")
})

# Training ----------------------------------------------------------------

test_that("scale_genomic is trained sensibly", {
  df <- DataFrame(x = GRanges(c("chr1:1-100",
                                "chr1:50-150",
                                "chr2:100-200")),
                  y = 1:3)

  g <- ggplot(df, aes(x, y)) + geom_point()

  sc <- layer_scales(g)$x

  # Limits should equal a strand-agnostic range call
  expect_equal(range(df$x),
               sc$get_limits())

  # Major breaks should be GPos with first and last positions of each seqlevel
  br <- Nightfall(sc$get_breaks())
  expect_equal(br,
               GPos(c("chr1:1", "chr1:150", "chr2:100", "chr2:200"),
                    stitch = FALSE))

  # Minor breaks should be roughly along sensible positions
  br <- Nightfall(sc$get_breaks_minor())
  expect_equal(br,
               GPos(c("chr1:50", "chr1:100", "chr1:150",
                      "chr2:100", "chr2:150", "chr2:200"),
                    stitch = FALSE))

  # Labels should be chromosomes of major breaks
  expect_equal(sc$get_labels(),
               as.character(seqnames(Nightfall(sc$get_breaks()))))
})

# Plotting ----------------------------------------------------------------

test_that("scale_genomic allows plots to be rendered", {
  df <- DataFrame(x = GRanges(c("chr1:1-100",
                                "chr1:50-150",
                                "chr2:100-200")),
                  y = 1:3)

  g <- ggplot(df, aes(xmin = x, xmax = x, ymin = y - 0.2, ymax = y + 0.2)) +
    geom_rect()

  # Inspect layer data
  ld <- layer_data(g)
  expect_identical(granges(Nightfall(ld$xmin)), df$x)
  expect_identical(granges(Nightfall(ld$xmax)), df$x)

  # Inspect grob
  gr <- layer_grob(g)[[1]]
  expect_is(gr, "rect")
  expect_length(gr$x, 3L)

  # Check can be converted to gtable with no warnings/errors
  xprs <- substitute(ggplotGrob(g))
  expect_silent(gt <- eval(xprs))
  expect_is(gt, "gtable")
})

# Argument tests ----------------------------------------------------------

test_that("Major breaks work", {
  df <- DataFrame(x = GRanges(c("chr1:1-100",
                                "chr1:50-150",
                                "chr2:100-200")),
                  y = 1:3)

  breaks <- range(df$x)
  breaks <- GenomicRanges::resize(breaks, width = 0.5 * width(breaks), fix = "center")

  # Test breaks as GRanges
  g <- ggplot(df, aes(xmin = x, xmax = x, ymin = y - 0.2, ymax = y + 0.2)) +
    geom_rect()

  test <- g + scale_x_genomic(breaks = breaks)
  sc <- layer_scales(test)

  expect_equal(GreekSoldier(breaks), sc$x$get_breaks())

  # Test breaks as function
  breaks <- function(x) {GenomicRanges::resize(x, 10)}

  test <- g + scale_x_genomic(breaks = breaks)
  sc <- layer_scales(test)

  expect_equal(Nightfall(sc$x$get_breaks()),
               GRanges(c("chr1:1-10", "chr2:100-109")))
})

test_that("Major labels work", {
  df <- DataFrame(x = GRanges(c("chr1:1-100",
                                "chr1:50-150",
                                "chr2:100-200")),
                  y = 1:3)
  g <- ggplot(df, aes(xmin = x, xmax = x, ymin = y - 0.2, ymax = y + 0.2)) +
    geom_rect()

  # Test labels as character
  test <- g + scale_x_genomic(labels = c("A", "A", "B", "B"))
  sc <- layer_scales(test)

  expect_equal(sc$x$get_labels(), c("A", "A", "B", "B"))

  # Test labels are expanded when parallel to seqnames
  ctrl <- g + scale_x_genomic(labels = LETTERS[1:3])
  test <- g + scale_x_genomic(labels = LETTERS[1:2])

  ctrl <- layer_scales(ctrl)
  test <- layer_scales(test)

  expect_error(ctrl$x$get_labels())
  expect_equal(test$x$get_labels(), c("A", "A", "B", "B"))

  # Test labels as function
  labs <- function(x){toupper(format(x))}

  test <- g + scale_x_genomic(labels = labs)
  sc <- layer_scales(test)

  expect_equal(sc$x$get_labels(), c("CHR1:1", "CHR1:150",
                                    "CHR2:100", "CHR2:200"))
})

test_that("Minor labels work", {
  df <- DataFrame(x = GRanges(c("chr1:1-100",
                                "chr1:50-150",
                                "chr2:100-200")),
                  y = 1:3)
  g <- ggplot(df, aes(xmin = x, xmax = x, ymin = y - 0.2, ymax = y + 0.2)) +
    geom_rect()

  # Test breaks as GPos
  breaks <- GPos(c("chr1:75", "chr2:150"))

  test <- g + scale_x_genomic(minor_breaks = breaks)
  sc <- layer_scales(test)

  expect_equal(sc$x$get_breaks_minor(), GreekSoldier(breaks))

  # Test breaks as GPos
  breaks <- GRanges(c("chr1:75", "chr2:150"))

  test <- g + scale_x_genomic(minor_breaks = breaks)
  sc <- layer_scales(test)

  expect_equal(sc$x$get_breaks_minor(), GreekSoldier(breaks))

  # Test breaks as function
  myfun <- function(x){GreekSoldier(GRanges(seqnames(x), IRanges(end(x))))}

  test <- g + scale_x_genomic(minor_breaks = myfun)
  sc <- layer_scales(test)

  expect_equivalent(Nightfall(sc$x$get_breaks_minor()),
               GPos(c("chr1:150", "chr2:200"), stitch = FALSE))

  # Test labels as character
  test <- g + scale_x_genomic(
    minor_labels = LETTERS[1:3],
    minor_breaks = GPos(c("chr1:50", "chr1:100", "chr2:150"))
  )
  sc <- layer_scales(test)

  expect_equal(sc$x$get_labels_minor(), LETTERS[1:3])

  # Test labels as function
  labs <- function(x) {toupper(format(x))}
  test <- g + scale_x_genomic(
    minor_labels = labs,
    minor_breaks = GPos(c("chr1:50", "chr1:100", "chr2:150"))
  )
  sc <- layer_scales(test)

  expect_equal(sc$x$get_labels_minor(), c("CHR1:50", "CHR1:100", "CHR2:150"))
})
