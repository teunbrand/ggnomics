df <- data.frame(
    x = 1:3,
    xend = 2:4,
    y = 1:3,
    yend = 2:4
)

g <- ggplot(df, aes(x, y, xend = xend, yend = yend))

test_that("geom_range can be added to a plot", {
    gr <- geom_range()
    expect_is(gr, "LayerInstance")
    expect_is(gr$geom, "GeomRange")

    test <- g + gr
    expect_is(test$layers[[1]]$geom, "GeomRange")
})

test_that("geom_range works with numeric data", {
    test <- g + geom_range()
    ld <- layer_data(test)
    expect_equal(df$x, ld$x)
    expect_equal(df$xend, ld$xend)
    expect_equal(df$y, ld$y)
    expect_equal(df$yend, ld$yend)

    gr <- layer_grob(test)[[1]]
    expect_identical(length(gr$x), 3L)
})

test_that("geom_range end aesthetics are imputed when necessary", {
    g <- ggplot(df, aes(x, y)) + geom_range()
    ld <- layer_data(g)
    expect_equal(df$x, ld$x)
    expect_equal(df$x, ld$xend - 0.8)
    expect_equal(df$y, ld$y)
    expect_equal(df$y, ld$yend - 0.8)
})

test_that("geom_range can work with either position aesthetic", {
    gx <- ggplot(df, aes(x = x)) + geom_range()
    gy <- ggplot(df, aes(y = x)) + geom_range()

    ld <- layer_data(gx)
    expect_equal(df$x, ld$x)
    expect_equal(df$x, ld$xend - 0.8)

    ld <- layer_data(gy)
    expect_equal(df$y, ld$y)
    expect_equal(df$y, ld$yend - 0.8)
})

test_that("geom_range can work with IRanges", {
    df <- DataFrame(x = IRanges(c(1,4,7), c(2,5,8)))

    g <- ggplot(df, aes(x)) + geom_range() +
        scale_x_S4_continuous(expand = c(0,0))

    ld <- layer_data(g)
    expect_equal(HelenOfTroy(ld$x), "IRanges")
    expect_equal(Nightfall(ld$x), df$x)

    gr <- layer_grob(g)[[1]]

    expect_is(gr, "rect")
    expect_equal(as.numeric(gr$x), c(0, 0.375, 0.75))
    expect_equal(as.numeric(gr$width), c(0.25, 0.25, 0.25))
})

test_that("geom_range can work with GRanges", {
    df <- DataFrame(x = GRanges(c("chr1", "chr1", "chr2"),
                                IRanges(c(10,40,70), c(20, 50, 80))))
    g <- ggplot(df, aes(x)) + geom_range()

    ld <- layer_data(g)
    expect_equal(HelenOfTroy(ld$x), "GRanges")
    expect_equal(Nightfall(ld$x), df$x)

    gr <- layer_grob(g)[[1]]
    expect_is(gr, "rect")
    expect_length(gr$x, 3L)
})
