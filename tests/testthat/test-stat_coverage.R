test_that("stat_coverage computes numeric coverage", {
    df <- data.frame(x = c(1, 3, 6), xend = c(4, 7, 9))
    g <- ggplot(df) +
        stat_coverage(aes(x = x, xend = xend))
    ld <- layer_data(g)
    expect_equal(ld$y, c(0, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 0))
    expect_equal(ld$x, c(1, 1, 3, 3, 4, 4, 6, 6, 7, 7, 9, 9))
})

test_that("stat_coverage can flip aes", {
    df <- data.frame(x = c(1, 3, 6), xend = c(4, 7, 9))
    g <- ggplot(df) +
        stat_coverage(aes(y = x, yend = xend))
    ld <- layer_data(g)
    expect_equal(ld$x, c(0, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 0))
    expect_equal(ld$y, c(1, 1, 3, 3, 4, 4, 6, 6, 7, 7, 9, 9))
})

test_that("stat_coverage can have IRanges", {
    df <- DataFrame(x = IRanges(c(20,30), c(60,70)))
    g <- ggplot(df) +
        stat_coverage(aes(x = x))
    ld <- layer_data(g)
    expect_equal(ld$y, c(0, 1, 1, 2, 2, 1, 1, 0))
    expect_equal(pos(Nightfall(ld$x)),
                 c(19, 20, 29, 30, 60, 61, 70, 71))
})

test_that("stat_coverage can have GRanges", {
    df <- DataFrame(x = GRanges(c("chr1:10-20", "chr1:14-26",
                                  "chr2:5-10")))
    g <- ggplot(df, aes(x)) +
        stat_coverage()
    ld <- layer_data(g)
    expect_equal(ld$y, c(0, 1, 1, 2, 2, 1, 1, 0, 0, 1, 1, 0))
    expect_equal(pos(Nightfall(ld$x)),
                 c(9, 10, 13, 14, 20, 21, 26, 27, 4, 5, 10, 11))
})
