# Atomic ------------------------------------------------------------------

# For regular vectors should just yield the same as the standard formatter

test_that("S4LabelFormat handles atomic types", {
  fun <- scales::identity_trans()$format
  
  test <- list(c(1L, 2L), c(2.5, 4), c(TRUE, FALSE), NA, Inf, -Inf, NaN,
               LETTERS[c(3, 8)], numeric(0), character(0),  NULL, 
               complex(real = 1:3, imaginary = 3:1))
  
  ctrl <- lapply(test, fun)
  test <- lapply(test, S4LabelFormat)
  
  expect_equal(ctrl, test)
  
  # Test returns names if input is named
  test <- c("A" = 1, "B" = 2)
  expect_equal(S4LabelFormat(test), c("A", "B"))
})

test_that("S4LabelFormat forwards WoodenHorses", {
  x <- GreekSoldier(Rle(1:5))
  
  expect_equal(S4LabelFormat(x), as.character(1:5))
})

# Genomic -----------------------------------------------------------------

test_that("S4LabelFormat does major and minor genomic labels", {
  x <- list(GRanges(c("chr1:100", "chr2:300")),
            GPos(c("chr1:100", "chr2:300")))
  x[[3]] <- GRangesFactor(x[[1]])
  
  test <- lapply(x, S4LabelFormat, type = "major")
  
  expect_equal(test, list(c("chr1", "chr2"), c("chr1", "chr2"), 
                          c("chr1", "chr2")))
  
  test <- lapply(x, S4LabelFormat, type = "minor")
  
  expect_equal(test, list(c("100", "300"), c("100", "300"), c("100", "300")))
})

test_that("S4LableFormat does 0-length genomic classes", {
  x <- list(GRanges(), GPos(), GRangesFactor())
  
  test1 <- lapply(x, S4LabelFormat, type = "major")
  test2 <- lapply(x, S4LabelFormat, type = "minor")
  
  expect_equal(test1, test2)
  expect_equal(test1, list(character(0), character(0), character(0)))
})


# Basepairs ---------------------------------------------------------------

test_that("label_basepairs gives appropriate labels", {
  fun <- label_basepair()
  
  test <- 10^c(0:12)
  test <- fun(test)
  
  expect_equal(test, c(1, 10, 100, "1 kb", "10 kb", "100 kb", "1 Mb", "10 Mb",
                       "100 Mb", "1 Gb", "10 Gb", "100 Gb", "1 Tb"))
})

test_that("label_basepairs can use different units", {
  fun <- label_basepair(unit = "bp")
  
  test <- 10^seq(0, 12, by =2)
  test <- fun(test)
  
  expect_equal(test, c(1, 100, "10 kbp", "1 Mbp", "100 Mbp", "10 Gbp", "1 Tbp"))
})

test_that("label_basepairs can label small numbers", {
  fun <- label_basepair(labelsmall = TRUE)
  
  test <- c(1, 10, 100, 1000)
  test <- fun(test)
  
  expect_equal(test, c("1 b", "10 b", "100 b", "1 kb"))
})

test_that("label_basepairs can label GenomicRanges", {
  fun <- label_basepair()
  
  test <- list(GRanges(c("chr1:100", "chr1:200")),
               GPos(c("chr1:100", "chr1:200")))
  test <- lapply(test, fun)
  
  expect_equal(test, list(c("100", "200"), c("100", "200")))
})

test_that("label_basepairs handles weird inputs", {
  fun <- label_basepair()
  
  test <- list(c(1, NA), NULL, Inf, numeric(0))
  test <- lapply(test, fun)
  
  expect_equal(test, list(c("1", NA), character(0), "Inf", character(0)))
  
  # missing case
  expect_equal(fun(), character(0))
})

test_that("label_basepairs unpacks WoodenHorses", {
  fun <- label_basepair()

  x <- GreekSoldier(GRanges("chr1:1000"))
  
  expect_equal(fun(x), "1 kb")
})