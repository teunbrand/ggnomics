# Packing / unpacking -----------------------------------------------------

test_that("GreekSoldiers packs appropriately", {
  cases <- list(
    1:10,
    Rle(1:5),
    IntegerList(1, 2),
    GRanges("chr1:100-200"),
    DataFrame(x = 1:5, y = 2:6)
  )
  
  test <- lapply(cases, GreekSoldier)
  classes <- lapply(test, function(x) head(class(x), 1))
  expect_identical(classes, list("integer", "BeechHorse", 
                                 "BeechHorse", "OakHorse", "BeechHorse"))
  
  implicit <- lapply(test, HelenOfTroy)
  expect_equivalent(implicit, list("integer", "Rle", "CompressedIntegerList", 
                                   "GRanges", "DFrame"))
  
  restored <- lapply(test, Nightfall)
  expect_identical(cases, restored)
})

# Quality of life ---------------------------------------------------------

test_that("WoodenHorse is printed prettily", {
  x <- GreekSoldier(Rle(1:3, 3:1))
  
  expect_output(obj_print_header(x), "<WoodenHorse: Rle\\[6\\]>")
  expect_equal(format(x), c(1, 1, 1, 2, 2, 3))
  expect_equal(vec_ptype_full(x), "WoodenHorse")
  expect_equal(vec_ptype_abbr(x), "WHrse")
})

# Concatenation, subsetting -----------------------------------------------

test_that("WoodenHorse casts correctly", {
  x <- GreekSoldier(Rle(1:3))
  y <- GreekSoldier(IRanges(1:3, width = 3:1))
  z <- GreekSoldier(GRanges(c("chr1:100-200", "chr2:200-300")))
  w <- GreekSoldier(DataFrame(x = 1:5, y = 2:6))
  
  expect_error(c(x, 1), 
               class = "vctrs_error_incompatible_type")
  expect_s3_class(c(x, x), "WoodenHorse")
  expect_s3_class(c(x, x), "BeechHorse")
  expect_s3_class(c(z, z), "OakHorse")
  expect_s3_class(c(w, w), "BeechHorse")
  
  expect_identical(Nightfall(c(x, x)), c(Rle(1:3), Rle(1:3)))
  expect_identical(Nightfall(c(z, z)), 
                   c(GRanges(c("chr1:100-200", "chr2:200-300")),
                     GRanges(c("chr1:100-200", "chr2:200-300"))))
  expect_identical(Nightfall(c(w, w)),
                   DataFrame(x = c(1:5, 1:5),
                             y = c(2:6, 2:6)))
  
  w <- GreekSoldier(GRanges(c("chr1:300-400", "chr2:50-100")))
  expect_identical(Nightfall(c(z, w)), 
                   c(GRanges(c("chr1:100-200", "chr2:200-300")),
                     GRanges(c("chr1:300-400", "chr2:50-100"))))
})

test_that("WoodenHorse is subsetted appropriately", {
  ctrl <- Rle(1:4)
  ctrl2 <- GRanges("chr1", IRanges(1:4, width = 5))
  ctrl3 <- DataFrame(x = 1:5, y = 2:6)
  test <- GreekSoldier(ctrl)
  test2 <- GreekSoldier(ctrl2)
  test3 <- GreekSoldier(ctrl3)
  
  expect_identical(ctrl[2:3], Nightfall(test[2:3]))
  expect_identical(ctrl2[2:3], Nightfall(test2[2:3]))
  expect_identical(ctrl3[2:3,], Nightfall(test3[2:3]))
})

test_that("WoodenHorse double bracket subsetting works", {
  ctrl <- IRanges::RleList(1:6, 10:15)
  test <- GreekSoldier(ctrl)
  expect_identical(Nightfall(test[[1]]), ctrl[[1]])
  expect_identical(Nightfall(test[[2]]), ctrl[[2]])
  
  ctrl <- GRanges(c("chr1:100-200", "chr2:100-200"))
  ctrl <- GRangesList(ctrl[1], ctrl[2])
  test <- GreekSoldier(ctrl)
  expect_identical(Nightfall(test[[1]]), ctrl[[1]])
  expect_identical(Nightfall(test[[2]]), ctrl[[2]])
})

test_that("WoodenHorse single bracket subassignment works", {
  ctrl <- Rle(1:5, 1)
  test <- GreekSoldier(ctrl)
  val <- Rle(10:11)
  ctrl[3:4] <- val
  test[3:4] <- val
  expect_identical(Nightfall(test), ctrl)
  test[3:4] <- GreekSoldier(val)
  expect_identical(Nightfall(test), ctrl)
  
  ctrl <- DataFrame(x = 1:5, y = 2:6)
  test <- GreekSoldier(ctrl)
  val <- DataFrame(x = 10:11, y = 12:13)
  ctrl[3:4,] <- val
  test[3:4] <- val
  expect_identical(Nightfall(test), ctrl)
  test[3:4] <- GreekSoldier(val)
  expect_identical(Nightfall(test), ctrl)
  
  ctrl <- GRanges("chr1", IRanges(1:5, width = 10))
  test <- GreekSoldier(ctrl)
  val <- GRanges("chr1", IRanges(20:21, width = 2))
  ctrl[3:4] <- val
  test[3:4] <- val
  expect_identical(Nightfall(test), ctrl)
  test[3:4] <- GreekSoldier(val)
  expect_identical(Nightfall(test), ctrl)
})

test_that("WoodenHorse double bracket subassignment works", {
  ctrl <- IRanges::RleList(1:6, 10:15)
  test <- GreekSoldier(ctrl)
  val <- Rle(1:3, 3:1)
  ctrl[[2]] <- val
  test[[2]] <- val
  expect_identical(Nightfall(test), ctrl)
  test[[2]] <- GreekSoldier(val)
  expect_identical(Nightfall(test), ctrl)
})

# NA handling -------------------------------------------------------------

test_that("GreekSoldier preserves NAs", {
  x <- GreekSoldier(Rle(c(1, NA, 2)))
  expect_identical(is.na(x), c(FALSE, TRUE, FALSE))
  expect_identical(vec_data(x), c(0, NA_real_, 0))
})

test_that("setNA sets NAs", {
  x <- GreekSoldier(Rle(c(1:3)))
  x <- setNA(x, c(FALSE, TRUE, FALSE))
  y <- setNA(1:3, c(FALSE, TRUE, FALSE))
  expect_identical(is.na(x), c(FALSE, TRUE, FALSE))
  expect_identical(is.na(y), c(FALSE, TRUE, FALSE))
  
  x <- GreekSoldier(GRanges("chr1", IRanges(1:3, width = 2)))
  x <- setNA(x, c(FALSE, TRUE, FALSE))
  expect_identical(is.na(x), c(FALSE, TRUE, FALSE))
})

test_that("WoodenHorse NAs are preserved after concatenation", {
  a <- GreekSoldier(Rle(1:2))
  b <- GreekSoldier(Rle(3:4))
  a <- setNA(a, c(TRUE, FALSE))
  b <- setNA(b, c(FALSE, TRUE))
  ab <- c(a, b)
  expect_identical(is.na(ab), c(TRUE, FALSE, FALSE, TRUE))
  
  a <- GreekSoldier(GRanges("chr1", IRanges(c(2, 4), width = 5)))
  b <- GreekSoldier(GRanges("chr1", IRanges(c(100, 104), width = 10)))
  a <- setNA(a, c(TRUE, FALSE))
  b <- setNA(b, c(FALSE, TRUE))
  ab <- c(a, b)
  expect_identical(is.na(ab), c(TRUE, FALSE, FALSE, TRUE))
})

test_that("WoodenHorse NAs are preserved after subsetting", {
  ab <- GreekSoldier(Rle(1:4))
  ab <- setNA(ab, c(TRUE, FALSE, FALSE, TRUE))
  a <- ab[1:2]
  b <- ab[3:4]
  expect_identical(is.na(a), c(TRUE, FALSE))
  expect_identical(is.na(b), c(FALSE, TRUE))
  
  ab <- GreekSoldier(GRanges("chr1", IRanges(c(1, 10, 100, 1000), width = 20)))
  ab <- setNA(ab, c(TRUE, FALSE, FALSE, TRUE))
  a <- ab[1:2]
  b <- ab[3:4]
  expect_identical(is.na(a), c(TRUE, FALSE))
  expect_identical(is.na(b), c(FALSE, TRUE))
})


# (In)finites -------------------------------------------------------------

test_that("WoodenHorse reports infinites when possible", {
  a <- GreekSoldier(Rle(c(1, NA, Inf)))
  # IRanges can never be NA or infinite
  b <- GreekSoldier(IRanges(1:3, width = 3:1))
  b <- setNA(b, c(FALSE, TRUE, FALSE))
  
  expect_equal(is.na(a), c(FALSE, TRUE, FALSE))
  expect_equal(is.finite(a), c(TRUE, FALSE, FALSE))
  # Rle doesn't have an is.infinite method
  expect_equal(is.infinite(a), c(FALSE, FALSE, FALSE))
  
  expect_equal(is.na(b), c(FALSE, TRUE, FALSE))
  expect_equal(is.finite(b), c(TRUE, FALSE, TRUE))
  expect_equal(is.infinite(b), c(FALSE, FALSE, FALSE))
})
