context("test-to_poly")

gr <- GenomicRanges::GRanges(
  "chr1",
  IRanges::IRanges(
    c(10, 20, 30),
    c(15, 50, 40)
  )
)

cov <- GenomicRanges::coverage(gr)

file <- tempfile(fileext = ".bw")
rtracklayer::export.bw(cov, file)
file <- rtracklayer::BigWigFile(file)

base <- structure(list(lengths = cov[[1]]@lengths,
                       values  = cov[[1]]@values), class = "rle")

expected <- data.frame(
  x = c(1, 9, 10, 15, 16, 19, 20, 29, 30, 40, 41, 50, 50),
  y = c(0, 0, 1, 1, 0, 0, 1, 1, 2, 2, 1, 1, 0)
)

test_that("to_poly correctly converts base rle to polygon", {
  out <- to_poly(base)
  expect_equivalent(out, expected)
})

test_that("to_poly correctly converts Rle to polygon", {
  out <- to_poly(cov[[1]])
  expect_equivalent(out, expected)
})

test_that("to_poly correctly converts RleList to polygon", {
  # Without ROI
  out1 <- to_poly(cov)

  # With ROI
  out2 <- to_poly(cov, GenomicRanges::GRanges("chr1", IRanges::IRanges(1, 50)))

  expect_identical(out1, out2)
  expect_equivalent(out1[,-3], expected)
})

test_that("to_poly correctly converts BigWigFile to polygon", {
  out <- to_poly(file, GenomicRanges::GRanges("chr1", IRanges::IRanges(1, 50)))
  expect_equivalent(out[,-3], expected)
})
