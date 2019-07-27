context("test-formats")


# format_genco ------------------------------------------------------------

test_that("format_genco formats to genomic coordinates", {
  input <- 10^(0:8)
  output <- format_genco(input)
  exp <- paste(rep(c(1,10,100), 3), rep(c("bp", "kb", "Mb"), each = 3))
  expect_equal(output, exp)
})


test_that("format_genco can be used in ggplot as label argument", {
  g <- ggplot() +
    geom_point(aes(x = c(0, 1000), y = rep(1, 2))) +
    scale_x_continuous(labels = format_genco)
  g <- ggplotGrob(g)
  axis <- g$grobs[[which(g$layout$name == "axis-b")]]
  tab <- axis$children[[which(names(axis$children) == "axis")]]
  labs <- tab$grobs[[2]]$children[[1]]$label
  expect_equal(labs, c("0 bp", "250 bp", "500 bp", "750 bp", "1 kb"))
})

# format_logtrans ---------------------------------------------------------

test_that("format_logtrans formats to powers of 10", {
  input <- 10^(0:8)
  output <- format_logtrans(input)

  expect_is(output, "list")
  expect_is(output[[1]], "call")

  parsed <- sapply(output, deparse)
  exp <- paste0("10^`", 0:8, "`")

  expect_equal(parsed, exp)
})

test_that("format_logtrans can be used in ggplot as label argument", {
  g <- ggplot() +
    geom_point(aes(x = c(1, 1e6), y = rep(1, 2))) +
    scale_x_continuous(trans = "log10", labels = format_logtrans)
  g <- ggplotGrob(g)
  axis <- g$grobs[[which(g$layout$name == "axis-b")]]
  tab <- axis$children[[which(names(axis$children) == "axis")]]
  labs <- tab$grobs[[2]]$children[[1]]$label
  expect_equal(labs, expression(10^`1`, 10^`3`, 10^`5`))
})
