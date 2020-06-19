test_that("ggproto sibling works as intended", {
  # These share an aesthetics name, but object is different
  a <- ggplot2::Scale
  b <- ggplot2::Geom
  expect_false(identical(a$aesthetics, b$aesthetics))
  
  test1 <- ggproto_sibling("test1", a, b, 
                           greet = function()(print("Hello world!")))
  test2 <- ggproto_sibling("test2", b, a,
                           attack = function()(print("Leeeroy Jenkins!")))
  
  # Expect classes are correct
  expect_is(test1, "Scale")
  expect_is(test1, "test1")
  expect_false(inherits(test1, "Geom"))
  expect_is(test2, "Geom")
  expect_is(test2, "test2")
  expect_false(inherits(test2, "Scale"))
  
  # Expect functions are added as per usual
  expect_output(test1$greet(), "Hello world")
  expect_output(test2$attack(), "Jenkins")
  
  # Test1 should have inherited aesthetics from Scale
  expect_equal(a$aesthetics, test1$aesthetics)
  # Test2 should have inherited aesthetics form Geom
  expect_equal(b$aesthetics, test2$aesthetics)
  expect_false(identical(test1$aesthetics, test2$aesthetics))
})

test_that("ggproto_sibling works with either only parent or only sibling", {
  a <- ggplot2::Scale
  
  test1 <- ggproto_sibling("test1", a, NULL, 
                           greet = function()(print("Hello world!")))
  test2 <- ggproto_sibling("test2", NULL, a,
                           attack = function()(print("Leeeroy Jenkins!")))
  # Negative control
  test3 <- ggproto_sibling("test3", NULL, NULL)
  
  # Basic assumptions
  expect_true(inherits(test1, "Scale"))
  expect_false(inherits(test2, "Scale"))
  expect_false(inherits(test3, "Scale"))
  expect_is(test1, "ggproto")
  expect_is(test2, "ggproto")
  expect_is(test3, "ggproto")
  
  # Has parent so should have parent environment listed in super()
  expect_true("super" %in% names(test1))
  expect_identical(names(test1$super()), names(a))
  # Has no parent so should not have parent environment
  expect_false("super" %in% names(test2)) 
  # Negative control should have no objects
  expect_identical(names(test3), character(0))
  
  # Test2 should have copied methods verbatim, while test1 inherits them
  expect_gt(length(names(test2)), length(names(test1)))
  # Should have copied most except "call"
  expect_true(mean(names(a) %in% names(test2)) > 0.95)
})

test_that("ggproto_sibling signals errors", {
  # When it has no named members
  test1 <- substitute(
    ggproto_sibling("a", Scale, Geom, function(x){print("Centimorgans")})
  )
  
  test2 <- substitute(
    ggproto_sibling("b", "Luke, I am your father!", Geom)
  )
  
  expect_error(eval(test1), "Members must have names")
  expect_error(eval(test2), "Must provide valid parent")
})
