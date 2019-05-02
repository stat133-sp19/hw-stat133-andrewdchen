context("Check binomial")

test_that("check choose", {
  expect_equal(bin_choose(7,4), 35)
  expect_error(bin_choose(4,7))
  expect_is(bin_choose(7,4), "numeric")
})

test_that("check probability", {
  expect_equal(bin_probability(2,5,0.5), 0.3125)
  expect_equal(bin_probability(55,100,0.45), 0.01075277)
  expect_is(bin_probability(2,5,0.5), "numeric")
})

test_that("check distribution", {
  expect_error(bin_distribution(-1, 0.5))
  expect_error(bin_distribution(100, 2))
  expect_is(bin_distribution(10, 0.3), "bindis")
})

test_that("check cumulative", {
  expect_error(bin_cumulative(-1, 0.5))
  expect_error(bin_cumulative(100, 2))
  expect_is(bin_cumulative(10,0.3), "bincum")
})
