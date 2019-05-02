context("Check summary measures")

test_that("check mean", {
  expect_equal(aux_mean(10,0.3), 3)
  expect_equal(aux_mean(100,0.3), 30)
  expect_is(aux_mean(10,0.3), "numeric")
})

test_that("check variance", {
  expect_equal(aux_variance(10,0.3), 2.1)
  expect_equal(aux_variance(100,0.3), 21)
  expect_is(aux_variance(10,0.3), "numeric")
})

test_that("check mode", {
  expect_equal(aux_mode(10,0.3), 3)
  expect_equal(aux_mode(100,0.3), 30)
  expect_is(aux_mode(10,0.3), "numeric")
})

test_that("check skewness", {
  expect_equal(aux_skewness(10,0.5), 0)
  expect_equal(aux_skewness(100,0.3), 0.08728716)
  expect_is(aux_skewness(10,0.3), "numeric")
})

test_that("check kurtosis", {
  expect_equal(aux_kurtosis(10,0.5), -0.2)
  expect_equal(aux_kurtosis(100,0.3), -0.01238095)
  expect_is(aux_kurtosis(10,0.3), "numeric")
})
