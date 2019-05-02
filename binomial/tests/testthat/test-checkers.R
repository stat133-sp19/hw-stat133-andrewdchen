context("Check checkers")

test_that("check probabilities", {
  expect_true(check_prob(0.9))
  expect_error(check_prob(2))
  expect_error(check_prob(-2))
})

test_that("check trials", {
  expect_error(check_trials(-2))
  expect_true(check_trials(2))
  expect_true(check_trials(0))
})

test_that("check success", {
  expect_true(check_success(2,4))
  expect_error(check_success(4,2))
  expect_error(check_success(-1,3))
})
