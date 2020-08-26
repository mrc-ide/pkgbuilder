context("utils")

test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("forever loops", {
  callback <- mockery::mock(NULL, NULL, stop("exit"))
  expect_error(forever(callback), "exit")
  mockery::expect_called(callback, 3)
  expect_equal(mockery::mock_args(callback),
               list(list(), list(), list()))
})
