context("queue")

test_that("queue can be submitted to", {
  workdir <- tempfile()
  version <- "4.0"
  q <- queue$new(version, workdir)
  expect_is(q, "queue")

  id <- q$submit(version, "user/repo", NULL)
  expect_is(id, "character")
  expect_match(id, "^[[:xdigit:]]{32}$")

  contents <- q$list(version)
  expect_equal(contents$id, id)
  expect_equal(contents$status, "READY")

  expect_equal(q$status(version, id),
               list(status = "READY", log = NULL))
  expect_equal(q$status(version, id, TRUE),
               list(status = "READY", log = NULL))

  writeLines(letters, path_log(workdir, version, id))
  expect_equal(q$status(version, id, FALSE),
               list(status = "READY", log = NULL))
  expect_equal(q$status(version, id, TRUE),
               list(status = "READY", log = letters))
})


test_that("missing tasks get sensible status", {
  workdir <- tempfile()
  version <- "4.0"

  q <- queue$new(version, workdir)
  expect_is(q, "queue")

  id <- ids::random_id()

  expect_equal(q$status(version, id),
               list(status = "UNKNOWN", log = NULL))
  expect_equal(q$status(version, id, TRUE),
               list(status = "UNKNOWN", log = NULL))
  writeLines(letters, path_log(workdir, version, id))
  expect_equal(q$status(version, id, FALSE),
               list(status = "UNKNOWN", log = NULL))
  expect_equal(q$status(version, id, TRUE),
               list(status = "UNKNOWN", log = NULL))
})


test_that("queue validate versions", {
  expect_silent(check_version("4.0", c("3.5", "3.6", "4.0")))
  expect_error(check_version("4.0", c("3.5", "3.6")),
               "'version' must be one of '3.5', '3.6'",
               fixed = TRUE)
})


test_that("queue returns versions", {
  workdir <- tempfile()
  expect_equal(queue$new("4.0", workdir)$versions, "4.0")
  expect_equal(queue$new(c("3.6", "4.0"), workdir)$versions, c("3.6", "4.0"))
})
