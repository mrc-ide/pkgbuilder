context("integration: queue")

test_that("run a job in a queue", {
  workdir <- tempfile()
  version <- r_version2()

  q <- queue$new(version, workdir)
  id <- q$submit(version, "reside-ic/defer", NULL)

  w <- worker_create(workdir, NULL)
  w()

  expect_equal(
    q$status(version, id),
    list(status = "COMPLETE", log = NULL))

  res <- q$status(version, id, TRUE)
  expect_equal(res$status, "COMPLETE")
  expect_is(res$log, "character")
  expect_true(length(res$log) > 10)

  res <- q$result(version, id)
  expect_true(file.exists(res))
  expect_match(basename(res),
               "defer_[0-9]+\\.[0-9]+\\.[0-9]+\\.(tgz|zip|tar.gz)$")
})
