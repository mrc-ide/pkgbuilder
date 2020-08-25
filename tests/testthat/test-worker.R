context("worker")


test_that("worker build submits jobs via callr::r", {
  workdir <- tempfile()
  version <- "4.0"
  dir_create(workdir)
  dir_create(path_log(workdir, version, ""))
  dir_create(path_binary(workdir, version, ""))

  path <- file.path(workdir, "pkg_0.1.2.tar.gz")
  bin <- as.raw(0:255)
  writeBin(bin, path)

  id <- ids::random_id()
  data <- list(ref = "user/pkg", extra_dependencies = NULL, workdir = workdir)

  mock_callr_r <- mockery::mock(path)
  res <- with_mock(
    "callr::r" = mock_callr_r,
    worker_build(version, id, data))
  expect_true(res)

  dest <- file.path(path_binary(workdir, version, id), basename(path))

  expect_true(file.exists(path_binary(workdir, version, id)))
  expect_true(file.exists(dest))
  expect_equal(readBin(dest, raw(), file.size(dest)), as.raw(0:255))
  expect_false(file.exists(path))

  mockery::expect_called(mock_callr_r, 1)
  args <- mockery::mock_args(mock_callr_r)[[1]]
  expect_is(args[[1]], "function")
  expect_equal(args$args, data)
  expect_equal(args$stdout, path_log(workdir, version, id))
  expect_equal(args$stderr, path_log(workdir, version, id))
})


test_that("worker fail gets reported", {
  workdir <- tempfile()
  version <- "4.0"
  dir_create(workdir)
  dir_create(path_log(workdir, version, ""))
  dir_create(path_binary(workdir, version, ""))

  id <- ids::random_id()
  data <- list(ref = "user/pkg", extra_dependencies = NULL, workdir = workdir)

  mock_callr_r <- mockery::mock(stop("some build error"))
  res <- with_mock(
    "callr::r" = mock_callr_r,
    worker_build(version, id, data))
  expect_false(res)

  mockery::expect_called(mock_callr_r, 1)
  args <- mockery::mock_args(mock_callr_r)[[1]]
  expect_is(args[[1]], "function")
  expect_equal(args$args, data)
  expect_equal(args$stdout, path_log(workdir, version, id))
  expect_equal(args$stderr, path_log(workdir, version, id))
})


test_that("additional dependencies are passed through", {
  workdir <- tempfile()
  version <- "4.0"
  dir_create(workdir)
  dir_create(path_log(workdir, version, ""))
  dir_create(path_binary(workdir, version, ""))

  path <- file.path(workdir, "pkg_0.1.2.tar.gz")
  file.create(path)

  id <- ids::random_id()
  data <- list(ref = "user/pkg",
               extra_dependencies = c("a/b", "c/d"),
               workdir = workdir)

  mock_callr_r <- mockery::mock(path)
  res <- with_mock(
    "callr::r" = mock_callr_r,
    worker_build(version, id, data))
  expect_true(res)

  mockery::expect_called(mock_callr_r, 1)
  args <- mockery::mock_args(mock_callr_r)[[1]]
  expect_equal(args$args, data)
})


test_that("report back to queue on failure", {
  mock_worker_build <- mockery::mock(FALSE)

  workdir <- tempfile()
  version <- "4.0"
  q <- queue$new(version, workdir)
  id <- q$submit(version, "user/repo", NULL)

  lq <- liteq::ensure_queue(version, path_queue(workdir))

  with_mock(
    "pkgbuilder:::worker_build" = mock_worker_build,
    worker_poll(lq, version))

  expect_equal(q$list(version)$status, "FAILED")
  expect_equal(q$status(version, id), list(status = "FAILED", log = NULL))
  expect_null(q$result(version, id))

  mockery::expect_called(mock_worker_build, 1)
  expect_equal(mockery::mock_args(mock_worker_build)[[1]],
               list(version, id, list(ref = "user/repo",
                                      extra_dependencies = NULL,
                                      workdir = workdir)))
})


test_that("remove from queue on success", {
  workdir <- tempfile()
  version <- "4.0"
  q <- queue$new(version, workdir)
  id <- q$submit(version, "user/repo", NULL)

  path_result <- file.path(path_binary(workdir, version, id), "repo.tgz")
  mock_worker_build <- mockery::mock({
    dir_create(dirname(path_result))
    file.create(path_result)
    TRUE
  })

  lq <- liteq::ensure_queue(version, path_queue(workdir))

  with_mock(
    "pkgbuilder:::worker_build" = mock_worker_build,
    worker_poll(lq, version))

  expect_equal(q$list(version)$status, character(0))
  expect_equal(q$status(version, id), list(status = "COMPLETE", log = NULL))

  expect_equal(q$result(version, id), path_result)

  mockery::expect_called(mock_worker_build, 1)
  expect_equal(mockery::mock_args(mock_worker_build)[[1]],
               list(version, id, list(ref = "user/repo",
                                      extra_dependencies = NULL,
                                      workdir = workdir)))
})


test_that("construct worker", {
  workdir <- tempfile()
  version <- "4.0"
  q <- queue$new(c("3.6", "4.0"), workdir)

  mock_worker_poll <- mockery::mock()
  with_mock("pkgbuilder:::worker_poll" = mock_worker_poll, {
    poll <- pb_worker(version, workdir)
    expect_is(poll, "function")
    poll()
    mockery::expect_called(mock_worker_poll, 1)
    args <- mockery::mock_args(mock_worker_poll)[[1]]
    expect_s3_class(args[[1]], "liteq_queue")
    expect_equal(args[[2]], version)
  })
})
